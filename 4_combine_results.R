# 4_combine_results.R
# combine the results on lyra from 1_find_auc.R
# Feb 2023
library(dplyr)
library(stringr)

# get list of files to combine
here = getwd()
setwd("//hpc-fs/barnetta/auc/processed") # move to lyra where processed files are
to_combine = dir()
# check that all files exist - see 4_check_raw.R

# loop through files (takes a while)
results = excluded = abstracts = NULL
for (file in to_combine){
  load(file)
  #
  file_num = str_remove_all(file, 'pubmed\\.|\\.RData')
  abstract.data = mutate(abstract.data, file = file_num) # add file number, useful for checking odd results
  # date fix
  if(class(abstract.data$date) == 'numeric'){
    abstract.data = mutate(abstract.data,
                           date = as.Date(date, origin='1970-01-01'))
  }
  # date fix
  if(is.null(aucs) == FALSE){
    if(class(aucs$date) == 'numeric'){
      aucs = mutate(aucs,
                    date = as.Date(date, origin='1970-01-01'))
    }
  }
  
  ## create frame of exclusions
  # PK exclusions in second round
  pk.excluded = filter(abstract.data, exclude==TRUE) %>%
    summarise(n = n()) %>%
    pull(n)
  # exclusions from first round
  e1 = data.frame(reason = 'Not English', n = numbers$start - numbers$post.non.english)
  e2 = data.frame(reason = 'Empty abstract', n = numbers$post.non.english - numbers$post.empty)
  e3 = data.frame(reason = 'PK', n = (numbers$post.empty - numbers$post.non.pk) + pk.excluded) # exclusions at both stages
  e4 = data.frame(reason = 'Meta-analysis', n = numbers$post.non.pk - numbers$post.non.meta)
  ex.frame = bind_rows(e1, e2, e3, e4) %>%
    mutate(file = file_num)
  
  # remove excluded from abstract data
  abstract.data = filter(abstract.data, exclude==FALSE) %>%
    select(-exclude, -any_auc)
  
  # redo any AUC
  are_aucs = unique(aucs$pmid)
  abstract.data = mutate(abstract.data,
                         any_auc = pmid %in% are_aucs)
  
  # concatenate
  abstracts = bind_rows(abstracts, abstract.data)
  results = bind_rows(results, aucs)
  excluded = bind_rows(excluded, ex.frame)
}
setwd(here)

# check where finds are coming from
#group_by(counts, source) %>%
#  summarise(total = sum(counts))

# tidy up result types
results = mutate(results,
                # percent = str_detect(type, 'percent'), # no longer works, does not matter
                 type = ifelse(type == 'percent', 'mean', type),
                 type = ifelse(type == 'diff - percent', 'diff', type),
                 type = ifelse(type == 'pair - percent', 'pair', type),
                 type = ifelse(type == 'mean - percent', 'mean', type),
                 type = ifelse(type == 'lower - percent', 'lower', type),
                 type = ifelse(type == 'upper - percent', 'upper', type))

# check for duplicate abstracts and remove them
dups = select(abstracts, pmid) %>%
  duplicated()
if(any(dups)==TRUE){
  duplicates = abstracts[dups,] # Mostly F1000 or similar
  table(duplicates$jabbrv)
}
# take latest result for duplicate PMIDs (takes a while)
abstracts = group_by(abstracts, pmid) %>%
  arrange(pmid, desc(date)) %>%
  slice(1) %>% # take latest
  ungroup()
# need to add dates to results so that duplicates can be removed

# arrange type
levels = c('mean','pair','lower','upper','diff')
labels = c('Mean','Pair','Lower','Upper','Difference')
results = mutate(results,
                 type = factor(type, levels=levels, labels=labels))

# save
save(results, excluded, abstracts, file = 'data/analysis_ready.RData')
