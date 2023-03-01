# 0b_run_read_baseline.R
# called by 0_read_pubmed_baseline.R
# read and process the XML data
# version using data that has been manually downloaded from ftp survey (because batch process using API was creating gaps)
# February 2023
load('data/countries.RData') # for country affiliation
#source('0_my_pubmed_key_do_not_share.R') # not needed
source('1_patterns.R') # for PK studies

# import the XML file into R - takes a while
very_raw_pubmed = table_articles_byAuth_adapted(pubmed_data = file, # from 99_table_articles_byAuth_adapted.R
                                                 encoding = "UTF-8") # recommended encoding
start.N = very_raw_pubmed$N # store starting number of papers
very_raw_pubmed = very_raw_pubmed$papers.authors.df # reuse old data name

# remove non-English
post_english = filter(very_raw_pubmed, language=='eng')

# exclude empty and/or short abstracts
post_empty = filter(post_english,
                    !is.na(abstract),
                    nchar(abstract) > 100)

# exclude Pharmacokinetic studies
post_pharma = filter(post_empty,
                     !str_detect(tolower(title), 'pharmacokinetics?'),
                     !str_detect(tolower(abstract), 'pharmacokinetics?'),
                     !str_detect(mesh, mesh_exclude),
                     !str_detect(tolower(abstract), to_remove_not_auc))

# exclude meta-analysis and tutorials
meta_exclude_mesh = c('Meta.Analysis','Systematic Review')
meta_exclude_mesh = paste(meta_exclude_mesh, collapse='|')
meta_exclude_title = c('meta.?analys(i|e)s','pooled.?analys(i|e)s','tutorial')
meta_exclude_title = paste(meta_exclude_title, collapse='|')
post_meta = filter(post_pharma,
                   !str_detect(tolower(title), meta_exclude_title),
                   !str_detect(tolower(abstract), meta_exclude_title),
                   !str_detect(mesh, meta_exclude_mesh))

## further processing
raw_pubmed = mutate(post_meta, 
              pmid = as.numeric(pmid),
              # replace_non_ascii does not get the following right; instead makes it 'copyright'
              #abstract = str_replace_all(abstract, '⩽' ,' less than or equal to '),  # removed Jan 2023
              #abstract = str_replace_all(abstract, '⩾' ,' greater than or equal to '),
              abstract = str_replace_all(abstract, 'Ω' ,' Omega '), 
              #
              country = ifelse(country =='', NA, country), # replace empty with missing
              #
              title = replace_non_ascii(title),
              title = str_remove(string=title, pattern="[:punct:]authors' transl[:punct:]|[:punct:]author's transl[:punct:]|[:punct:]authors transl[:punct:]|[:punct:]authors transl"), # remove this note
              title = str_remove(string=title, pattern='[:punct:]$'), # remove punctuation at end of title
              title = str_remove(string=title, pattern='\\] $|^\\[|\\]$') # remove square brackets around title
)

## Paper numbers throughout the data management process
numbers = data.frame(start = start.N)
numbers$post.non.english = nrow(post_english)
numbers$post.empty = nrow(post_empty)
numbers$post.non.pk = nrow(post_pharma)
numbers$post.non.meta = nrow(post_meta)

# reduce the number of variables
raw_pubmed = dplyr::select(raw_pubmed, pmid, n.authors, country, jabbrv, type, date, title, abstract, mesh) 

# keep all PMIDs as separate file, useful for finding missing abstracts
pmids = select(very_raw_pubmed, pmid) %>%
  mutate(file = number)

# save data for further processing; number from 0_read_pubmed_baseline.R
outfile = paste('raw/unprocessed.pubmed.baseline.', number, '.RData', sep='')
save(pmids, raw_pubmed, numbers, file=outfile)
