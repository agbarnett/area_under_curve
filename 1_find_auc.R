# 1_find_auc.R
# find and extract the AUC statistics in the pubmed abstracts
# moved to lyra? not yet!
# December 2022
library(stringr)
library(dplyr)
library(tidyr)
library(words2number) # convert words to number, e.g. 'forty' = '40'
source('99_main_function_abstract.R') # main function for abstracts
source('99_functions.R') # 
source('1_confidence_intervals_pattern.R')
source('1_patterns.R') # text patterns for matching
source('../narrator/98_key_not_sharing.R')

# load data for further processing.
files_to_loop = dir('raw', pattern='baseline')
number = 0
for (file in files_to_loop){  # 
  
  number = number + 1
  if(number%%10 == 0){cat(paste('Up to number', number, '\r'))} # progress bar

  ## file names
  # check if the processed file already exists
  file_number = str_remove_all(file, 'unprocessed\\.pubmed\\.baseline\\.|\\.RData')
  if(length(dir('processed', pattern = file_number) > 0)){
    next 
  }
  infile = paste('raw/', file, sep='') # from 0_read_pubmed_api.R
  load(infile)
  
  ## process the papers in a large loop
  abstract.data = excluded.abstracts = aucs = NULL # start with empty data sets
  for (k in 1:nrow(raw_pubmed)){ # loop through abstracts
    
    # exclude meta-analysis based on title or abstract
    meta1 = str_detect(raw_pubmed$title[k], 'meta.?analys(i|e)s')
    meta2 = str_detect(raw_pubmed$abstract[k], 'meta.?analys(i|e)s')
    if(meta1 == TRUE | meta2 == TRUE){
      this.exclude = data.frame(pmid=raw_pubmed$pmid[k], date=raw_pubmed$date[k], type=raw_pubmed$type[k], reason='Meta-analysis', stringsAsFactors = FALSE)
      excluded.abstracts = bind_rows(excluded.abstracts, this.exclude) 
      next # skip to next abstract
    }
    
    # abstracts
    abstract.empty = FALSE
    # don't even start if abstract is empty or is very short
    if(is.na(raw_pubmed$abstract[k]) == TRUE | raw_pubmed$abstract[k]=='' | raw_pubmed$abstract[k]==' ' | 
       tolower(raw_pubmed$abstract[k])=='n/a' | tolower(raw_pubmed$abstract[k])=='n/a.'|
       tolower(raw_pubmed$abstract[k])=='no abstract available' | tolower(raw_pubmed$abstract[k])=='no abstract available.'){abstract.empty = TRUE}
    if(abstract.empty==FALSE){
      n.words = str_count(raw_pubmed$abstract[k], ' ') # rough word count
      if(is.na(n.words) == TRUE){n.words = 0}
      if(n.words <= 10 & raw_pubmed$type[k] == 'Published Erratum'){abstract.empty = TRUE} # short abstracts with errata are usually just a citation/note
      if(n.words <= 10 & str_detect(string=raw_pubmed$abstract[k], pattern='This corrects the article')) {abstract.empty = TRUE} # alternative search for errata (in case they are listed as type 'journal article')
    }
    if(abstract.empty == TRUE){
      this.exclude = data.frame(pmid=raw_pubmed$pmid[k], date=raw_pubmed$date[k], type=raw_pubmed$type[k], reason='No abstract', stringsAsFactors = FALSE)
      excluded.abstracts = bind_rows(excluded.abstracts, this.exclude) 
    }
    if(abstract.empty == FALSE){
      aresults = process_abstract(indata = raw_pubmed, k=k) # using the main function 99_main_function_abstract.R
      # concatenate the data
      if(class(aresults) == 'data.frame'){
        abstract.data = bind_rows(abstract.data, aresults) 
      }
      if(class(aresults) != 'data.frame'){
        abstract.data = bind_rows(abstract.data, aresults$tframe) 
        aucs = bind_rows(aucs, aresults$aframe) 
      }
      remove(aresults) # tidy up
    }
    
    #
    if(k%%500 == 0){cat(paste('Up to abstract', k, '\r'))} # progress bar
  }
  
  # save
  outfile = paste('processed/pubmed.', file_number, '.RData', sep='') # 
  save(abstract.data, aucs, excluded.abstracts, file=outfile)
  
}

# create random checks - to do
#output_file = 'checks.docx'
#rmarkdown::render(input = '99_random_checks.Rmd', 
#                  output_format='word_document',
#                  output_file = output_file)
