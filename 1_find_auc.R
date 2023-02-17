# 1_find_auc.R
# find and extract the AUC statistics in the pubmed abstracts
# moved to lyra
# December 2022
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(words2number) # convert words to number, e.g. 'forty' = '40'
source('99_main_function_abstract.R') # main function for extracting AUC statistics from abstracts
source('99_functions.R') # 
source('1_confidence_intervals_pattern.R')
source('1_patterns.R') # text patterns for matching


# load data for further processing.
files_to_loop = dir('raw', pattern='baseline')
files_to_loop = rev(files_to_loop) # start at more recent files
files_to_loop = sample(files_to_loop, replace=FALSE, size = length(files_to_loop)) # random order
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

    # abstracts
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

    #
    if(k%%500 == 0){cat(paste('Up to abstract', k, '\r'))} # progress bar
  }
  
  # save
  outfile = paste('processed/pubmed.', file_number, '.RData', sep='') # 
  save(abstract.data, 
       aucs, 
       numbers, # numbers excluded from first run (0_read_pubmed_baseline.R)
       excluded.abstracts, 
       file=outfile)
  
}

