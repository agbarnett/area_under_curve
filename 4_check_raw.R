# 4_check_raw.R
# check that `raw` files have run correctly. These are the first-pass processing of the pubmed XML data
# Jan 2023
library(dplyr)
library(stringr)

# get list of files to combine
here = getwd()
setwd("//hpc-fs/barnetta/auc/raw") # move to lyra where processed files are
to_check = dir()
to_check = to_check[!str_detect(to_check, '\\.txt')]

#
for (file in to_check){
  # check if starting numbers are 30,000
  load(file)
  if(numbers$start != 30000){cat('Warning, not enough data for', file, '\n')}
}

# check if there's a matching file in processed
setwd("//hpc-fs/barnetta/auc/processed") # move to lyra where processed files are
to_compare = dir()
to_compare = str_remove_all(to_compare, 'pubmed\\.|\\.RData')
to_compare1 = to_compare[!str_detect(to_compare,'1074|1077')] # remove files with letters
to_compare1 = sprintf('%04d', as.numeric(to_compare1))
to_compare2 = to_compare[str_detect(to_compare,'[a-i]$')] # keep files with letters
to_compare = c(to_compare1, to_compare2)
#
to_check = str_remove_all(to_check, 'unprocessed\\.pubmed\\.baseline\\.|\\.RData')

#
to_compare[to_compare %in% to_check == FALSE]
to_check[to_check %in% to_compare == FALSE]
# file 0654 has all empty abstracts

# move back
setwd(here)