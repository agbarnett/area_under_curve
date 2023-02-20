# 1_list_pmids.R
# list all PMIDS and what file they are in
# Feb 2023
library(dplyr)

# loop
files_to_loop = dir('raw', pattern='baseline')
pmid_list = NULL
for (file in files_to_loop){  # 
  file_number = str_remove_all(file, 'unprocessed\\.pubmed\\.baseline\\.|\\.RData')
  infile = paste('raw/', file, sep='') # from 0_read_pubmed_api.R
  load(infile)
  pmid_list = bind_rows(pmid_list, pmids) 
}

#
save(pmid_list, file='data/pmid_list.RData')


