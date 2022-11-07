# 0_read_pubmed_baseline.R
# get all pubmed data from web; takes a while
# see https://dtd.nlm.nih.gov/ncbi/pubmed/doc/out/190101/index.html
# use annual baseline data, ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline (open in file manager and download to 'zipped' subfolder)
# had to download manually rather than using download.file and gunzip
# used 7-zip to unzip from gz to XML  
# help from IT, files are \\hpc-fs\barnetta\pubmed_baseline
# October 2022
library(XML)
library(dplyr)
library(stringr)
library(textclean) # for replace non-ASCII
library(easyPubMed) # for new_PM_df which imports XML into R
source('../narrator/99_table_articles_byAuth_adapted.R') # use my adapted versions of this code
source('../narrator/99_article_to_df_adapted.R') # faster without author data
# see https://cran.r-project.org/web/packages/easyPubMed/vignettes/getting_started_with_easyPubMed.html

# two key locations
home_location = "U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/text.mining/AUC"
download_location = "//hpc-fs/barnetta/pubmed_baseline"

# big loop through files
files = dir(download_location, pattern='^pubmed')
fnumbers = str_remove_all(files, pattern='^pubmed22n|\\.xml')
#
for (number in fnumbers){
  # check if file has already been processed
  file_exists = length(dir(paste(home_location, '/raw', sep=''), paste('unprocessed.pubmed.baseline.', number, '.RData', sep='')))>0
  if(file_exists == TRUE){ # if file exists move to next number
    next
  }
  file = paste(download_location, '/pubmed22n', number, '.xml', sep='')
  try(source('0b_run_read_baseline.R')) # run and ignore errors
}
