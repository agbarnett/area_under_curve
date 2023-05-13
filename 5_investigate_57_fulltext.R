# 5_investigate_57_fulltext.R
# investigating the excess of results at 0.57
# use automated extraction of the fulltext of the papers to examine software

### not working, too many text not available, check by hand instead, see 99_random_checks.Rmd ###

# March 2023
library(XML)
library(stringr)
library(fulltext)
library(dplyr)

# what software to look for (lower case)
software = c('stata','sas','r','r.?studio','graphpad','spss','matlab','minitab','statistica','excel','jmp','jamovi')
software_breaks = paste('\\b', software, '\\b', sep='') # add breaks, so whole words only

# get the abstract data
load('data/analysis_ready.RData') # from 4_combine_results.R
# 0.57 and a comparison 0.58
are_57 = filter(results, auc == 0.57)
are_58 = filter(results, auc == 0.58)

# get DOIs from pmids (DOIs needed for fulltext package)
## add pmcid (pubmed central IDs); see https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
base_url = 'https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/'
all_ids = are_57$pmid
# run in batches
batch_size = 100
max_batch = floor(length(all_ids) / batch_size)
conversion = NULL
for (loop in 1:max_batch){
  start = (loop-1)*batch_size + 1
  stop = min(loop*batch_size, length(all_ids))
  ids = all_ids[start:stop]
  ids = paste(ids, collapse=',') # Use commas to separate multiple ID
  request = paste(base_url, '?tool=R&email=a.barnett@qut.edu.au&ids=', ids, sep='')
  destfile = 'data/xml.txt'
  download.file(request, destfile)
  xml = xmlParse(destfile) # parse the XML
  list = xmlSApply(xml["//record"], xmlAttrs) # convert to a list
  for (s in 1:length(list)){ # loop through the list
    this = list[[s]]
    d = data.frame(t(this))
    conversion = bind_rows(conversion, d)
  }
} # end of batch loop


# big loop
for (k in 1:10){
  # try to get full text
  doi = '10.1371/journal.pone.0086169'
  res <- ft_get(doi)
  # read xml
  result_xml = xmlParse(file = res$plos$data$path[[1]]$path) # read into R
  
  
  text = xmlValue(getNodeSet(result_xml, "/article//body"))
  software_found = str_detect(tolower(text), software_breaks)
  if(any(software_found) == FALSE){
    found = 'none'
  }
  if(any(software_found) == TRUE){
    found = paste(software[software_found], collapse=', ')
  }
}
