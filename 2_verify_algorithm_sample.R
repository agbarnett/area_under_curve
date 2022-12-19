# 2_verify_algorithm_sample.R
# verify the algorithm using a random selection from pubmed
# search for MESH term with AUC
# creates the random sample for manual checking
# Dec 2022
library(rentrez)
library(dplyr)
library(stringr)
library(openxlsx) # for exporting to Excel
source('0_my_pubmed_key_do_not_share.R')

## part 1: get new sample of papers that likely have AUC ##

# search pubmed; removed PK studies as there were too many
year = '2020' # year to search pubmed
query = paste('(', year, '[pdat] AND hasabstract AND Area Under Curve[mesh]) NOT (meta-analyses[tiab] OR metaanalyses[tiab] OR meta-analysis[tiab] OR metaanalysis[tiab] OR pharmacokinetic[tiab] OR pharmacokinetics[tiab])', sep='')
search = entrez_search(db = "pubmed",
              term = query,
              retmax = 1000,
              api_key = my.api.key)

# take random sample
n_sample = 100
seed = TeachingDemos::char2seed('tranmere') # this works, but search data for 2022 changes, hence created 3_verify_algorithm_compare.R
sample = sample(search$ids, size = n_sample, replace=FALSE)
#sample = sample[order(-as.numeric(sample))] # order from low to high

# now get the abstracts
random_selection = NULL
for (k in 1:n_sample){
  detail = entrez_fetch(db = 'pubmed', id=sample[k], rettype='xml', parsed=FALSE, api_key = my.api.key) # get detail from pubmed
  Sys.sleep(5) # to avoid limiter
  parsed = parse_pubmed_xml(detail) # parse from XML
  frame = data.frame(pmid = parsed$pmid, # make into a data frame, use same variables as real data
                     date = parsed$year, # just year
                     jabbrv = parsed$journal,
                     n.authors = length(parsed$authors), # number of authors
                     type = 'validation', # does not matter for validation
                     country = 'validation', # does not matter for validation
                     title = parsed$title,
                     abstract = paste(parsed$abstract, collapse=' ')) # paste together paragraphs
  random_selection = bind_rows(random_selection, frame)
}

## part 2: export for checking by hand
# add blank column names for hand entered data
for_excel = select(random_selection, pmid, abstract) %>%
  mutate(actual_sample_size = NA,
         actual_AUC = NA)
  
#%>% arrange(pmid) # not useful
# header style
hs = createStyle(fontColour = "#ffffff", fgFill = "#4F80BD", halign = "center", wrapText = TRUE,
                  valign = "center", textDecoration = "Bold", border = "TopBottomLeftRight")
body = createStyle(wrapText = TRUE) # wrap text in abstract
outfile = paste("validate/AUC_mesh_", year, ".xlsx", sep='') # year of search in file name
wb = write.xlsx(for_excel, 
           file = outfile,
           headerStyle = hs)
addStyle(wb, style = body, sheet=1, cols=2, rows=2:101)
setColWidths(wb, sheet = 1, cols = 1:5, widths = c(9,100,9,10))
saveWorkbook(wb, file = outfile, overwrite = TRUE)

# see 3_verify_algorithm_compare.R for analysis

