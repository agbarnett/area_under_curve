#review_example_data.R
source('99_functions.R')
library(tidyverse)
library(tidytext)
#load data

load('example data/unprocessed.pubmed.baseline.0833.RData')

raw_pubmed = raw_pubmed %>% mutate_at('abstract',~str_squish(.) %>% 
                                        str_replace_all(., pattern='\\( ', '\\(') %>%
                                        str_replace_all(., pattern=' \\)', '\\)') %>% tolower(.))


#find all abstracts with AUC, ROC, AUROC and area under.*
auc_text = c('\\bauac\\b','\\bauroc\\b','\\baucroc\\b','\\bauc\\b','\\broc\\b','area under','receiver operating','c-statistic','c statistic')
auc_text = paste(auc_text, collapse = '|')



possible_matches = filter(raw_pubmed,grepl(auc_text,abstract,ignore.case=T))

raw_pubmed %>% filter()


auc_text = c('\\bauac\\b','\\bauroc\\b','\\bauc\\b','area under the curve','area under the accumulation curve','area under curve','area under roc curve')
auc_text = paste(auc_text, collapse = '|')
any_auc = str_detect(tolower(abstract), pattern = auc_text)


