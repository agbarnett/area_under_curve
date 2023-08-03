# 99_methods_papers.R
# looking for methods papers, responding to request from reviewer
# July 2023
library(dplyr)

load('data/analysis_ready.RData') # from 4_combine_results.R
methods_journals = c('Biostatistics','Stat Med','J Am Stat Assoc','J Stat Softw')
# no JRSS-B, or J Stat software, 
filter(abstracts, jabbrv %in% methods_journals) %>%
  group_by(jabbrv, any_auc) %>%
  tally()
filter(abstracts, jabbrv %in% methods_journals) %>%
  group_by(any_auc) %>%
  tally()

## and external validation
filter(abstracts, any_external) %>%
  group_by(any_auc) %>%
  tally()

# how good is external validation, take a sample, looks okay
filter(abstracts, any_external, any_auc) %>% sample_n(5) %>% select(pmid)

## to do: 
# run histogram?