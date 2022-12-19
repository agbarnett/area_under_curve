# 3_verify_algorithm_compare.R
# verify the algorithm using a random selection from pubmed
# see 2_verify_algorithm_create.R for random selection of pubmed data
# December 2022
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)
library(stringr)
library(openxlsx) # for reading in Excel
# code needed to run the algorithm:
source('99_main_function_abstract.R')
source('99_functions.R')
source('1_patterns.R')
source('1_confidence_intervals_pattern.R')

# basics for validation:
year = '2022' # 2020, 2021 or 2022
n_sample = 100

## part 1: read in results checked by hand
infile = paste("validate/AUC_mesh_", year, "_completed.xlsx", sep='') # year of search in file name
random_selection = read.xlsx(infile) %>%
  select(pmid, abstract, actual_sample_size, actual_AUC) %>%
  mutate(date = as.numeric(year), # just year
         jabbrv = 'validation', # does not matter for validation
         n.authors = 0, # does not matter for validation
         type = 'validation', # does not matter for validation
         country = 'validation', # does not matter for validation
  )

## part 2: run the random selections through the algorithm ##
algorithm_data = NULL
for (k in 1:n_sample){
  results = process_abstract(random_selection, k = k) # the algorithm
  frame = data.frame(pmid = results$tframe$pmid, 
                     abstract = random_selection$abstract[k],
                     sample_size = results$tframe$sample_size,
                     AUC = NA)
  if(is.null(results$aframe) == FALSE){
    frame$AUC = paste(results$aframe$auc, collapse = ', ')
  }
  algorithm_data = bind_rows(algorithm_data, frame)
}
algorithm_data = select(algorithm_data, pmid, sample_size, AUC)

## part 3: merge hand-entered data with algorithm data and then compare 
to_compare = full_join(random_selection, algorithm_data, by='pmid') %>%
  select(pmid, sample_size, AUC, actual_sample_size, actual_AUC)

# compare AUC
auc_compare = auc_numbers = NULL
for (k in 1:n_sample){
  this_compare = to_compare[k,]
  algorithm = as.numeric(str_split(this_compare$AUC, pattern = ',')[[1]])
  manual = as.numeric(str_split(this_compare$actual_AUC, pattern = ',')[[1]]) # causing some warning, need to figure out why
 # if(exists('last.warning') == TRUE){ 
#    cat(k, ', AUC = ', this_compare$actual_AUC, '\n')
#    last.warning = NULL
#  }
  n_algorithm = sum(!is.na(algorithm)) # count numbers of AUCs returned
  n_manual = sum(!is.na(manual))
  # frame of AUC - merge two sources
  algorithm = data.frame(auc = algorithm)
  manual = data.frame(auc = manual)
  frame = full_join(algorithm, manual, by = 'auc', keep=TRUE, suffix = c(".algorithm", ".manual")) %>%
    mutate(pmid = this_compare$pmid)
  auc_compare = bind_rows(auc_compare, frame)
  # frame of numbers
  frame = data.frame(pmid = this_compare$pmid, 
                     n_algorithm = n_algorithm,
                     n_manual = n_manual)
  auc_numbers = bind_rows(auc_numbers, frame)
}

# Tidy up compare data
auc_compare = filter(auc_compare, !(is.na(auc.algorithm) & is.na(auc.manual))) # remove if both missing
# differences per abstract - to here

# Bland-Altman plot for numbers per abstract
auc_numbers = mutate(auc_numbers,
                     diff = n_algorithm - n_manual,
                     av = (n_algorithm + n_manual)/2)
# limits of agreement
loa = summarise(auc_numbers, 
                lower = quantile(diff, 0.05),
                upper = quantile(diff, 0.95))
#
aplot = ggplot(data = auc_numbers, aes(x = av, y = diff))+
  geom_hline(yintercept = 0, lty=2, col='pink')+
  geom_hline(yintercept = loa$lower, lty=2, col='red')+ # 90% limit of agreement
  geom_hline(yintercept = loa$upper, lty=2, col='red')+
  geom_jitter(width = 0.15, height = 0.15)+ # to avoid overlap
  ylab('Difference, Algorithm minus Manual')+
  xlab('Average')+
  theme_bw()
aplot

# differences
filter(auc_numbers, n_algorithm > n_manual)
filter(auc_numbers, n_algorithm < n_manual)

# compare sample size
sample_size = mutate(to_compare, 
                  actual_sample_size = as.numeric(actual_sample_size))


