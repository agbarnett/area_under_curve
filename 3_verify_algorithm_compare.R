# 3_verify_algorithm_compare.R
# verify the algorithm using a random selection from pubmed
# see 2_verify_algorithm_create.R for random selection of pubmed data
# Jan 2023
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(tidyr)
library(purrr)
library(dplyr)
library(stringr)
library(vecsets) # for vintersects
library(openxlsx) # for reading in Excel
# code needed to run the algorithm:
source('99_main_function_abstract.R')
source('99_functions.R')
source('1_patterns.R')
source('1_confidence_intervals_pattern.R')

## part 1: read in results checked by hand
random_selection = NULL
years = c('2020','2021','2022')
for (year in years){
  infile = paste("validate/AUC_mesh_", year, "_completed.xlsx", sep='') # year of search in file name
  this_selection = read.xlsx(infile) %>%
    select(pmid, abstract, actual_sample_size, actual_AUC) %>%
    mutate(date = as.numeric(year), # just year
           jabbrv = 'validation', # does not matter for validation
           n.authors = 0, # does not matter for validation
           type = 'validation', # does not matter for validation
           country = 'validation', # does not matter for validation
           actual_sample_size = NA # for consistency across Excel sheets
    )
  random_selection = bind_rows(random_selection, this_selection)
}
n_sample = nrow(random_selection)
random_selection = mutate(random_selection, mesh='') # do not have MESH terms


## part 2: run the random selections through the algorithm ##
algorithm_data = framed_results = NULL
for (k in 1:n_sample){
  results = process_abstract(random_selection, k = k) # the algorithm
  frame = data.frame(pmid = results$tframe$pmid, 
                     abstract = random_selection$abstract[k],
                     #sample_size = results$tframe$sample_size, # no longer used, too inaccurate
                     AUC = NA)
  if(is.null(results$aframe) == FALSE){
    framed_results = bind_rows(framed_results, results$aframe)
    aucs_to_add = filter(results$aframe, !str_detect(type,'diff')) %>% # do not include difference in CI
      pull(auc)
    frame$AUC = paste(aucs_to_add, collapse = ', ')
  }
  algorithm_data = bind_rows(algorithm_data, frame)
}

## part 3: merge hand-entered data with algorithm data and then compare 
to_compare = full_join(random_selection, algorithm_data, by='pmid') %>%
  select(pmid, AUC, actual_AUC)

# compare AUC
auc_compare = auc_numbers = NULL
for (k in 1:n_sample){
  this_compare = to_compare[k,]
  algorithm = as.numeric(str_split(this_compare$AUC, pattern = ',')[[1]])
  manual = as.numeric(str_split(this_compare$actual_AUC, pattern = ',')[[1]]) # 
 # if(exists('last.warning') == TRUE){  # looking at warnings
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

# Bland-Altman plot for numbers per abstract
auc_numbers_compare = mutate(auc_numbers,
                     diff = n_algorithm - n_manual,
                     av = (n_algorithm + n_manual)/2)
# limits of agreement for numbers
loa = summarise(auc_numbers_compare, 
                lower = quantile(diff, 0.05),
                upper = quantile(diff, 0.95))
#
aplot = ggplot(data = auc_numbers_compare, aes(x = av, y = diff))+
  geom_hline(yintercept = 0, lty=2, col='pink')+
  geom_hline(yintercept = loa$lower, lty=2, col='red')+ # 90% limit of agreement
  geom_hline(yintercept = loa$upper, lty=2, col='red')+
  geom_jitter(width = 0.15, height = 0.15)+ # to avoid overlap
  ylab('Difference, Algorithm minus Manual')+
  xlab('Average')+
  theme_bw()
aplot
jpeg('figures/bland_altman_validation.jpg', width=5, height=5, units='in', res=500, quality = 100)
print(aplot)
dev.off()

# differences
filter(auc_numbers_compare, n_algorithm > n_manual)
filter(auc_numbers_compare, n_algorithm < n_manual)

# compare sample size - no longer used, sample size needs much more work, so abandoned
#sample_size_compare = mutate(to_compare, 
#                  actual_sample_size = as.numeric(actual_sample_size),
#                  sdiff = sample_size - actual_sample_size)

# check with parts of the algorithm are returning the most AUCs
group_by(framed_results, source) %>%
  tally()

## compare AUC numbers - repeat from above?? 
auc_compare = NULL
for (k in 1:nrow(to_compare)){
  estimated = as.numeric(str_split(to_compare[k,]$AUC, pattern=',')[[1]])
  actual = as.numeric(str_split(to_compare[k,]$actual_AUC, pattern=',')[[1]])
  same = NULL
  if(length(intersect(estimated, actual)) > 0){
    same = vintersect(estimated, actual) # DOES NOT WORK WHEN THERE IS NO OVERLAP
  }
  differences1 = vsetdiff(estimated, actual) # in estimated but not actual
  differences2 = vsetdiff(actual, estimated) # in actual but not estimated
  f1 = f2 = f3 = NULL
  if(length(same) > 0){
    f1 = data.frame(estimated = same, actual = same)
  }
  if(length(differences1) > 0){
    f2 = data.frame(estimated = differences1, actual = NA)
  }
  if(length(differences2) > 0){
    f3 = data.frame(estimated = NA, actual = differences2)
  }
  frame = bind_rows(f1, f2, f3)
  frame$pmid = to_compare[k,]$pmid
  auc_compare = bind_rows(auc_compare, frame)
}
auc_compare = mutate(auc_compare, na.total = is.na(estimated) + is.na(actual)) %>%
  filter(na.total < 2) # remove rows where both missing

## compare the distribution of included and excluded numbers
# switch to long format
f1 = filter(auc_compare, is.na(estimated)) %>%
  mutate(type = 'Algorithm missed') %>%
  rename('auc' = 'actual')
f2 = filter(auc_compare, is.na(actual)) %>%
  mutate(type = 'Algorithm added') %>%
  rename('auc' = 'estimated')
f3 = filter(auc_compare, !is.na(estimated) & !is.na(actual)) %>%
  mutate(type = 'Both complete') %>%
  rename('auc' = 'estimated') # can be either
for_plot = bind_rows(f1, f2, f3)
# add numbers and percents to labels
n = group_by(for_plot, type) %>%
  tally() %>%
  mutate(percent = round(prop.table(n)*100),
         cell = paste(type, '\n n = ', n, ' (', percent, '%)', sep = '')) %>%
  ungroup()
for_plot = full_join(for_plot, n, by = 'type')
#
bplot = ggplot(for_plot, aes(x=cell, y=auc))+
  geom_boxplot()+
  geom_jitter(height=0, width=0.25)+
  xlab('')+
  ylab('AUC')+
  g.theme
bplot
jpeg('figures/boxplot_validation.jpg', width=5, height=5, units='in', res=500, quality = 100)
print(bplot)
dev.off()

# linear model of differences 
for_plot = mutate(for_plot,
type = as.factor(type),
type = relevel(type, ref='Both complete'))
mmodel = glm(auc ~ type, data = for_plot)
summary(mmodel)
hist(resid(mmodel))
# differences mostly due to 33706377 which has a `area under the precision-recall curve`
