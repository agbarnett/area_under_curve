# 99_main_function_abstract.R
# main function to extract the AUC from abstracts
# some code copied from https://github.com/agbarnett/stats_section/blob/master/code/plosone/5_process_stats_section.R
# used by 1_find_auc.R
# October 2022

# #
process_abstract = function(indata, k){

# process the abstract
abstract = indata$abstract[k]

# remove double spaces; and spaces at start and end
abstract = str_squish(abstract)
abstract = str_replace_all(abstract, pattern='\\( ', '\\(')
abstract = str_replace_all(abstract, pattern=' \\)', '\\)')
abstract = str_replace_all(abstract, pattern='sars.cov.2', 'sarscov2') # to avoid clash with number 2
abstract = str_replace_all(abstract, pattern='coronavirus.disease.2019|covid.?19|COVID.?19', 'COVIDnineteen') # to avoid clash with number 19
abstract = remove_commas(abstract) # remove commas in numbers

# remove from publication date onwards (only some abstracts)
is_pub_date = str_locate(pattern = 'Expected final online publication date', string=abstract)
if(any(!is.na(is_pub_date))){
  abstract = str_sub(abstract, 1, is_pub_date[1,1] - 1)
}

## AUC is detected or not
any_auc = str_detect(tolower(abstract), pattern = auc.pattern) # pattern from 1_other_patterns.R
# remove false matches for other types of area under the curve (see http://onbiostatistics.blogspot.com/2012/10/using-area-under-curve-auc-as-clinical.html)
find_remove = str_detect(tolower(abstract), pattern = to_remove) # pattern from 1_other_patterns.R
any_auc = ifelse(find_remove==TRUE, FALSE, any_auc)

## get sample size
source('99_sample_size.R', local = environment())

# look for AUC statistic
aucs = NULL
if (any_auc == TRUE){

  # switch to lower case  
  for_auc = tolower(abstract)
  
  #### remove things that can get in the way, see 1_patterns.R
  # (but keep original version of for_auc)
  # remove p-values and other statistics
  for_auc_clean = str_remove_all(for_auc, statistics_patterns) # 
  # remove numbers after plus/minus (from both)
  for_auc_clean = str_remove_all(for_auc_clean, plus_minus_patterns) 
  for_auc = str_remove_all(for_auc, plus_minus_patterns) 
  # remove numbers and text after sensitivity/specificity/youden/etc
  for_auc_clean = str_remove_all(for_auc_clean, sens_spec_patterns) 
  # remove numbers after thresholds (from both)
  for_auc_clean = str_remove_all(for_auc_clean, threshold_patterns) 
  for_auc = str_remove_all(for_auc, threshold_patterns) 
  # remove correlations
  for_auc_clean = str_remove_all(for_auc_clean, correlation_patterns) 
  # remove ratio numbers, e.g. '35:0' (from both)
  for_auc_clean = str_remove_all(for_auc_clean, '\\b[0-9][0-9]?[0-9]?:[0-9][0-9]?[0-9]?\\b') 
  for_auc = str_remove_all(for_auc, '\\b[0-9][0-9]?[0-9]?:[0-9][0-9]?[0-9]?\\b') 
  # remove double-spaces
  for_auc_clean = str_squish(for_auc_clean) 
  for_auc = str_squish(for_auc) 
  
  # (order of extractions does matter)
  ## AUC as a confidence interval
  source('99_auc_confidence_intervals.R', local = environment())

  ## AUC as a range/pair
  source('99_auc_pair.R', local = environment())

  ## AUC from nearby numbers
  source('99_auc_next_numbers.R', local = environment())

  ## AUC as a percent - using nearby words
  source('99_auc_percent.R', local = environment())

  ## "respectively" - uses for_auc (not clean)
  source('99_auc_respectively.R', local = environment())

  ## For sentences with combined statistics, split on statistics key words
  source('99_auc_nearby_statistics.R', local = environment())
  
  ## combine all sources of AUCs
  aucs = bind_rows(auc_numbers, # 99_auc_confidence_intervals.R
                   auc_numbers2, # 99_auc_confidence_intervals.R
                   aucs1_range_split, # 99_auc_pair.R
                   aucs1_split, # 99_auc_pair.R
                   aucs2, # 99_auc_next_numbers.R
                   aucs3, # 99_auc_next_numbers.R
                   aucs_percent, 
                   aucs_sub, # 99_auc_nearby_statistics.R 
                   auc_respectively) # 99_auc_respectively.R
  fcounts = NULL
  if(nrow(aucs) > 0){
    #
    aucs_char = filter(aucs, !is.na(auc))
    aucs = mutate(aucs_char, 
             auc = str_remove_all(auc, '[^0-9|\\.]'), # remove text
             digits = str_count(str_remove(auc, '^0'), '[0-9]'), # count decimal places ...
             auc = as.numeric(auc), # ... can now convert to number
             auc = ifelse(type == 'percent', auc/100, auc))  # convert percents
    # find non-numeric results
    f = filter(aucs, is.na(auc))
    if(nrow(f) > 0){
      cat('Warning, non-numeric AUC for', indata$pmid[k],'\n')
      print(aucs_char)
    }
    # count the number of statistics from each source - useful for checking the most important functions
    fcounts = c(nrow0(auc_numbers), # 99_auc_confidence_intervals.R
                nrow0(auc_numbers2), # 99_auc_confidence_intervals.R
                nrow0(aucs1_range_split), # 99_auc_pair.R
                nrow0(aucs1_split), # 99_auc_pair.R
                nrow0(aucs2), # 99_auc_next_numbers.R
                nrow0(aucs3), # 99_auc_next_numbers.R
                nrow0(aucs_percent), 
                nrow0(aucs_sub), # 99_auc_nearby_statistics.R 
                nrow0(auc_respectively))
    fnames = c('auc_numbers','auc_numbers2','aucs1_range_split','aucs1_split','aucs2','aucs3','aucs_percent','aucs_sub','auc_respectively')
    fcounts = data.frame(pmid = indata$pmid[k], source = fnames, counts = fcounts)
  }
}

## return the results
aframe = NULL
if(is.null(aucs) == FALSE){
  if(nrow(aucs) >0){
    aframe = mutate(aucs,
                    pmid = indata$pmid[k]) %>%
      filter(auc >= 0,
             auc <= 1) # exclude AUCs outside 0 to 1
    if(nrow(aframe) == 0){aframe = NULL}
  }
}
tframe = data.frame(pmid = indata$pmid[k], 
          date = indata$date[k], 
          type = indata$type[k], 
          jabbrv = indata$jabbrv[k], 
          n.authors = indata$n.authors[k],
          country = indata$country[k],
          any_auc = any_auc,
          sample_size = sample_size,
          stringsAsFactors = FALSE)
if(nrow(tframe)!=1){cat('error, wrong number of rows', indata$pmid[k], '.\n', sep='')}

# return all three
to.return = list()
to.return$aframe = aframe
to.return$tframe = tframe
to.return$fcounts = fcounts
return(to.return)

}

