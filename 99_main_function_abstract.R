# 99_main_function_abstract.R
# main function to extract the AUC from abstracts
# some code copied from https://github.com/agbarnett/stats_section/blob/master/code/plosone/5_process_stats_section.R
# used by 1_find_auc.R
# Feb 2023

# #
process_abstract = function(indata, k){

# process the abstract
  mesh = tolower(indata$mesh[k])
  abstract = tolower(indata$abstract[k]) # switch to lower case

# remove double spaces; and spaces at start and end
abstract = str_squish(abstract)
abstract = str_replace_all(abstract, pattern='\\( ', '\\(') # remove space after opening round bracket
abstract = str_remove_all(abstract, pattern = to_remove_italic) # remove italic, bold, subscript, superscript
abstract = str_replace_all(abstract, pattern=' \\)', '\\)')
abstract = str_replace_all(abstract, pattern='sars.cov.2', 'sarscov2') # to avoid clash with number 2
abstract = str_replace_all(abstract, pattern='coronavirus.disease.2019|covid.?19|COVID.?19', 'COVIDnineteen') # to avoid clash with number 19
abstract = remove_commas(abstract) # remove commas in numbers
abstract = str_replace_all(abstract, pattern='[a-z][0-9]-[0-9]','x') # remove numbers involved in hyphens with letters and numbers, add dummy 'x'
abstract = str_replace_all(abstract, pattern='[a-z]-[0-9]','x') # remove numbers involved in hyphens with letters and numbers
abstract = gsub("·", ".", abstract, perl = FALSE)  # replace `high` decimal places used by Lancet
abstract = str_remove_all(abstract, pattern='\\{|\\}') # caused problems with some search terms
abstract = str_replace_all(abstract, pattern='\\+\\/-|-\\/\\+', replacement = '±') # caused problems with some search terms
abstract = str_replace_all(abstract, pattern='\\+', replacement = ' ') # caused problems with some search terms
abstract = str_remove_all(abstract, '\\b[0-9][0-9]?[0-9]? ?: ?[0-9][0-9]?[0-9]?\\b') # remove ratio numbers, e.g. '35:0' (from both)
abstract = str_remove_all(abstract, '\\b[0-9][0-9]?[0-9]? ?: ?[0-9][0-9]?[0-9]? ?: ?[0-9][0-9]?[0-9]?\\b') # remove ratio numbers, e.g. '35:0' (from both)
abstract = str_replace_all(abstract, 'auc.accuracy','auc') # remove problematic pattern
abstract = str_remove_all(abstract, plus_minus_patterns) # remove numbers after plus/minus (from both)
abstract = str_replace_all(abstract, '(?<=[0-9]) \\%', '%') # remove space to percent - using look ahead
abstract = str_replace_all(abstract, '\\bfev.?[0-9]', ' fev') # 
abstract = str_replace_all(abstract, '[a-z]\\(1\\)', '(one)') # e.g. 19161210
abstract = str_replace_all(abstract, '1 ige', 'one ige') # 
abstract = str_replace_all(abstract, 'vs\\. ', 'vs ') # avoid this looking like a full-stop 
abstract = str_replace_all(abstract, '1 ?- ?auc', 'one - auc') # 
abstract = str_remove_all(abstract, scientific) # remove scientific numbers
abstract = str_replace_all(abstract, '\\bic.9(0|5|9)(\\%)?\\b', '95% ci') #  # reverse CI causes confusion
# remove years, etc as they get confused with numbers
abstract = str_replace_all(abstract, time_pattern, ' ') 
# remove describing text
abstract = str_remove_all(abstract, describing_pattern) 
#
abstract = str_squish(abstract)

# uses commas instead of full-stops for AUC numbers
count_commas = str_count(abstract, '0,[0-9]')
if(count_commas > 2){
  abstract = str_replace_all(abstract, '0,', '0.')
}

# remove `1` used as a counter
index = str_detect(abstract, '(\\(| )1\\) |(\\(| )1\\. |(\\(| )1, |(\\(| )1- ') & str_detect(abstract, '(\\(| )2\\) | 2\\. | 2, | 2- ')
if(index == TRUE){
  abstract = str_replace_all(abstract, '(\\(| )1\\) |(\\(| )1\\. |(\\(| )1, |(\\(| )1- ', ' ')
}

# remove from publication date onwards (only some abstracts)
is_pub_date = str_locate(pattern = 'Expected final online publication date', string=abstract)
if(any(!is.na(is_pub_date))){
  abstract = str_sub(abstract, 1, is_pub_date[1,1] - 1)
}

## AUC is detected or not
any_auc = str_detect(tolower(abstract), pattern = auc.pattern) # pattern from 1_patterns.R
# remove false matches for other types of area under the curve (see http://onbiostatistics.blogspot.com/2012/10/using-area-under-curve-auc-as-clinical.html)
find_remove = str_detect(tolower(abstract), pattern = to_remove_not_auc) # pattern from 1_patterns.R
any_auc = ifelse(find_remove==TRUE, FALSE, any_auc)
is_pk = str_detect(tolower(abstract), pattern = to_remove_not_auc_additional) # pattern from 1_patterns.R
is_pk_mesh = str_detect(tolower(mesh), pattern = mesh_exclude_additional) 
any_auc = ifelse(is_pk==TRUE | is_pk_mesh==TRUE, FALSE, any_auc) # do not search for AUCs if PK study
# flag to exclude PK studies
exclude = ifelse(is_pk==TRUE | is_pk_mesh==TRUE, TRUE, FALSE)

## get sample size - not accurate enough
#source('99_sample_size.R', local = environment())

# look for AUC statistic
aucs = fcounts = NULL
if (any_auc == TRUE){

  for_auc = abstract
  
  #### remove things that can get in the way, see 1_patterns.R
  # (but keep original version of for_auc)
  # remove p-values and other statistics - takes the longest
  for_auc_clean = str_remove_all(for_auc, statistics_patterns) # 
  # remove numbers and text after sensitivity/specificity/youden/etc
  for_auc_clean = str_remove_all(for_auc_clean, sens_spec_patterns) 
  # remove numbers after thresholds (from both)
  for_auc_clean = str_remove_all(for_auc_clean, threshold_patterns) 
  for_auc = str_remove_all(for_auc, threshold_patterns) 
  # remove correlations
  for_auc_clean = str_remove_all(for_auc_clean, correlation_patterns) 
  # change one specific pattern that causes problems
  for_auc_clean = str_replace_all(for_auc_clean, pattern='auc\\(roc\\)', replacement = 'auc')
  for_auc = str_replace_all(for_auc, pattern='auc\\(roc\\)', replacement = 'auc')
  # change one specific pattern for confidence intervals
  for_auc_clean = str_replace_all(for_auc_clean, pattern='c\\.i\\.', replacement = 'ci')
  for_auc = str_replace_all(for_auc, pattern='c\\.i\\.', replacement = 'ci')
  # remove double-spaces
  for_auc_clean = str_squish(for_auc_clean) 
  for_auc = str_squish(for_auc) 
  
  # (order of extractions DOES matter)
  ## AUC as a confidence interval
  source('99_auc_confidence_intervals.R', local = environment())

  ## AUC as a range/pair, also gets nearby numbers
  source('99_auc_pair.R', local = environment())

  ## AUC from nearby numbers - not needed, done by 99_auc_pair.R
  #source('99_auc_next_numbers.R', local = environment())

  ## AUC as a percent - using nearby words
  source('99_auc_percent.R', local = environment())

  ## "respectively" - uses for_auc (not clean)
  source('99_auc_respectively.R', local = environment())

  ## For sentences with combined statistics, split on statistics key words
  source('99_auc_nearby_statistics.R', local = environment())
  
  ## combine all sources of AUCs
  # include which code-source found the stats
  if(is.null(auc_numbers)==FALSE){auc_numbers = mutate(auc_numbers, source='confidence interval 1')} # 99_auc_confidence_intervals.R
  if(is.null(auc_numbers2)==FALSE){auc_numbers2 = mutate(auc_numbers2, source='confidence interval 2')} # 99_auc_confidence_intervals.R
  if(is.null(auc_numbers_percent)==FALSE){auc_numbers_percent = mutate(auc_numbers_percent, source='confidence interval 3')} # 99_auc_confidence_intervals.R
  if(is.null(aucs1_range_split)==FALSE){aucs1_range_split = mutate(aucs1_range_split, source='pair 1')} # 99_auc_pair.R
  if(is.null(aucs1_range_split_percent)==FALSE){aucs1_range_split_percent = mutate(aucs1_range_split_percent, source='pair 2')} # 99_auc_pair.R
  if(is.null(aucs1_split)==FALSE){aucs1_split = mutate(aucs1_split, source='pair 3')} # 99_auc_pair.R
  if(is.null(aucs_percent)==FALSE){aucs_percent = mutate(aucs_percent, source='percent')} # 99_auc_percent.R
  if(is.null(aucs_sub)==FALSE){aucs_sub = mutate(aucs_sub, source='nearby')} # 99_auc_nearby_statistics.R
  if(is.null(auc_respectively)==FALSE){auc_respectively = mutate(auc_respectively, source='respectively')} # 99_auc_respectively.R
  aucs = bind_rows(auc_numbers,
                   auc_numbers2,
                   auc_numbers_percent,
                   aucs1_range_split,
                   aucs1_range_split_percent,
                   aucs1_split,
                   aucs_percent,
                   aucs_sub,
                   auc_respectively)
  if(nrow(aucs) > 0){
    # final processing of AUC statistics
    aucs_char = filter(aucs, !is.na(auc))
    aucs = mutate(aucs_char, 
             auc = str_remove_all(auc, '[^0-9|\\.]'), # remove text
             digits = str_count(str_remove(auc, '^(0|1)'), '[0-9]'), # count decimal places ...
             auc = as.numeric(auc), # ... can now convert to number
             auc = ifelse(str_detect(type, 'percent'), auc/100, auc))  # convert percentages
    # find non-numeric results
    f = filter(aucs, is.na(auc))
    if(nrow(f) > 0){
      cat('Warning, non-numeric AUC for', indata$pmid[k],'\n')
      print(aucs_char)
    }
  }
}

## return the results
aframe = NULL
if(is.null(aucs) == FALSE){
  if(nrow(aucs) >0){
    aframe = mutate(aucs,
                    pmid = indata$pmid[k],
					date = indata$date[k]) %>% # added as there may be PMID duplicates
      filter(auc >= 0,
             auc <= 1) # exclude AUCs outside 0 to 1
    any_auc = TRUE
    if(nrow(aframe) == 0){
      aframe = NULL
      any_auc = FALSE
    }
  }
}
tframe = data.frame(pmid = indata$pmid[k], 
          date = indata$date[k], 
          type = indata$type[k], 
          jabbrv = indata$jabbrv[k], 
          n.authors = indata$n.authors[k],
          country = indata$country[k],
          any_auc = any_auc, # any AUCs found
          exclude = exclude, # exclude as PK
          #sample_size = sample_size,# not accurate enough
          stringsAsFactors = FALSE)
if(nrow(tframe)!=1){cat('error, wrong number of rows', indata$pmid[k], '.\n', sep='')}

# return all three
to.return = list()
to.return$aframe = aframe
to.return$tframe = tframe
return(to.return)

}

