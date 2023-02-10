# 99_auc_percent.R
# find AUCs as a percent based on nearby words
# Jan 2023

# first remove CIs
for_percent = str_remove_all(for_auc_clean, pattern = '9(0|5|9)\\%.confidence.interval|9(0|5|9)\\%.?ci')
# search
aucs_percent = str_extract_all(for_percent, pattern = find_pattern_percent)[[1]]
if(length(aucs_percent) > 0){
  # split if multiple statistics
  split_percents = unlist(str_split(aucs_percent, pattern = ', | and ')) # take them all
  #
  aucs_percent = data.frame(type = 'percent', auc = split_percents) # flag as percent
  # now remove first finds so they don't get entered again below - from both versions
  to_remove = str_replace_all(find_pattern_percent, '\\(|\\)|\\[|\\]', '.') # first replace round/square brackets
  for_auc_clean = str_remove_all(for_auc_clean, pattern = to_remove )
  for_auc = str_remove_all(for_auc, pattern = to_remove ) # can remove from this one too as nothing to do with other statistics, e.g., sensitivity
}
if(length(aucs_percent) == 0){aucs_percent = NULL}
