# 99_auc_percent.R
# find AUCs as a percent based on nearby words
# December 2022

# make patterns to find matching phrases
find_pattern1 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern1 = paste(find_pattern1, general_number_percent_no_start_end, sep=' ?')
find_pattern1 = paste(find_pattern1, collapse = '|')
# version with `and` where first numbers is not a percent
find_pattern2a = paste(rep(auc.words.no.breaks[9], each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern2a = paste(find_pattern2a, general_number_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern2a = paste(find_pattern2a, collapse = '|')
# version with `and`
find_pattern2 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern2 = paste(find_pattern2, general_number_percent_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern2 = paste(find_pattern2, collapse = '|')
# version with comma and `and`
find_pattern3 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern3 = paste(find_pattern3, general_number_percent_no_start_end, ', ', general_number_percent_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern3 = paste(find_pattern3, collapse = '|')
# version with comma and `and`, with only last number as a percent
find_pattern3a = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern3a = paste(find_pattern3a, general_number_no_start_end, ', ', general_number_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern3a = paste(find_pattern3a, collapse = '|')
#
find_pattern = paste(c(find_pattern3a, find_pattern3, find_pattern2a, find_pattern2, find_pattern1), collapse='|')

# search
aucs_percent = str_extract_all(for_auc_clean, pattern = find_pattern)[[1]]
if(length(aucs_percent) > 0){
  # split if multiple statistics
  split_percents = str_split(aucs_percent, pattern = ' and ')[[1]]
  #
  aucs_percent = data.frame(type = 'percent', auc = split_percents) # flag as percent
  # now remove first finds so they don't get entered again below - from both versions
  find_pattern = str_replace_all(find_pattern, '\\(|\\)|\\[|\\]', '.') # first replace round/square brackets
  for_auc_clean = str_remove_all(for_auc_clean, pattern = find_pattern )
  for_auc = str_remove_all(for_auc, pattern = find_pattern ) # can remove from this one too as nothing to do with other statistics, e.g., sensitivity
}
if(length(aucs_percent) == 0){aucs_percent = NULL}
