# 99_auc_percent.R
# find AUCs as a percent based on nearby words
# December 2022

find_pattern = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ") # all combinations of AUC and phrases
find_pattern = paste(find_pattern, general_number_percent, sep=' ')
find_pattern = paste(find_pattern, collapse = '|')
aucs_percent = str_extract_all(for_auc_clean, pattern = find_pattern)[[1]]
if(length(aucs_percent) > 0){
  aucs_percent = data.frame(type = 'percent', auc = aucs_percent) # flag as percent
  # now remove first finds so they don't get entered again below - from both versions
  for_auc_clean = str_remove_all(for_auc_clean, pattern = find_pattern )
  for_auc = str_remove_all(for_auc, pattern = find_pattern )
}
if(length(aucs_percent) == 0){aucs_percent = NULL}
