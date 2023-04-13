# 99_auc_pair.R
# extract AUCs that are in pairs; two attempts
# January 2023
library(stringr)


# a) AUC numbers
all_auc_words = paste(auc.words.no.breaks, collapse='|')
split_pattern = ' ?to ?| ?versus ?| ?vs\\.? ?| ?- ?' # text between the two AUC numbers
pair_end = paste('( range of | ranged between | of | |: ?|, ?)', auc_number_no_start_end, '(', split_pattern, ')', auc_number_no_start_end, boundary_no_dot_end, sep='')
pair_patterns = paste("(", all_auc_words, ')\\)?', pair_end, collapse='', sep='') # added options bracket end after AUC words
aucs1_range = str_extract_all(for_auc_clean, pattern = pair_patterns)[[1]]
aucs1_range_split = NULL
if(length(aucs1_range) > 0){
  aucs1_range = str_remove_all(aucs1_range, '\\)|aur?o?c of|aur?o?c|:|c.?statistics?') # clean up text a bit
  for (r in 1:length(aucs1_range)){
    aucs1_range_this_split = str_split(aucs1_range, pattern = split_pattern)[[r]]
    if(length(aucs1_range_this_split) < 2){next} # if not two numbers then skip
    aucs1_range_this_split = aucs1_range_this_split[aucs1_range_this_split!='']
    aucs1_range_this_split = data.frame(type = 'pair', auc = aucs1_range_this_split) # assuming these are all of type = mean
    aucs1_range_split = bind_rows(aucs1_range_split, aucs1_range_this_split)
    #print(aucs1_range_split)
  }
  # now remove first finds so they don't get repeated
  to_remove = str_replace_all(pair_patterns, '\\\\[(]|\\\\[)]|\\\\[\\[]|\\\\[\\]]', '.') # first replace round/square brackets with any character
  for_auc_clean = str_remove_all(for_auc_clean, pattern = to_remove)
  for_auc = str_remove_all(for_auc, pattern = to_remove) # can also remove from auc_clean as not related to other statistics
}
if(length(aucs1_range_split) == 0){aucs1_range_split = NULL}

# b) AUC as percents
all_auc_words = paste(auc.words.no.breaks, collapse='|')
split_pattern = ' ?to ?| ?versus ?| ?vs\\.? ?| ?- ?' # text between the two AUC numbers
pair_end = paste('( range of | ranged between | of | |: ?|, ?)', general_number_percent_no_start_end, '(', split_pattern, ')', general_number_percent_no_start_end, boundary_no_dot_end, sep='')
pair_patterns = paste("(", all_auc_words, ')', pair_end, collapse='', sep='')
aucs1_range = str_extract_all(for_auc_clean, pattern = pair_patterns)[[1]]
aucs1_range_split_percent = NULL
if(length(aucs1_range) > 0){
  aucs1_range = str_remove_all(aucs1_range, 'aur?o?c of|aur?o?c|:|c.?statistics?') # clean up text a bit
  for (r in 1:length(aucs1_range)){
    aucs1_range_this_split = str_split(aucs1_range, pattern = split_pattern)[[r]]
    if(length(aucs1_range_this_split) < 2){next} # if not two numbers then skip
    aucs1_range_this_split = aucs1_range_this_split[aucs1_range_this_split!='']
    aucs1_range_this_split = data.frame(type = 'pair - percent', auc = aucs1_range_this_split) # assuming these are all of type = mean
    aucs1_range_split_percent = bind_rows(aucs1_range_split_percent, aucs1_range_this_split)
    #print(aucs1_range_split_percent)
  }
  # now remove first finds so they don't get repeated
  to_remove = str_replace_all(pair_patterns, '\\\\[(]|\\\\[)]|\\\\[\\[]|\\\\[\\]]', '.') # first replace round/square brackets with any character
  for_auc_clean = str_remove_all(for_auc_clean, pattern = to_remove)
  for_auc = str_remove_all(for_auc, pattern = to_remove) # can also remove from auc_clean as not related to other statistics
}
if(length(aucs1_range_split_percent) == 0){aucs1_range_split_percent = NULL}

## as above without range
aucs1 = NULL
for (ind in 1:8){ # had to split because of the size of the search
  this_aucs1 = str_extract_all(for_auc_clean, pattern = sentence_pattern_aucs[[ind]])[[1]]
  if(length(this_aucs1) > 0){
    aucs1 = c(aucs1, this_aucs1)
    # now remove first finds so they don't get entered again below
    to_remove = str_replace_all(paste(this_aucs1, collapse='|'), '\\(|\\)|\\[|\\]', '.') # first replace round/square brackets
    for_auc_clean = str_remove_all(for_auc_clean, pattern = to_remove)
    for_auc = str_remove_all(for_auc, pattern = to_remove )
  }
}
aucs1_split = NULL
if(length(aucs1) > 0){
  aucs1_split = unlist(str_split(aucs1, pattern = ', | and | to ')) # split if these words are there, or there's a comma
  aucs1_split = aucs1_split[aucs1_split!='']
  aucs1_split = data.frame(type = 'mean', auc = aucs1_split) # assuming these are all of type = mean
}
if(length(aucs1_split) == 0){aucs1_split = NULL}

