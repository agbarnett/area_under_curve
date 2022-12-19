# 99_auc_pair.R
# extract AUCs that are in pairs; two attempts
# December 2022

pair_end = paste('( of | |: ?|.)', auc_number,'( ?versus ?| ?to ?| ?vs\\.? ?| ?. ?| ?, ?)', auc_number, sep='')
pair_patterns = paste("\\b(", paste(auc.words.no.breaks, collapse='|'), ')', pair_end, collapse='|', sep='')
aucs1_range = str_extract_all(for_auc_clean, pattern = pair_patterns)[[1]]
aucs1_range_split = NULL
if(length(aucs1_range) > 0){
  aucs1_range = str_remove_all(aucs1_range, 'aur?o?c of|aur?o?c|:|c.?statistics?')
  for (r in 1:length(aucs1_range)){
    aucs1_range_this_split = str_split(aucs1_range, pattern = ' ?to ?| ?versus ?| ?vs\\.? ?| ?- ?| ?, ?')[[r]]
    aucs1_range_this_split = aucs1_range_this_split[aucs1_range_this_split!='']
    aucs1_range_this_split = data.frame(type = 'pair', auc = aucs1_range_this_split) # assuming these are all of type = mean
    aucs1_range_split = bind_rows(aucs1_range_split, aucs1_range_this_split)
    #print(aucs1_range_split)
  }
  # now remove first finds so they don't get repeated
  for_auc_clean = str_remove_all(for_auc_clean, pattern = pair_patterns)
}
if(length(aucs1_range_split) == 0){aucs1_range_split = NULL}

## as above without range
aucs1 = str_extract_all(for_auc_clean, pattern = sentence_pattern_aucs)[[1]]
if(length(aucs1) > 0){
  aucs1 = data.frame(type = 'mean', auc = aucs1) # assuming these are all of type = mean
  # now remove first finds so they don't get entered again below
  remove_pattern = paste('\\b(', paste(auc.words.no.breaks, collapse='|'), ')', link_words, auc_number, sep='')
  for_auc_clean = str_remove_all(for_auc_clean, pattern = remove_pattern )
}
if(length(aucs1) == 0){aucs1 = NULL}

