# 99_auc_next_numbers.R
# find AUC statistics based on:
# 1. next number in sentence
# 2. nearby words
# December 2022

## find AUC statistics based on next number in sentence
aucs2 = NULL
sentences = str_split(for_auc_clean, pattern='\\. |\\.$')[[1]] # split into sentence
sentences = sentences[sentences!='']
index = str_detect(sentences, pattern = paste(auc.words, collapse='|'))
#
any_sens_spec_text = str_detect(sentences, pattern = sens_spec_short) #
index[any_sens_spec_text] = FALSE # remove sentences with sensitivity/specificity

if(sum(index)>0){ #
  other_sentences = sentences[!index]
  sentences = sentences[index]
  sentences_with_numbers = str_detect(sentences, pattern = auc_number)
  sentences = sentences[sentences_with_numbers]
  auc = unlist(str_extract_all(sentences, pattern = auc_number))
  if(length(auc) > 0){
    aucs2 = c(aucs2, auc)
    # now remove first finds so they don't get entered again below
    # optional = could remove entire sentence
    for_auc = str_remove_all(for_auc, pattern = auc_number)
    for_auc_clean = str_remove_all(for_auc_clean, pattern = auc_number)
  }
}
if(is.null(aucs2)==FALSE){aucs2 = data.frame(type = 'mean', auc = aucs2)} # assuming these are all of type = mean

## get results where AUCs are near particular words
find_pattern = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ") # all combinations of AUC and phrases
find_pattern = paste(find_pattern, '( |0|1)\\.[0-9][0-9]?[0-9]?[0-9]?\\b', sep=' ')
find_pattern = paste(find_pattern, collapse = '|')
aucs3 = str_extract_all(for_auc_clean, pattern = find_pattern)[[1]]
if(length(aucs3) > 0){
  aucs3 = data.frame(type = 'mean', auc = aucs3) # assuming these are all of type = mean
  # now remove first finds so they don't get entered again below - in both versions
  for_auc_clean = str_remove_all(for_auc_clean, pattern = find_pattern )
  for_auc = str_remove_all(for_auc, pattern = find_pattern )
}
if(length(aucs3) == 0){aucs3 = NULL}
