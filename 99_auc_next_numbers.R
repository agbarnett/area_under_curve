# 99_auc_next_numbers.R
# find AUC statistics based on:
# 1. next number in sentence
# 2. nearby words
# December 2022

## part 1: find AUC statistics based on next number in sentence
aucs2 = NULL
sentences = str_split(for_auc_clean, pattern='\\. |\\.$')[[1]] # split into sentence
sentences = sentences[sentences!='']
index = str_detect(sentences, pattern = paste(auc.words, collapse='|'))
# remove sentences with sensitivity/specificity - dealt with elsewhere in 'respectively'
any_sens_spec_text = str_detect(sentences, pattern = sens_spec_short) #
index[any_sens_spec_text] = FALSE 

if(sum(index)>0){ #
  other_sentences = sentences[!index]
  sentences = sentences[index]
  sentences_with_numbers = str_detect(sentences, pattern = sentence_pattern_aucs)
  sentences = sentences[sentences_with_numbers]
  auc = unlist(str_extract_all(sentences, pattern = sentence_pattern_aucs))
  if(length(auc) > 0){
    aucs2 = c(aucs2, auc)
    # now remove first finds so they don't get entered again below
    to_remove = str_replace_all(auc_number, '\\(|\\)|\\[|\\]', '.') # first replace round/square brackets
    # optional = could remove entire sentence
    for_auc = str_remove_all(for_auc, pattern = to_remove)
    for_auc_clean = str_remove_all(for_auc_clean, pattern = to_remove)
  }
}
if(is.null(aucs2)==FALSE){aucs2 = data.frame(type = 'mean', auc = aucs2)} # assuming these are all of type = mean


## part 2: get results where AUCs are near particular words
aucs3 = NULL
find_pattern = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ") # all combinations of AUC and phrases
find_pattern = paste(find_pattern, auc_number_no_start_end, sep=' ')
find_pattern = paste(find_pattern, collapse = '|')
# search by sentence
sentences = str_split(for_auc_clean, pattern='\\. |\\.$')[[1]] # split into sentence
sentences = sentences[sentences!='']
index = str_detect(sentences, pattern = find_pattern)
# remove sentences with sensitivity/specificity - dealt with elsewhere in 'respectively'
any_sens_spec_text = str_detect(sentences, pattern = sens_spec_short) #
index[any_sens_spec_text] = FALSE 

if(sum(index)>0){ #
  other_sentences = sentences[!index]
  sentences = sentences[index]
  sentences_with_numbers = str_detect(sentences, pattern = auc_number)
  sentences = sentences[sentences_with_numbers]
  auc = unlist(str_extract_all(sentences, pattern = find_pattern))
  if(length(auc) > 0){
    aucs3 = c(aucs3, auc)
    # now remove first finds so they don't get entered again below
    find_pattern = str_replace_all(find_pattern, '\\(|\\)|\\[|\\]', '.') # first replace round/square brackets
    # optional = could remove entire sentence
    for_auc = str_remove_all(for_auc, pattern = find_pattern)
    for_auc_clean = str_remove_all(for_auc_clean, pattern = find_pattern)
  }
}
if(is.null(aucs3)==FALSE){aucs3 = data.frame(type = 'mean', auc = aucs3)} # assuming these are all of type = mean
