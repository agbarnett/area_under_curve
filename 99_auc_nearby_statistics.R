# 99_auc_nearby_statistics.R
# AUC split from nearby list of statistics
# useful for respectively
# December 2022

aucs_in = str_detect(for_auc, pattern = paste(auc.words, collapse='|'))
aucs_sub = NULL
if(aucs_in == TRUE){
  sentences = str_split(for_auc, pattern='\\. |\\.$')[[1]] # split into sentences
  sentences = sentences[sentences!='']
  index = str_detect(sentences, pattern = paste(auc.words, collapse='|'))
  numbers = str_detect(sentences, pattern = auc_number) # with and without decimal
  sentences = sentences[index & numbers] # just sentences with words and numbers
  if(length(sentences) > 0){
    for(a in 1:length(sentences)){
      sub_sentences = str_split(sentences[a], pattern = paste(sens_spec_words, collapse = '|'))[[1]]
      # are there sub-sentences with just AUC statistics
      index = str_detect(sub_sentences, pattern = auc.pattern)
      not_index = str_detect(sub_sentences, pattern = sens_spec_short) # do not include sentences that have sensitivity, specificity, etc
      sub_sentences = sub_sentences[index & !not_index]
      if(length(sub_sentences)>0){
        sub_aucs = unlist(str_extract_all(sub_sentences, pattern = auc_number))
        if(length(sub_aucs) > 0){
          aucs_sub = data.frame(type = 'mean', auc = sub_aucs) # assuming these are all of type = mean
        }
      }
    }
  }
}
