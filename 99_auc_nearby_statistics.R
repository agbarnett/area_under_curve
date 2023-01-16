# 99_auc_nearby_statistics.R
# AUC split from nearby list of statistics
# useful for "respectively"
# December 2022

# use for_auc_clean here:
aucs_in = str_detect(for_auc_clean, pattern = paste(auc.words, collapse='|'))
aucs_sub = NULL
if(aucs_in == TRUE){
  sentences = str_split(for_auc_clean, pattern='\\. |\\.$')[[1]] # split into sentences
  sentences = sentences[sentences!='']
  index = str_detect(sentences, pattern = paste(auc.words, collapse='|'))
  numbers = str_detect(sentences, pattern = auc_number) # with and without decimal
  sentences = sentences[index & numbers] # just sentences with words and numbers
  if(length(sentences) > 0){
    for(a in 1:length(sentences)){
      # this is probably not working because I'm using 'clean' version; the 'clean' version is giving much better results that `for_auc`
      sub_sentences = str_split(sentences[a], pattern = paste(sens_spec_words, collapse = '|'))[[1]]
      # are there sub-sentences with just AUC statistics
      index = str_detect(sub_sentences, pattern = auc.pattern)
      not_index = str_detect(sub_sentences, pattern = sens_spec_short) # do not include sentences that have sensitivity, specificity, etc
      sub_sentences = sub_sentences[index & !not_index]
      if(length(sub_sentences)>0){
        sub_aucs = unlist(str_extract_all(sub_sentences, pattern = auc_number))
        if(length(sub_aucs) > 0){
          frame = data.frame(type = 'mean', auc = sub_aucs) # assuming these are all of type = mean
          aucs_sub = bind_rows(aucs_sub, frame)
          # remove finds so numbers do not get entered again
          sub_sentences = str_replace_all(sub_sentences, '[(]|[)]|[\\[]|[\\]]', '.') # first replace round/square brackets with any character - no slashes needed here as per 99_auc_pair.R
          for_auc_clean = str_remove_all(for_auc_clean, sub_sentences)
          for_auc_clean = str_replace_all(for_auc_clean, pattern='\\. \\.', replacement = '\\. ')
          for_auc_clean = str_squish(for_auc_clean)
        }
      }
    }
  }
}
