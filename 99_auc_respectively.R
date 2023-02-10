# 99_auc_respectively.R
# code to extract AUC in a long string of numbers with 'respectively'
# December 2022

# scan for "respectively"
auc_respectively = NULL
respect_pattern ="respectively|sensitivity,? ?specificity|specificity,? ?sensitivity"
any_respect = str_detect(for_auc, pattern = respect_pattern)
if(any_respect == TRUE){
  
  # find the sentence with "respectively"; use sentences with only full stop
  sentences_only_full_stop = str_split(for_auc, pattern='\\. |\\.$')[[1]] # split into sentence - using for_auc, not for_auc_clean
  sentences_only_full_stop = sentences_only_full_stop[sentences_only_full_stop!='']
  index = str_detect(sentences_only_full_stop, respect_pattern)
  sentences_with_respectively = sentences_only_full_stop[index]
  # split on semi-colon for long lists
  sentences = unlist(str_split(sentences_with_respectively, pattern='(\\.|;) |\\.$')) # split into sentence
  sentences = sentences[sentences!='']

  ## is 'respectively' of this style = lists of results split by statistics, e.g, "in addition, the aucs of iota sr, o-rads, and ca125 in the overall population were 0.831, 0.804, and 0.812, respectively, and the sensitivities of iota sr, o-rads, and ca125 were 94.42, 94.42, and 80.30%, respectively"
  # look at gaps between words
  all_gaps = NULL; skipit = FALSE
  for (loop in 1:length(sentences)){ # had to do individual sentences
    
    # must have statistics other than AUCs in sentence, otherwise skip and let other searches get the numbers
    index = str_detect(sentences[loop], paste(sens_spec_words, collapse = '|'))
    if (index == FALSE){skipit = TRUE} # changed to FALSE
      
    ## count all types of statistics
    n_stats = sum(str_count(sentences[loop], c(sens_spec_words, auc.words.no.breaks)))
    if(n_stats <= 1){next} 
    
    gaps = str_extract_all(sentences[loop], str_c(c(sens_spec_words, auc.words.no.breaks), collapse = "|")) %>% 
      setNames(seq_along(.)) %>%
      map_dfr(~ tibble(word = .x, position = str_locate(sentences[loop], pattern = .x)[,1]), .id = 'sentence') %>%
      mutate(lag = lag(position),
             diff = position - lag) %>%
      filter(!is.na(diff)) %>%
      summarise(av = mean(diff)) # average gap in characters
    if(any(!is.na(gaps$av))){all_gaps = c(all_gaps, gaps$av)}
  }
  if(is.null(all_gaps) == TRUE){skipit = TRUE} # not enough words, so move on
  if(skipit==FALSE){if(mean(all_gaps) > 50){skipit = TRUE}} # assume it is the other style of 'respectively' if gaps between statistics are this large
  
  if(skipit == FALSE){
    for (loop in 1:length(sentences)){
      
      # split on comma / and
      sentences_comma = str_replace_all(sentences[loop], '95% ci,', '95% ci') # remove comma after '95% ci' to avoid it being used as a break
      sentences_comma = str_split(sentences_comma, pattern = ',|\\band\\b')[[1]] # removed colon as can split on '95% ci'
      sentences_comma = sentences_comma[!sentences_comma%in% c('', ' ')]
      sentences_comma_no_ci = str_remove_all(sentences_comma, '9(0|5|9)\\%.?(ci|conf)') # remove so that 95% ci does not get counted as a number
      
      # sub-sentences with numbers
      any_numbers = str_detect(sentences_comma_no_ci, general_number)
      if(any(any_numbers) == FALSE){next} # skip if no numbers
      sentences_numbers = sentences_comma[any_numbers]
      
      # now detect statistics-words in order and find AUC number
      # (used stackoverflow for help https://stackoverflow.com/questions/74838185/ordering-of-words-in-r/74838257#74838257)
      order = str_extract_all(sentences[loop], str_c(c(sens_spec_words, auc.words.no.breaks), collapse = "|")) %>% 
        setNames(seq_along(.)) %>%
        map_dfr(~ tibble(word = .x, order = seq_along(word)), .id = 'sentence') %>%
        mutate(is_auc = str_detect(word, paste(auc.words.no.breaks, collapse = '|'))) %>%
        filter(is_auc) %>%
        pull(order)
      if(length(order)==0){next} # skip if no AUC
      order = min(order) # if multiple matches then chose first - could change to both?
      
      # now select part of text with AUC
      auc_text = sentences_numbers[order]
      if(is.na(auc_text)==TRUE){next} # skip if no matching text
      
      # are there confidence intervals?
      ci1 = str_detect(auc_text, ci_auc_number)
      ci2 = str_detect(auc_text, '9(0|5|9)\\%.?(ci|conf)')
      any_ci = ci1 | ci2
      this_pattern = ifelse(any_ci, ci_auc_number, auc_number) # pattern depends on CIs
      
      # skip if match is to a percent ... why?
      if(!any_ci & str_detect(auc_text, '\\%')==TRUE){next} 
      
      #
      aucs4 = str_extract_all(auc_text, pattern = this_pattern)[[1]]
      aucs4 = str_replace_all(aucs4, '9(0|5|9)\\%', replacement = ' ') # get rid of CI as a number
      aucs4 = str_replace_all(aucs4, pattern='[^0-9|\\.]', replacement=' ') # get rid of everything bar numbers
      aucs4 = str_squish(aucs4)
      aucs4 = unlist(str_split(aucs4, pattern = ' '))
      as.numbers = as.numeric(aucs4)
      #
      if(length(as.numbers)==3){
        if((all(as.numbers >= 0 & as.numbers <= 1)) & as.numbers[1]>=as.numbers[2] & as.numbers[1]<=as.numbers[3]){ # only keep if all three numbers are between 0 and 1, and if mean is within interval
          this_frame = data.frame(type = c('mean','lower','upper'), auc = aucs4)  # keep stats as non-numbers for now
          # add interval difference
          diff = make_diff(as.numbers) # function to extract largest CI width
          f_diff = data.frame(type = 'diff', auc = diff)
          #
          auc_respectively = bind_rows(auc_respectively, this_frame, f_diff)
        }
      }
      if(length(as.numbers) < 3 & length(as.numbers) > 0){
        this_frame = data.frame(type = 'mean', auc = aucs4) # keep stats as non-numbers for now
        auc_respectively = bind_rows(auc_respectively, this_frame)
      }
      if(length(as.numbers) > 0){
        # now remove numbers so they don't get entered again below
        # is currently the last extraction, so does not matter as num
        all_numbers = str_extract_all(sentences[loop], pattern = auc_number)[[1]] # could add '-' to end of auc_number in ending characters
        all_numbers = paste(all_numbers, collapse='|')
        all_numbers = str_replace_all(all_numbers, '[(]|[)]|[\\[]|[\\]]', '.') # first replace round/square brackets with any character - no slashes needed here as per 99_auc_pair.R
        if(nchar(all_numbers)>0){ # for rare occasions when there are no matches
          for_auc_clean = str_remove_all(for_auc_clean, pattern = all_numbers) 
          for_auc = str_remove_all(for_auc, pattern = all_numbers)
        }
      }
    }
  } # end of skipit if
} 
