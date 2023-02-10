# 99_auc_confidence_intervals.R
# look for AUC confidence intervals - two parts
# December 2022

## part 1, pattern like 0.xx (0.xx-0.xx) with matching AUC-text from same sentence
# not working perfectly, is sometimes flagging second two numbers

just_numbers = str_locate_all(for_auc_clean, pattern = ci_auc_number)[[1]]
auc_numbers = NULL
if(nrow(just_numbers) > 0){ # if any CIs to look for
  
  sentences = str_split(for_auc_clean, pattern='(\\.|;) |\\.$')[[1]] # split into sentence, using full-stop or semi-colon
  sentences = sentences[sentences!='']
  
  # which sentences have AUC patterns?
  index = str_detect(sentences, pattern = auc.pattern)
  not_index = str_detect(sentences, pattern = sens_spec_short) # using "not clean" version; do not include sentences that have sensitivity, specificity, etc
  # for 'respectively'-style statistics
  if(any(not_index) == TRUE){
    sentences = remove_other_stats(sentences) # blank the other statistics
    index = str_detect(sentences, pattern = auc.pattern) # update
  }
  if(sum(index) > 0){ # only if matching sentences
    sentences_no = sentences[!index]
    sentences_yes = sentences[index]
    to_search = paste(sentences_yes, collapse='. ') # paste sentences with AUC back into a mini-abstract
    
    # update the search for numbers in the smaller abstract
    just_numbers = str_locate_all(to_search, pattern = ci_auc_number)[[1]]
    if(nrow(just_numbers) > 0){
      
      for (i in 1:nrow(just_numbers)){
        text_with_ci_long = str_sub(to_search, start = just_numbers[i,1], end = just_numbers[i,2])
        text_with_ci = str_replace_all(text_with_ci_long, pattern='[^0-9|\\.]', replacement=' ') # get rid of everything bar numbers
        text_with_ci = str_replace_all(text_with_ci, ' 9(0|5|9) |\\. |\\.$', ' ') # remove CI text and full-stops
        text_with_ci = str_squish(text_with_ci)
        numbers = str_split(text_with_ci, pattern = ' ')[[1]] # flag mean and CI
        if(length(numbers) == 3){ # 
          f = data.frame(type = c('mean','lower','upper'), auc=numbers)
          as.numbers = as.numeric(numbers)
          as.numbers[is.na(as.numbers)] = -99 # needed for rare typos
          if((all(as.numbers >= 0 & as.numbers <= 1)) & as.numbers[1]>=as.numbers[2] & as.numbers[1]<=as.numbers[3]){ # only keep if all three numbers are between 0 and 1, and if mean is within interval
            # add interval difference
            diff = make_diff(as.numbers) # function to extract largest CI width
            f_diff = data.frame(type = 'diff', auc = diff) # upper minus lower, must be a character for now
            auc_numbers = bind_rows(auc_numbers, f, f_diff)
            # now remove the text from the abstract so the numbers are not extracted again below
            text_with_ci_long = str_replace_all(text_with_ci_long, pattern='\\(|\\[|\\)|\\]', replacement='.') # should this be \\.?
            sentences_yes = str_remove_all(sentences_yes, pattern=text_with_ci_long)
          }
        }
      }
    }# end of nrow > 0 if
  
    # 
    for_auc_clean_no = paste(c(sentences_no,'. '), collapse='. ', sep='')
    for_auc_clean_yes = paste(c(sentences_yes,'. ') , collapse = '. ')
    for_auc_clean = str_squish(paste('dummy start sentence. ', for_auc_clean_no, for_auc_clean_yes, sep='', collapse = '. ')) # update abstract
    for_auc_clean = str_replace_all(for_auc_clean, pattern = '\\. \\.', '\\.') # remove double full-stops
  } # end of if index > 0

}


## part 1b, as above but with percentages ##
just_numbers_percent = str_locate_all(for_auc_clean, pattern = ci_auc_number_percent )[[1]]
just_numbers_percent2 = str_locate_all(for_auc_clean, pattern = ci_auc_number_percent2 )[[1]]
just_numbers_percent = rbind(just_numbers_percent, just_numbers_percent2)
auc_numbers_percent = NULL
if(nrow(just_numbers_percent) > 0){ # if any CIs to look for
  
  # remove duplicates
  if(nrow(just_numbers_percent)>1){
    just_numbers_percent = just_numbers_percent[!duplicated(just_numbers_percent),]
    just_numbers_percent = matrix(just_numbers_percent, ncol=2) # in case of format change
  }
  
  sentences = str_split(for_auc_clean, pattern='(\\.|;) |\\.$')[[1]] # split into sentence, using full-stop or semi-colon
  sentences = sentences[sentences!='']
  
  # which sentences have AUC patterns?
  index = str_detect(sentences, pattern = auc.pattern)
  not_index = str_detect(sentences, pattern = sens_spec_short) # using "not clean" version; do not include sentences that have sensitivity, specificity, etc
  # for 'respectively'-style statistics
  if(any(not_index) == TRUE){
    sentences = remove_other_stats(sentences) # blank the other statistics
    index = str_detect(sentences, pattern = auc.pattern) # update
  }
  if(sum(index) > 0){ # only if matching sentences
    sentences_no = sentences[!index]
    sentences_yes = sentences[index]
    to_search = paste(sentences_yes, collapse='. ') # paste sentences with AUC back into a mini-abstract
    
    # update the search for numbers in the smaller abstract
    just_numbers_percent = str_locate_all(to_search, pattern = ci_auc_number_percent )[[1]]
    just_numbers_percent2 = str_locate_all(to_search, pattern = ci_auc_number_percent2 )[[1]]
    just_numbers_percent = rbind(just_numbers_percent, just_numbers_percent2)
    if(nrow(just_numbers_percent) > 0){
      
      # remove duplicates
      if(nrow(just_numbers_percent)>1){
        just_numbers_percent = just_numbers_percent[!duplicated(just_numbers_percent),]
        just_numbers_percent = matrix(just_numbers_percent, ncol=2) # in case of format change
      }
      
      for (i in 1:nrow(just_numbers_percent)){
        text_with_ci_long = str_sub(to_search, start = just_numbers_percent[i,1], end = just_numbers_percent[i,2])
        text_with_ci = str_replace_all(text_with_ci_long, pattern='[^0-9|\\.]', replacement=' ') # get rid of everything bar numbers
        text_with_ci = str_replace_all(text_with_ci, ' 9(0|5|9) |\\. |\\.$', ' ') # remove CI text and full-stops
        text_with_ci = str_squish(text_with_ci)
        numbers = str_split(text_with_ci, pattern = ' ')[[1]] # flag mean and CI
        if(length(numbers) == 3){ # 
          f = data.frame(type = c('mean - percent','lower - percent','upper - percent'), auc=numbers)
          as.numbers = as.numeric(numbers)
          if((all(as.numbers >= 0 & as.numbers <= 100)) & as.numbers[1]>=as.numbers[2] & as.numbers[1]<=as.numbers[3]){ # only keep if all three numbers are between 0 and 100, and if mean is within interval
            # add interval difference
            diff = make_diff(as.numbers) # function to extract largest CI width
            f_diff = data.frame(type = 'diff - percent', auc = diff) # 
            auc_numbers_percent = bind_rows(auc_numbers_percent, f, f_diff)
            # now remove the text from the abstract so the numbers are not extracted again below
            text_with_ci_long = str_replace_all(text_with_ci_long, pattern='\\(|\\[|\\)|\\]', replacement='.') # should this be \\.?
            sentences_yes = str_remove_all(sentences_yes, pattern=text_with_ci_long)
          }
        }
      }
    }# end of nrow > 0 if
    
    for_auc_clean_no = paste(c(sentences_no,'. '), collapse='. ', sep='')
    for_auc_clean_yes = paste(c(sentences_yes,'. ') , collapse = '. ')
    for_auc_clean = str_squish(paste('dummy start sentence. ', for_auc_clean_no, for_auc_clean_yes, sep='', collapse = '. ')) # update abstract
    for_auc_clean = str_replace_all(for_auc_clean, pattern = '\\. \\.', '\\.') # remove double full-stops
  } # end of if index > 0
  
}


## part 2, look for confidence intervals with 'ci' (or similar) in wording ##
# can also do percents
auc_numbers2 = NULL
ci.places = str_locate_all(string = for_auc_clean, pattern = ci.pattern.spaces)[[1]] # all patterns, including annals - from 1_confidence_intervals_patterns.R
if(nrow(ci.places) > 0){ # if any CIs to look for
  
  for_auc_clean_new = for_auc_clean # temporary new abstract
  for_auc_new = for_auc # temporary new abstract
  
  ## get text that has mean
  # find full-stops
  full_stops = str_locate_all(for_auc_clean, pattern='\\. |\\.$')[[1]][,1]
  full_stops = c(1, full_stops) # add 1 in case match is in first sentence
  sentence_start = expand.grid(full_stops, ci.places[,1]) %>% # from full stop to start of match
    mutate(diff = Var2 - Var1) %>% # 
    filter(diff > 0) %>% # must be positive from full-stop to start of match
    group_by(Var2) %>% # by each match
    arrange(Var2, diff) %>%
    slice(1)

  # extract means
  mean.text = str_sub(for_auc_clean, sentence_start$Var1+2, sentence_start$Var2-1)   # from start of sentence up to CI phrase; assumes mean comes before CI
  
  ## get text that has CI (after mean text)
  word.ends = str_count(str_sub(for_auc_clean, start = 1, end = ci.places[,2]), ' ') # count the spaces from the start of the abstract to the END of the CI phrase
  max.words = min(which(is.na(word(for_auc_clean, 1:2000)))) # count maximum number of words using 'word' function
  end = word.ends + words.to.search 
  end[end >= max.words] = -1 # if beyond max words, then go to last word (negative - counts backward from last word) ; found by 23490371
  # number of words to skip depends on type of interval (Annals-type with no interval level)
  # not sure if this is working with "[ci]"
  plus = rep(1, nrow(ci.places))  # '1' used to be '2'
  # turned off - lost ci.places.annals
  #  if(nrow(ci.places.annals) > 0){
  #    aindex = ci.places[,1] %in% ci.places.annals[,1]
  #    plus[aindex] = 1
  #  }
  ci.text = word(for_auc_clean, start = word.ends+plus, end = end) # now take next set of words from starting location
  # loop through instances
  for (i in 1:nrow(ci.places)){
    # does it have AUC in mean text?
    any_auc_in_text = str_detect(mean.text[[i]], pattern = auc.pattern)
    any_sens_spec_text = str_detect(mean.text[[i]], pattern = sens_spec_short) #
    any_difference = str_detect(mean.text[[i]], pattern = 'difference|delta') #
    any_auc_in_text[any_sens_spec_text] = FALSE # remove sentences with sensitivity/specificity
    any_auc_in_text[any_difference] = FALSE # remove sentences that are likely for a difference
    if(any_auc_in_text ==FALSE){next}
    
    # remove any number after 'per' and any two numbers after 'range'; also split into words
    ci.words = remove.per(str_split(ci.text[[i]], pattern = what.to.split)[[1]])
    ci.words = ci.words[!ci.words %in% c('95','90','99')] # remove 95 for interval
    mean.words = remove.per(str_split(mean.text[[i]], pattern=what.to.split)[[1]])
    mean.words = mean.words[!mean.words %in% c('95','90','99')] # remove 95 for interval
    
    # are the numbers a percent?
    mean.text.no.ci = str_remove_all(mean.text[[i]], '9(0|5|9) ?\\%')
    ci.text.no.ci = str_remove_all(ci.text[[i]], '9(0|5|9) ?\\%')
    percent = str_detect(mean.text.no.ci, '[0-9] ?\\%') & str_detect(ci.text.no.ci, '[0-9] ?\\%') # percent in both
    
    # find next two numbers per CI
    nums = suppressWarnings(as.numeric(unlist(ci.words)))
    nums.to.test = nums[is.na(nums)==FALSE] # just look at first two numbers for next line ...
    if(all(nums.to.test[1:2]<=1, na.rm = TRUE)){percent = FALSE} # ... very unlikely to be percents if all numbers are under 1
    if(percent == TRUE){nums = nums / 100} # convert percentages here
    nums[nums > 1] = NA
    nums[nums < 0] = NA
    chars = suppressWarnings(unlist(ci.words)) # keep character version as this is better for removing duplicate text
    if(sum(is.na(nums)==FALSE) > 1){ # Any confidence intervals? (need two numbers) No numbers usually mean it was just text
      cis = chars[is.na(nums)==FALSE][1:2] # first two (use characters to keep decimal places)
      # find last number for mean
      nums = suppressWarnings(as.numeric(mean.words)) # suppressed warnings for turning 
      if(percent == TRUE){nums = nums / 100} # convert percentages here
      nums[nums > 1] = NA # blank numbers which can't be a AUC
      nums[nums < 0] =  NA
      mean = -99 # start with -99 (missing), to be replaced by the next steps
      if(sum(is.na(nums)==FALSE) >= 1){ # any matching mean numbers
        mean = mean.words[is.na(nums)==FALSE]
        mean = mean[length(mean)] # last number
      }
      f = data.frame(type = c('mean','lower','upper'), auc=c(mean, cis))
      if(percent == TRUE){f$auc = as.character(as.numeric(f$auc)/100)}
      as.numbers = as.numeric(c(mean, cis))
      if(percent == TRUE){as.numbers = as.numbers/100} # seperate version if working with percents
      if((all(as.numbers >= 0 & as.numbers <= 1)) & as.numbers[1]>=as.numbers[2] & as.numbers[1]<=as.numbers[3]){ # only keep if all three numbers are between 0 and 1, and if mean is within interval
        # do not add if intervals are a perfect match to an already included result
        if(is.null(auc_numbers2)==FALSE){
          already_included = inner_join(f, auc_numbers2, by=c('type','auc'))
          if(nrow(already_included) == 3){f = NULL} # must be perfect match
        }
        # add interval difference
        diff = make_diff(as.numbers) # function to extract largest CI width
        f_diff = data.frame(type = 'diff', auc = diff)
        #
        auc_numbers2 = bind_rows(auc_numbers2, f, f_diff)
        # now remove the text from the abstract so the numbers are not extracted below
        for_auc_clean_new = str_remove_all(for_auc_clean_new, pattern=paste(c(mean, cis), collapse='|'))
        for_auc_new = str_remove_all(for_auc_new, pattern=paste(c(mean, cis), collapse='|'))
      }
    }
  }
  for_auc_clean = for_auc_clean_new # shift back
  for_auc = for_auc_new # shift back
} # end of any CIs
