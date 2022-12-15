# 99_main_function_abstract.R
# main function to extract the AUC from abstracts
# some code copied from https://github.com/agbarnett/stats_section/blob/master/code/plosone/5_process_stats_section.R
# used by 1_find_auc.R
# October 2022

# #
process_abstract = function(indata, k){

# process the abstract
abstract = indata$abstract[k]

# remove double spaces; and spaces at start and end
abstract = str_squish(abstract)
abstract = str_replace_all(abstract, pattern='\\( ', '\\(')
abstract = str_replace_all(abstract, pattern=' \\)', '\\)')
abstract = str_replace_all(abstract, pattern='coronavirus.disease.2019|covid.?19|COVID.?19', 'COVIDnineteen') # to avoid clash with number 19
abstract = remove_commas(abstract) # remove commas in numbers

# remove publication date onwards
is_pub_date = str_locate(pattern = 'Expected final online publication date', string=abstract)
if(any(!is.na(is_pub_date))){
  abstract = str_sub(abstract, 1, is_pub_date[1,1] - 1)
}

## AUC is detected or not
any_auc = str_detect(tolower(abstract), pattern = auc.pattern) # pattern from 1_other_patterns.R
# remove false matches for other types of area under the curve (see http://onbiostatistics.blogspot.com/2012/10/using-area-under-curve-auc-as-clinical.html)
find_remove = str_detect(tolower(abstract), pattern = to_remove) # pattern from 1_other_patterns.R
any_auc = ifelse(find_remove==TRUE, FALSE, any_auc)

## sample size
sample_size = str_extract_all(tolower(abstract), sample_size_patterns)[[1]]
if(length(sample_size) > 0){
  split = unlist(str_split(sample_size, pattern = '\\b')) # split into words
  split = split[!split %in% c('',' ')] # remove blanks
  any_chars = str_detect(str_remove_all(split, '[0-9]'),'[^0-9]') # need to remove numbers combined with characters, e.g., "t1dm"
  sample_sizes = str_remove_all(split, '[^0-9]')
  sample_sizes = sample_sizes[!any_chars]
  sample_sizes = as.numeric(sample_sizes)
  sample_size = sum(sample_sizes) # assume separate groups add to total?
  # if authors give sample size, then split of test and training sizes
  if(length(sample_sizes)==3 & sample_sizes[1] == sum(sample_sizes[2:3])){
    sample_size = sample_sizes[1]
  }
}
# look for sample size in words 
if(length(sample_size) == 0){ 
  # find two words before match
  slocs = str_locate_all(tolower(abstract), sample_size_words)[[1]]
  if(length(slocs) > 0){
    for (r in 1:nrow(slocs)){
      words = str_split(str_sub(abstract, 1, slocs[r,2]), pattern='\\W')[[1]] # split on any not word
      words = words[words!='']
      n = length(words)
      if(n <= 3){last_words = words}
      if(n > 3){last_words = rev(words[(n-3):(n-1)])} # take last 3 words
      for (index in 1:3){
        res = tryCatch(to_number(last_words[1:index]), error=function(e){'error'}) # change words to numbers
        if(is.character(res)[1] == TRUE){next}
        sample_size = sum(as.numeric(res)) # add all numbers
        if(is.na(sample_size)==FALSE){next}
      }
    }
  }
}
if(length(sample_size) == 0){sample_size = NA}

# look for AUC statistic
aucs = NULL
if (any_auc == TRUE){
  
  #### remove things that can get in the way, see 1_patterns.R
  for_auc = tolower(abstract)
  # remove p-values and other statistics
  for_auc = str_remove_all(for_auc, statistics_patterns) 
  # remove numbers after plus/minus
  for_auc = str_remove_all(for_auc, plus_minus_patterns) 
  # remove numbers and text after sensitivity/specificity/youden
  for_auc = str_remove_all(for_auc, sens_spec_patterns) 
  # remove AUC thresholds
  for_auc = str_remove_all(for_auc, threshold_patterns) 
  # remove standard errors
  for_auc = str_remove_all(for_auc, se_patterns) 
  # remove correlations
  for_auc = str_remove_all(for_auc, correlation_patterns) 
  for_auc = str_squish(for_auc) # remove double-spaces
  
  ## look for AUC confidence intervals - part 1, pattern like 0.xx (0.xx-0.xx) with matching AUC-text from same sentence
  # (|0|1) allows for '0.6' and '.6'
  just_numbers = str_locate_all(for_auc, pattern="(|0|1)\\.\\d*\\b ?(\\(|\\[|,|:) ?(|0|1)\\.\\d*\\b.?.?.?.?(|0|1)\\.\\d*\\b")[[1]]
  auc_numbers = NULL
  if(nrow(just_numbers) > 0){ # if any CIs to look for

    sentences = str_split(for_auc, pattern='\\. |\\.$')[[1]] # split into sentence
    sentences = sentences[sentences!='']
    
    # which sentences have AUC patterns?
    index = str_detect(sentences, pattern = auc.pattern)
    not_index = str_detect(sentences, pattern = sens_spec_short) # do not include sentences that have sensitivity, specificity, etc
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
      just_numbers = str_locate_all(to_search, pattern="(0|1)\\.\\d\\d?\\d?\\d? ?(\\(|\\[|:|;) ?(0|1)\\.\\d\\d?\\d?\\d?.?.?.?.?(0|1)\\.\\d\\d?\\d?\\d?")[[1]]
      if(nrow(just_numbers) > 0){
    
      for (i in 1:nrow(just_numbers)){
        text_with_ci_long = str_sub(to_search, start = just_numbers[i,1], end = just_numbers[i,2])
        text_with_ci = str_replace_all(text_with_ci_long, pattern='[^0-9|\\.]', replacement=' ') # get rid of everything bar numbers
        text_with_ci = str_squish(text_with_ci)
        numbers = str_split(text_with_ci, pattern = ' ')[[1]] # flag mean and CI
        if(length(numbers) == 3){ # 
          f = data.frame(type = c('mean','lower','upper'), auc=numbers)
          as.numbers = as.numeric(numbers)
          if((all(as.numbers >= 0 & as.numbers <= 1)) & as.numbers[1]>as.numbers[2] & as.numbers[1]<as.numbers[3]){ # only keep if all three numbers are between 0 and 1, and if mean is within interval
            auc_numbers = bind_rows(auc_numbers, f)
            # now remove the text from the abstract so the numbers are not extracted again below
            text_with_ci_long = str_replace_all(text_with_ci_long, pattern='\\(|\\[|\\)|\\]', replacement='.')
            sentences_yes = str_remove_all(sentences_yes, pattern=text_with_ci_long)
          }
        }
      }
      }# end of nrow > 0 if
    } # end of if index > 0
    
    for_auc_no = paste(c(sentences_no,'.'), collapse='. ', sep='')
    for_auc_yes = paste(c(sentences_yes,'.') , collapse = '. ')
    for_auc = str_squish(paste(for_auc_no, for_auc_yes, sep='', collapse = '. ')) # update abstract
    for_auc = str_replace_all(for_auc, pattern = '\\. \\.', '\\.') # remove double spaces
  }
    
  
  ## look for confidence intervals - part 2, with 'ci' (or similar) in wording ##
  auc_numbers2 = NULL
  ci.places = str_locate_all(pattern = ci.pattern.spaces, string = for_auc)[[1]] # all patterns, including annals - from 1_confidence_intervals_patterns.R
  if(nrow(ci.places) > 0){ # if any CIs to look for
    
    for_auc_new = for_auc # temporary new abstract
    
    ## get text that has mean
    # find full-stops
    full_stops = str_locate_all(for_auc, pattern='\\. |\\.$')[[1]][,1]
    sentence_start = expand.grid(full_stops, ci.places[,1]) %>%
      mutate(diff = Var2 - Var1) %>%
      filter(diff > 0) %>%
      group_by(Var2) %>%
      arrange(Var2, diff) %>%
      slice(1)
    # extract means
    mean.text = str_sub(for_auc, sentence_start$Var1+2, sentence_start$Var2-1)   # from start of sentence up to CI phrase; assumes mean comes before CI
    
    ## get text that has CI
    word.ends = str_count(str_sub(for_auc, start=1, end=ci.places[,2]), ' ') # count the spaces from the start of the abstract to the END of the CI phrase
    max.words = min(which(is.na(word(for_auc, 1:2000)))) # count maximum number of words using 'word' function
    end = word.ends + words.to.search 
    end[end >= max.words] = -1 # if beyond max words, then go to last word (negative - counts backward from last word) ; found by 23490371
    # number of words to skip depends on type of interval (Annals-type with no interval level)
    # not sure if this is working with "[ci]"
    plus = rep(1, nrow(ci.places))  # used to be '2'
    # turned off - lost ci.places.annals
  #  if(nrow(ci.places.annals) > 0){
  #    aindex = ci.places[,1] %in% ci.places.annals[,1]
  #    plus[aindex] = 1
  #  }
    ci.text = word(for_auc, start = word.ends+plus, end = end) # now take next set of words from starting location
    # loop through instances
    for (i in 1:nrow(ci.places)){
      # does it have AUC in mean text?
      any_auc_in_text = str_detect(mean.text[[i]], pattern = auc.pattern)
      any_sens_spec_text = str_detect(mean.text[[i]], pattern = sens_spec_short) #
      any_auc_in_text[any_sens_spec_text] = FALSE # remove sentences with sensitivity/specificity
      if(any_auc_in_text ==FALSE){next}

      # remove any number after 'per' and any two numbers after 'range'; also split into words
      ci.words = remove.per(str_split(ci.text[[i]], pattern=what.to.split)[[1]])
      ci.words = ci.words[!ci.words %in% c('95','90','99')] # remove 95 for interval
      mean.words = remove.per(str_split(mean.text[[i]], pattern=what.to.split)[[1]])
      mean.words = mean.words[!mean.words %in% c('95','90','99')] # remove 95 for interval
      
      # find next two numbers per CI
      nums = suppressWarnings(as.numeric(unlist(ci.words)))
      nums[nums > 1] = NA
      nums[nums < 0] = NA
      chars = suppressWarnings(unlist(ci.words)) # keep character version
      if(sum(is.na(nums)==FALSE) > 1){ # Any confidence intervals? (need two numbers) No numbers usually mean it was just text
        cis = chars[is.na(nums)==FALSE][1:2] # first two (use characters to keep decimal places)
        # find last number for mean
        nums = suppressWarnings(as.numeric(mean.words)) # suppressed warnings for turning 
        nums[nums > 1] = NA # blank numbers which can't be a AUC
        nums[nums < 0] =  NA
        mean = -99 # start with -99 (missing), to be replaced by the next steps
        if(sum(is.na(nums)==FALSE) >= 1){ # any matching mean numbers
          mean = mean.words[is.na(nums)==FALSE]
          mean = mean[length(mean)] # last number
        }
        f = data.frame(type = c('mean','lower','upper'), auc=c(mean, cis))
        as.numbers = as.numeric(c(mean, cis))
        if((all(as.numbers >= 0 & as.numbers <= 1)) & as.numbers[1]>=as.numbers[2] & as.numbers[1]<=as.numbers[3]){ # only keep if all three numbers are between 0 and 1, and if mean is within interval
          auc_numbers2 = bind_rows(auc_numbers2, f)
          # now remove the text from the abstract so the numbers are not extracted below
          for_auc_new = str_remove_all(for_auc_new, pattern=paste(c(mean, cis), collapse='|'))
        }
      }
    }
    for_auc = for_auc_new # shift back
  } # end of any CIs
  
  
  ## AUC as a range/pair, using `?. ?` for any hyphen
  pair_start = c('\\bc.?statistics?\\b','\\baurocs? of','\\baurocs?.?:?','\\baucs? of','\\baucs?.?:?')
  pair_end = '\\b.?.?(0|1)?\\.\\d\\d?\\d?\\d?\\d?( ?versus ?| ?to ?| ?vs\\.? ?| ?. ?| ?, ?)(0|1)?\\.\\d\\d?\\d?\\d?\\d?'
  pair_patterns = paste(pair_start, pair_end, collapse='|')
  aucs1_range = str_extract_all(for_auc, pattern=pair_patterns)[[1]]
  aucs1_range_split = NULL
  if(length(aucs1_range) > 0){
    aucs1_range = str_remove_all(aucs1_range, 'aur?o?c of|aur?o?c|:|c.?statistics?')
    for (r in 1:length(aucs1_range)){
      aucs1_range_split = str_split(aucs1_range, pattern = ' ?to ?| ?versus ?| ?vs\\.? ?| ?- ?| ?, ?')[[r]]
      aucs1_range_split = aucs1_range_split[aucs1_range_split!='']
      aucs1_range_split = data.frame(type = 'pair', auc = aucs1_range_split) # assuming these are all of type = mean
      #print(aucs1_range_split)
    }
    # now remove first finds so they don't get repeated
    for_auc = str_remove_all(for_auc, pattern=pair_patterns)
  }
  if(length(aucs1_range_split) == 0){aucs1_range_split = NULL}
  
  ## as above without range
  aucs1 = str_extract_all(for_auc, pattern=sentence_pattern_aucs)[[1]]
  if(length(aucs1) > 0){
    aucs1 = data.frame(type = 'mean', auc = aucs1) # assuming these are all of type = mean
    # now remove first finds so they don't get repeated
    for_auc = str_remove_all(for_auc, pattern='\\baur?o?cs?\\b.?.?.?.?.?.?0?\\.\\d\\d?\\d?\\d?\\d?')
  }
  if(length(aucs1) == 0){aucs1 = NULL}
    
  ## find AUC statistics based on next number in sentence
  aucs2 = NULL
  sentences = str_split(for_auc, pattern='\\. |\\.$')[[1]] # split into sentence
  sentences = sentences[sentences!='']
  index = str_detect(sentences, pattern = paste(auc.words, collapse='|'))
  #
  any_sens_spec_text = str_detect(sentences, pattern = sens_spec_short) #
  index[any_sens_spec_text] = FALSE # remove sentences with sensitivity/specificity
  
  if(sum(index)>0){ #
    sentences = sentences[index]
    sentences_with_numbers = str_detect(sentences, pattern='\\b(0|1)\\.\\d\\d?\\d?\\d?\\d?\\b| \\.\\d\\d?\\d?\\d?\\d?\\b') # can start "0.", "1." or just with dot
    sentences = sentences[sentences_with_numbers]
    auc = unlist(str_extract_all(sentences, pattern='\\b(0|1)\\.\\d\\d?\\d?\\d?\\d?\\b| \\.\\d\\d?\\d?\\d?\\d?\\b')) # can start "0.", "1." or just with dot
    if(length(auc) > 0){aucs2 = c(aucs2, auc)}
  }
  if(is.null(aucs2)==FALSE){aucs2 = data.frame(type = 'mean', auc = aucs2)} # assuming these are all of type = mean
  
  ## combine all sources of AUCs
  aucs = bind_rows(auc_numbers, auc_numbers2, aucs1_range_split, aucs1, aucs2) # combine two searches
  if(nrow(aucs) > 0){
    #
    aucs_char = filter(aucs, !is.na(auc))
    aucs = mutate(aucs_char, 
                  auc = str_remove_all(auc, '[^0-9|\\.]'), # remove text
             digits = str_count(str_remove(auc, '^0'), '[0-9]'), # count decimal places ...
             auc = as.numeric(auc)) # ... can now convert to number
    # find non-numeric results
    f = filter(aucs, is.na(auc))
    if(nrow(f)>0){
      cat('Warning, non-numeric AUC for', indata$pmid[k],'\n')
      print(aucs_char)
    }

  }
}

## return the results
aframe = NULL
if(is.null(aucs) == FALSE){
  if(nrow(aucs) >0){
    aframe = mutate(aucs,
                    pmid = indata$pmid[k]) %>%
      filter(auc >= 0,
             auc <= 1) # exclude AUCs outside 0 to 1
    if(nrow(aframe) == 0){aframe = NULL}
  }
}
tframe = data.frame(pmid = indata$pmid[k], 
          date = indata$date[k], 
          type = indata$type[k], 
          jabbrv = indata$jabbrv[k], 
          n.authors = indata$n.authors[k],
          country = indata$country[k],
          any_auc = any_auc,
          sample_size = sample_size,
          stringsAsFactors = FALSE)
if(nrow(tframe)!=1){cat('error, wrong number of rows', indata$pmid[k], '.\n', sep='')}

# return both
to.return = list()
to.return$aframe = aframe
to.return$tframe = tframe
return(to.return)

}

