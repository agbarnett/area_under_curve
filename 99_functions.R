# 99_functions.R
# useful functions for AUC work
# December 2022

## nrow that returns 0 for null
nrow0 = function(x){
  min(c(nrow(x),length(x)))
}

## copied from ../outside.confidence.intervals/1_find_intervals.R ##
# a) function used later to remove numbers after the word "per" as in '10 per 10,000' ...
# ... also removes two numbers after range or IQR
remove.text = function(intext, w, remove.range){
  if(length(w)>0){
    to.remove = NULL
    for(s in 1:remove.range){
      to.remove = c(to.remove, w + s)
    }
    intext = intext[1:length(intext) %in% to.remove ==F] # remove after per
  }
  return(intext)
}
# test.text = c('range','1','2','3','iqr','4','5','6','per','10000','9','','range','7','8') # text to test function below
remove.per = function(intext){
  intext = intext[intext!='']
  w = which(intext=='per')
  intext = remove.text(intext, w=w, remove.range=1)
  w = which(intext=='range')
  intext = remove.text(intext, w=w, remove.range=2)
  w = which(intext=='iqr')
  intext = remove.text(intext, w=w, remove.range=2)
  return(intext)
}

# b) function to make word-search combinations
make.combs = function(words){
  # different ways the key-words can appear in the text
  text.patterns = c('\\[x\\]','\\(x\\)','-x ',' x ',' x,',' x=',' x:',' x\\.','\\(x ','\\(x:','\\[x ','\\[x:','\\bx\\b') 
  # re-order words from long to short phrases (helps with substrings after searching)
  order = order(-nchar(words))
  words = words[order]
  #
  pattern = NULL
  for (w in words){
    single = str_replace_all(text.patterns, pattern='x', replacement = w)
    plural = str_replace_all(text.patterns, pattern='x', replacement = paste(w, 's', sep='')) # add 's' to make plural version
    pattern = c(pattern, single, plural)
  }
  pattern = paste(pattern, sep='', collapse='|')
  return(pattern)
}

## remove commas in numbers, from baseline_tables repository
remove_commas = function(in_text){
  in_text = gsub(",(?=\\d{3,})", "", in_text, perl = TRUE) # strip commas in numbers
  # following line not working, is changing "1.000" to "1000"
 # in_text = gsub("(?<=\\d{1})\\S(?=000)", '', in_text, perl=TRUE) # strip spaces (including special spaces) in thousands (space before 000). \S is any space
  return(in_text)
}

## remove other statistics from the same sentence
remove_other_stats = function(in_sentences){
  # vector of patterns, including AUC (desired) and others (not desired)
  stats_pattern = c('auc|auroc|area under curve',
  '\\bsens?\\b', # odd acronym used by 33739040
  '\\bspec?s?\\b',
  'sensitiv[a-z]*\\b',
  'specific[a-z]*\\b',
  'correlat[a-z]*\\b',
  'predictive value',
  'brier',
  'hosmer',
  '\\bf1\\b')
  # sentences with respectively
  with_respect = str_detect(tolower(in_sentences), 'respectively')
  if(any(with_respect) == TRUE){
    other_sentences = in_sentences[!with_respect]
    in_sentences = in_sentences[with_respect]
    for(s in 1:sum(with_respect)){ # loop through sentences that have 'respectively'
      to_fix = tolower(in_sentences[s])
      # get order of statistics
      order = str_locate(to_fix, stats_pattern)
      stat_order = rank(order[,1])
      not_auc = stat_order != 1
      # count the number of other statistics
      stat_count = sum(str_count(to_fix, stats_pattern[-1])) # without AUC patterns
      if(sum(stat_count) == 0){next} # no other statistics to remove
      # get numbers
      split = str_split(to_fix, '(,|\\))| and')[[1]] # split list of stats based on commas and 'and'
      splits_with_numbers = str_detect(split, '(0|1|)\\.\\d')
      to_blank = which(splits_with_numbers)[not_auc] # drop first, which is AUC
      update = split[1:length(split) %in% to_blank == FALSE] # remove non AUC numbers from sentence
      in_sentences[s] = paste(update, collapse=' ')
    }
    in_sentences = c(other_sentences, in_sentences)
  }
  # split by numbers
  return(in_sentences)
}
