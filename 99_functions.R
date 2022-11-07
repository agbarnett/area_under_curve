# 99_functions.R
# useful functions
# October 2022


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
