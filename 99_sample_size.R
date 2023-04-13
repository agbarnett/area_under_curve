# 99_sample_size.R
# extract estimate of the sample size from abstract
# called by 99_main_function_abstract.R
# December 2022
library(stringr)

sample_size = str_extract_all(tolower(abstract), sample_size_patterns)[[1]]
if(length(sample_size) > 0){
  split = unlist(str_split(sample_size, pattern = '\\b')) # split into words
  split = split[!split %in% c('',' ')] # remove blanks
  any_chars = str_detect(str_remove_all(split, '[0-9]'),'[^0-9]') # need to remove numbers combined with characters, e.g., "t1dm"
  sample_sizes = str_remove_all(split, '[^0-9]')
  sample_sizes = sample_sizes[!any_chars]
  sample_sizes = as.numeric(sample_sizes)
  sample_sizes = unique(sample_sizes) # assume the same number is a duplicate mention - will not work for things like "10 cases and 10 controls"
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

