# 99_functions.R
# useful functions for AUC work
# February 2023

## get the largest difference in CI width, assume order is mean, lower, upper
make_diff = function(numbers){
  diff = rep(NA, 2)
  diff[1] = numbers[3] - numbers[1] # upper minus mean
  diff[2] = numbers[1] - numbers[2] # mean minus lower
  max = as.character(max(diff)) # must be a character for merging with other numbers
  return(max)
}

## nrow that returns 0 for null
nrow0 = function(x){
  n = nrow(x)
  if(is.null(n)){
    n = length(x)
  }
  return(n)
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

## remove other statistics from the same sentence, used by 99_auc_confidence_intervals.R
remove_other_stats = function(in_sentences){
  # vector of patterns, including AUC (desired) and others (not desired)
  stats_pattern = c('auc|auroc|area under curve', # included because we need to count these too to work out their placement
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
  with_respect = str_detect(in_sentences, 'respectively')
  if(any(with_respect) == TRUE){
    other_sentences = in_sentences[!with_respect]
    in_sentences = in_sentences[with_respect]
    for(s in 1:sum(with_respect)){ # loop through sentences that have 'respectively'
      to_fix = in_sentences[s]
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

## function to group AUCs into bins
my_bin = function(x, digits){
  mult = 10^digits
  #y = floor(x*mult)/mult # works as [lower, upper)
  y = ceiling(x*mult)/mult # works as (lower, upper], better because of 1
  return(y)
}

## function to create histograms
make_histogram = function(indata,
                          df = 4, # degrees of freedom for smooth
                          upper_limit = 0 # upper limit for y-axis
){
### create the data for the rounded histogram
uncounted = filter(indata, 
                   !(type %in% c('Difference','Lower','Upper'))) 
one_digit = filter(uncounted, 
                   auc %in% seq(0,0.9,0.1),
                   digits == 1) %>% # count use of single digits
  nrow()
one_digit_percent = 100*one_digit/nrow(uncounted)
uncounted = filter(uncounted, digits > 1) %>% # avoid spikes due to rounding
  mutate(binned = my_bin(auc, digits = 2)) # from lower limit up to upper limit: (lower,upper]
for_histo = group_by(uncounted, binned) %>%
  tally() %>%
  ungroup() %>%
  mutate(fill = ifelse(binned %in% colour, 1, 2))

# smooth Poisson model
model = glm(n ~ ns(binned, df = df), data = for_histo, family=poisson())
ci = add_ci(for_histo, model, names = c("lower", "upper"), alpha = 0.05) %>%
  mutate(group = 1, # to avoid separate dotted lines by fill
         residual = n - pred) %>% 
  arrange(binned)

# histogram
rounded = ggplot(data = ci, aes(x = binned, y = n, fill = factor(fill), group = group))+
  geom_bar(stat='identity', col='grey22', size=0.1)+ # size for thinner lines around bars
  geom_line(data = ci, aes(x=binned, y=pred, group = group), lty=2)+ # smoothed
  #geom_line(data = ci, aes(x=binned, y=lower, group = group), lty=2)+ # CI for smooth
  #geom_line(data = ci, aes(x=binned, y=upper, group = group), lty=2)+ # CI for smooth
  scale_fill_manual(NULL, values=c('dark red','pink','red'))+
  scale_x_continuous(breaks = x.breaks + (0.01)/2, # nudge breaks to top of bar
                     labels = x.breaks,
                     expand=c(0.01,0))+
  theme_bw()+ # keep minor grid lines
  scale_y_continuous(expand = c(0, 0), # reduce space to x-axis ...
                     limits = c(0,upper_limit))+ # ... had to use upper limit
  xlab('AUC')+
  ylab('Frequency')+
  coord_cartesian(xlim=c(lower_auc_plot, NA))+ # focus on near 0.5 and above
  theme(legend.position = 'none')

# residuals
residual = ggplot(data = ci, aes(x = binned, y = residual, fill = factor(fill)))+
  geom_bar(stat='identity', col='grey22', size=0.1)+
  scale_fill_manual(NULL, values=c('dark red','pink','red'))+
  scale_x_continuous(breaks = x.breaks + (0.01)/2, # nudge breaks to top of bar
                     labels = x.breaks,
                     expand=c(0.01,0))+
  theme_bw()+ # keep minor grid lines
  xlab('AUC')+
  ylab('Residual (observed - expected)')+
  coord_cartesian(xlim=c(lower_auc_plot, NA))+ # focus on near 0.5 and above
  theme(legend.position = 'none')
residual

#
counts = NULL
for (bin in unique(for_histo$binned)){
  just_pmid = filter(uncounted, binned == bin) %>%
    select(pmid) %>%
    unique()
  all_for_pmid = left_join(just_pmid, uncounted, by='pmid') %>%
    filter(binned != bin) %>%
    group_by(pmid) %>%
    summarise(n = n(), mean = mean(binned)) %>% # by PMID
    ungroup() %>%
    summarise(meann = mean(n), meana = mean(mean)) %>% # over average per bin
    mutate(binned = bin)
  counts = bind_rows(counts, all_for_pmid)
}
counts = filter(counts, binned > 0.47)

# return
to_return = list()
to_return$histogram = rounded
to_return$residual = residual
to_return$one_digit = one_digit
to_return$one_digit_percent = one_digit_percent
to_return$df = df
to_return$counts = counts
return(to_return)

} # end of function
