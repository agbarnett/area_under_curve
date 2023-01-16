# 1_confidence_intervals_pattern.R
# code to find confidence intervals, copied from ../outside.confindence.intervals/1_find_intervals.R
# called by 1_find_auc.R
# October 2022

# Confidence interval patterns (with spaces and lower/upper cases)
# 11 is a dummy code for a missing number
levels = c(11,80,90,95,99) # currently common numbers, could replace with two integers [0-9][0-9]?
# ci and pi with space after (plus plurals) to avoid picking up words that start with these two letters
# typo 'uncertainly' from PMID:29171811
# 'uncertainty range' from PMID:28045934
words = c('\\bcis?','\\bcris?','\\bcls?','\\buis?','\\burs?','\\bpis?',
          'confidence intervals?',
          'confidence intervals? ?\\(cis?\\)',
          'confidence intervals? ?\\[cis?\\]',
          'posterior intervals?',
          'credibility intervals?', 
          'prediction intervals?',
          'ranges?',
          'uncertainly intervals?', 
          'uncertainty ranges?')
operators = c('', ':', ',', '=', '\\(') # added comma, e.g "AUC 0.88; 95% CI, 0.80-0.96"
# create all combinations
ci.phrases = apply(expand.grid(words, operators), 1, paste, collapse=" ?")
ci.phrases = ci.phrases[order(-nchar(ci.phrases))] # long to short
# re-order from long to short strings
ci.pattern.spaces = NULL
for (p in ci.phrases){ # 
  for (l in levels){ # 
    this.pattern = paste(l, '%', p, collapse = '', sep=' ') # space before and after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') #
    this.pattern = paste(l, '% ', p, collapse = ' ', sep='')  # space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
    this.pattern = paste(l, '%', p, collapse = ' ', sep='')  # no space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
  }
}
# add a few more (found in abstracts)
annals.patterns = c('\\(ci,', '\\(ci ', '\\[ci ', '\\[ci,',' ci ',' ci,',',ci ',', ci')
annals.patterns = paste(annals.patterns, collapse='|')
ci.pattern.spaces = paste(c(ci.pattern.spaces, annals.patterns), collapse='|') # for Annals
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'ci 95%', 'ci95%'), collapse='|') # reversed wording from 28228447 and 29176802
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95% cl'), collapse='|') # L instead of I from PMID:28665786 
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95%-ci'), collapse='|') # with dash from PMID:29155891 
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'ic 95%', 'ic95%', '95% ic'), collapse='|') # ! from PMID:28222175 and PMID:28245252
ci.pattern.spaces = paste(c('95 percent confidence interval', '95 percent ci'), ci.pattern.spaces , collapse='|') # from 2821396 and 20442430
