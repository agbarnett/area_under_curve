# 1_other_patterns.R
# other patterns and key constants, copied from  ../outside.confidence.intervals/1_find_intervals.R
# October 2022
# \p{Pd} is any type of hyphen
words.to.search = 10 # number of words to search before and after confidence interval; used 10 in Barnett and Wren paper
what.to.split = "\\p{Pd}|-|,|=|\\%|,|;|:| |~|\\)|\\(|\\]|\\[|\\{|\\}" # whole load of characters for what to split numbers on 

## Area under curve patterns
auc.words = c('AUCs?','area under the curves?','area under curves?','area under the receiver','area under the receiver','AUROCs?') # different words for AUC; AUROCs from 28813448
auc.words = unique(tolower(auc.words))
auc.pattern = make.combs(auc.words)

## statistics to remove so they don't get confused with AUC
stats = c('p.?valu?e?s?','p','chi','chi.?squared?','t.test','z.test','f.test','p for all')
operations = c('(&gt;|>|&lt;|<|=|less than|greater than|equals)')
numbers = c('0?\\.[0-9]', '1')
# create all combinations
one = apply(expand.grid(stats, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
statistics_patterns = paste(combs, collapse='|')

## plus/minus; some symbols look the same but are different
operators = c('±','±','\u00B1','\u2213','\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
numbers = c('0?\\.[0-9]', '1')
combs = apply(expand.grid(operators, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
plus_minus_patterns = paste(combs, collapse='|')

## standard errors
words = c('\\bse\\b','\\bsem\\b','standard error')
operations = c('(&gt;|>|&lt;|<|=|less than|greater than|equals)')
numbers = c('0?\\.[0-9]')
one = apply(expand.grid(words, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
se_patterns = paste(combs, collapse='|')

## standard errors
words = c('specificity','sensitivity')
operations = c('( |, ?|: ?)')
numbers = c('0?\\.[0-9]')
one = apply(expand.grid(words, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
sens_spec_patterns = paste(combs, collapse='|')


## thresholds, e.g., 30447463
operators = c('>','greater than','&gt;')
numbers = as.character(seq(0.5,0.95,0.05)) # round thresholds
numbers = str_replace(numbers, '\\.', '\\\\.')  
combs = apply(expand.grid(operators, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
threshold_patterns = paste(combs, collapse='|')

## Correlations
words = c('correlations? .','rhos?','spearman.?s? .','kendall.?s? .')
operations = c('(&lt;|<|=|less than|equals|greater than|>)')
numbers = c('0?\\.[0-9][0-9]?[0-9]?[0-9]?', '1')
# create all combinations
one = apply(expand.grid(words, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
correlation_patterns = paste(combs, collapse='|')

## sample sizes, add commas - to do
# a) words before number
words = c('\\bn\\b','\\btotal\\b')
operators = c(' ','=','equals?')
numbers = c('\\d\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\b') # must end with break
one = apply(expand.grid(words, operators), 1, paste, collapse=" ?")
combs_before = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
# b) words before number
words = c('\\bpatients\\b','\\bparticipants\\b','\\bsamples\\b','\\brespondents\\b','\\bchildren\\b','\\bneonates\\b')
operators = c(' ')
one = apply(expand.grid(words, operators), 1, paste, collapse=" ?")
combs_after = apply(expand.grid(numbers, one), 1, paste, collapse=" ?")
#
combs = c(combs_before, combs_after)
combs = combs[order(-nchar(combs))] # long to short
sample_size_patterns = paste(combs, collapse='|')
