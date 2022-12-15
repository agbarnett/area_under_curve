# 1_patterns.R
# patterns and key constants, part copied from  ../outside.confidence.intervals/1_find_intervals.R
# November 2022

# number of words to search before and after confidence interval; used 10 in Barnett and Wren paper
words.to.search = 10 

## splitting patterns
# \p{Pd} is any type of hyphen
what.to.split = "\\p{Pd}|\\/|-|,|=|\\%|,|;|:| |~|\\)|\\(|\\]|\\[|\\{|\\}" # whole load of characters for what to split numbers on 

## Area under curve patterns, using dots to allow for hyphens; different words for AUC; AUROCs from 28813448
auc.words = c(
  '\\bauacs?\\b',
  '\\baurocc?s?\\b',
  '\\baucrocc?s?\\b',
  '\\baucc?s?\\b',
  '\\brocc?s?\\b',
  '\\bareas?.under.the.curves?\\b',
  '\\bareas?.under.curves?\\b',
  '\\breceiver.operat[a-z]*\\b',
  '\\bc.ind(ex|ices)\\b',
  '\\bc.statistics?\\b',
  '\\bc.ind[a-z]*\\b' # c-index, c-indices
)
auc.words = auc.words[order(-nchar(auc.words))] # long to short
auc.pattern = paste(auc.words, collapse='|')

## patterns for AUC in sentences
auc_phrases  = c('\\baur?o?c\\b.?.?.?.?.?.?(0|1)?\\.\\d\\d?\\d?\\d?\\d?',
             '(0|1)?\\.\\d\\d?\\d?\\d?\\d? in t?h?e? ?training',
             '(0|1)?\\.\\d\\d?\\d?\\d?\\d? in t?h?e? ?test',
             '(0|1)?\\.\\d\\d?\\d?\\d?\\d? in t?h?e? ?validation',
             '(0|1)?\\.\\d\\d?\\d?\\d?\\d? in t?h?e? ?external')
sentence_pattern_aucs = paste(auc_phrases, collapse='|') 

## phrases that look like AUC but are other types of AUC (dots to allow hyphens)
to_remove = c('area.under.the.plasma.concentration.time.curve',
              'plasma.concentration.time.curve',
              'incremental.area.under.the.curve',
              'area.under.the.plasma.concentration.versus.time.curve',
              '\\bpk\\b', # acronym PK
              'pharmacokinetic',
              'pharmacology',
              '\\bc.?max\\b',
              'area.under.the.time.concentration.curve',
              'score.area.under.the.curve',
              'pain.relief.curve',
              'time.fev1.curve',
              'fev1.auc',
              'peptide.curve',
              'vas.auc')
to_remove = to_remove[order(-nchar(to_remove))] # long to short
to_remove = paste(to_remove, collapse="|")


## statistics to remove so they don't get confused with AUC; including differences in AUCs
stats = c('p.?valu?e?s?','\\bp',
          '\\bchi','chi.?squared?',
          '\\bt.?test','\\bz.?test','\\bf.?test',
          'p for all',
          'odds ratios?','rate ratios?','relative risks?',
          'kappa.?s?',
          '\\bauc change',
          '\\bauc improvement',
          '\\bauc difference',
          '\\bauc delta',
          'differences? in auc',
          'changes? in auc',
          'improvements? in auc',
          'accurac[a-z]*\\b', 
          'correlat[a-z]*\\b',
          'specificit[a-z]*\\b',
          'sensitiv[a-z]*\\b',
          'cut.?offs?',
          'youden',
          '\\bspec\\b', # abbreviations
          '\\bsens\\b')
operations = c('(&gt;|>|≥|&lt;|<|≤|=|less than|greater than|equals)')
numbers = c('(0|1)?\\.[0-9]*\\b', '(0|1)?\\.[0-9]*\\b ?.95% ?ci \\d\\.[0-9]*\\b.?.?.?.?\\d\\.[0-9]*\\b', '1') # with confidence intervals
# create all combinations
one = apply(expand.grid(stats, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
statistics_patterns = paste(combs, collapse='|')

## plus/minus; some symbols look the same but are different
operators = c('±','±','\u00B1','\u2213','\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
numbers = c('\\d?\\.[0-9]*\\b', '1') 
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

## specificity / sensitivity and other related statistics;
words = c('correlat[a-z]*\\b',
          'specificit[a-z]*\\b',
          'sensitiv[a-z]*\\b',
          'predictive value',
          'youden.?s?.?j?',
          'youden.?s? ind[a-z]*\\b',
          '\\bbrier',
          '\\bf1s?\\b',
          'Δ.aur?o?c', # difference in AUC
          'δ.aur?o?c',
          'delta.aur?o?c',
          'difference in aur?o?c')
operations = c('( |.? ?, ?|.? ?: ?|.? ?= ?| were |equal[a-z]*\\b )')
numbers = c('(0|1)?\\.[0-9]*\\b, (0|1)?\\.[0-9]*\\b and (0|1)?\\.[0-9]*\\b', 
            '(0|1)?\\.[0-9]*\\b and (0|1)?\\.[0-9]*\\b', 
            '(0|1)?\\.[0-9]*\\b, (0|1)?\\.[0-9]*\\b', 
            '100\\.?[0-9]?[0-9]?\\%', 
            '[0-9][0-9]?\\.?[0-9]?[0-9]?\\%?\\b', 
            '(0|1)?\\.[0-9]*\\b', 
            '1\\.?0?0?\\b') # including longer lists, eg., "0.6, 0.7 and 0.8";  sensitivity and specificity can be on percent scale
one = apply(expand.grid(words, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
#combs = combs[order(-nchar(combs))] # long to short - do not use; used sensible ordering above instead
sens_spec_patterns = paste(combs, collapse='|')
# without numbers
sens_spec_short = '\\bsensitiv|\\bspecific|\\bcorrelation|\\byouden|\\bbrier|\\bf1s?\\b'

## thresholds for p-values, e.g., 30447463
operators = c('>','greater than','&gt;')
numbers = as.character(seq(0.5,0.95,0.05)) # round thresholds
numbers = str_replace(numbers, '\\.', '\\\\.')  
combs = apply(expand.grid(operators, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
threshold_patterns = paste(combs, collapse='|')

## Correlations
words = c('correlations? .','\\brhos?','spearman.?s? .','kendall.?s? .')
operations = c('(&lt;|<|=|less than|equals|greater than|>)')
numbers = c('0?\\.[0-9][0-9]?[0-9]?[0-9]?', '1')
# create all combinations
one = apply(expand.grid(words, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
correlation_patterns = paste(combs, collapse='|')

## sample sizes
# a) words before number
words = c('\\bn\\b','\\btotal\\b')
operators = c(' ','=','equals?','of')
numbers = c('\\d\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\b','\\b\\d?\\d\\d,\\d\\d\\d\\b|\\b\\d?\\d,\\d\\d\\d,\\d\\d\\d\\b') # must end with break; add commas to numbers for large sample sizes
one = apply(expand.grid(words, operators), 1, paste, collapse=" ?", sep='')
combs_before = apply(expand.grid(one, numbers), 1, paste, collapse=" ?", sep='')
# b) words after number
sample_size_words = c('\\bpatients\\b',
                      '\\bsubjects\\b',
                      '\\bparticipants\\b',
                      '\\bindividuals\\b',
                      '\\bpersons\\b',
                      '\\bsamples\\b',
                      '\\brespondents\\b',
                      '\\bchildren\\b',
                      '\\bmen\\b',
                      '\\bwomen\\b',
                      '\\bgirls\\b',
                      '\\bboys\\b',
                      '\\bneonates\\b',
                      '\\bcases\\b',
                      '\\bcontrols\\b',
                      '\\bmice\\b')
operators = c('')
one = apply(expand.grid(sample_size_words, operators), 1, paste, collapse="", sep='')
combs_after = apply(expand.grid(numbers, one), 1, paste, collapse=" ?", sep='')
# c) words from part b with any spacing word, e.g., 'diseased patients', 'eligible patients'
# to do, could add two words, e.g., "100 breast cancer patients"
sample_size_words_plus = paste('[\\w]+ ', sample_size_words, sep='')
operators = c('')
one = apply(expand.grid(sample_size_words_plus, operators), 1, paste, collapse="", sep='')
combs_after_plus = apply(expand.grid(numbers, one), 1, paste, collapse=" ?", sep='')
#
combs = c(combs_before, combs_after, combs_after_plus)
combs = combs[order(-nchar(combs))] # long to short
sample_size_patterns = paste(combs, collapse='|')
