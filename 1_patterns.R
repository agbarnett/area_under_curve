# 1_patterns.R
# patterns and key constants, part copied from  ../outside.confidence.intervals/1_find_intervals.R
# November 2022

# number of words to search before and after confidence interval; used 10 in Barnett and Wren paper
words.to.search = 10 

## splitting patterns
# \p{Pd} is any type of hyphen
what.to.split = "\\p{Pd}|\\/|-|,|=|\\%|,|;|:| |~|\\)|\\(|\\]|\\[|\\{|\\}" # whole load of characters for what to split numbers on 

## what is an AUC number, can start blank or zero. up to 4 decimal places, then a break; can also be 1; ( |0) allows for '0.6' and ' .6'
auc_number = '((0| )\\.[0-9][0-9]?[0-9]?[0-9]?\\b|\\b1\\.0?0?0?\\b|\\b1)\\b'
# version with confidence intervals
ci_auc_number = paste(auc_number, " ?(\\(|\\[|,|:|)?(9(0|5|9)\\% confidence interval|9(0|5|9)\\%.?ci)? ?", auc_number, "(.?.?.?.?)", auc_number, sep='')
# general numbers
boundary_no_dot = "(,|;|\\(|\\)|\\%| |$)"# needed because decimal place gets confused with full-stop
general_number = paste('\\b([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]?\\b|\\b[0-9]?[0-9]?[0-9]?[0-9])', boundary_no_dot, sep='')
# as above, but must be a percentage
general_number_percent = '\\b([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]?\\%|\\b[0-9]?[0-9]?[0-9]?[0-9]\\%)'
test_function = function(text, pattern){
  result = str_extract_all(text, pattern)
  cbind(text, as.character(result))
}
#test_function(text = c('1.0','1','0.11','2.22', '10% fox', '101.11','.xx',' .80','0.001', 'apples','crab2','33','0.22x'), auc_number)
#test_function(text = c('1.0','1','0.11','2.22', '10% fox', '101.11','.xx',' .80','0.001', 'apples','crab2','33','0.22x'), general_number_percent)

## Area under curve patterns, using dots to allow for hyphens; different words for AUC; AUROCs from 28813448
auc.words.no.breaks = c(
  'auacs?',
  'aurocc?s?',
  'auprc',
  'aucrocc?s?',
  'aucc?s?',
  'rocc?s?',
  'auc.roc',
  'areas?.under.the.precision.recall.curves?',
  'areas?.under.the.curves?( \\(aucs?\\))?', # with optional acronym
  'areas?.under.curves?( \\(aucs?\\))?',
  'receiver.operat[a-z]*',
  'c.index',
  'c.indices',
  'c.statistics?',
  'c.ind[a-z]*' # c-index, c-indices
)
# now add breaks
auc.words.no.breaks = auc.words.no.breaks[order(-nchar(auc.words.no.breaks))] # long to short
auc.words = paste('\\b', auc.words.no.breaks, '\\b', sep='')
auc.pattern = paste(auc.words, collapse='|')

## patterns for AUC in sentences
# link words/phrases
link_words = c("were determined as",
               "was determined as",
               "was equals? to",
               "is determined as",
               "were equal to",
               "equals? to",
               "equals?",
               "were",
               "was",
               "are",
               "to",
               "of",
               "=")
opener = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ") # all combinations of AUC and phrases
sentence_pattern_aucs = paste(opener, auc_number,  ' in t?h?e? ?(training|test|validation|external)', sep='')
sentence_pattern_aucs = paste(sentence_pattern_aucs, collapse = '|')


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
stats = c('p.?valu?e?s?',
          '\\bp\\b',
          '\\bchi','chi.?squared?',
          '\\bt.?statistic','\\bz.?statistic','\\bf.?statistic',
          '\\bt.?test','\\bz.?test','\\bf.?test',
          'p for all',
          'positive predictive values?( \\(ppv\\))?', # optional acronym 
          'negative predictive values?( \\(npv\\))?', 
          'positive likelihood ratios?( \\(plr\\))?', 
          'negative likelihood ratios?( \\(nlr\\))?',
          '\\bppv\\b', '\\bnpv\\b', '\\bplr\\b', '\\bnlr\\b',
          '\\bratios?',
          'hazard ratios?( \\(hrs?\\))?',
          'diagnostic odds ratios?( \\(dors?\\))?',
          'odds ratios?( \\(ors?\\))?',
          'rate ratios?( \\(rrs?\\))?',
          'relative risks?( \\(rrs?\\))?',
          'probability ratios?( \\(prs?\\))?',
          '\\ba?hrs?\\b', '\\ba?ors?\\b', '\\bd?ors?\\b', '\\ba?rrs?\\b', '\\ba?prs?\\b', # just acronyms 'a' = adjusted
          'kappa.?s?',
          'prediction error',
          'standard error', # standard error / deviation
          'standard deviation',
          'mean.absolute.error (\\(mae\\))?', # with optional algorithm
          '\\bse\\b',
          '\\bsem\\b',
          '\\bsd\\b',
          '\\bauc change', # exclude changes in AUC statistics
          '\\bauc improvement',
          '\\bauc difference',
          '\\bauc delta',
          'drop in auc',
          'rise in auc',
          'differences? in auc',
          'Δauc.?roc',
          'δauc.?roc',
          'Δauc',
          'δauc',
          'changes? in auc',
          'improvements? in auc',
          'threshold probability',
          'accurac[a-z]*\\b', 
          'correlat[a-z]*\\b',
          'specificit[a-z]*\\b',
          'sensitiv[a-z]*\\b',
          'cut.?offs?',
          'optimism',
          'youden',
          'accuracy',
          '\\bspec\\b', # abbreviations
          '\\bsens\\b')
operations = c('((of )?&gt;|(of )?>|(of )?≥|(of )?&lt;|(of )?<|(of )?≤|=|(of )?less than|(of )?greater than|equals?( to)?|of|was|is|)') # include `|` for no operator
numbers = c(paste('(', general_number, ',? ?.95% ?ci:? ', general_number, '.?.?.?.?', general_number, ')', sep=''), # CI
            paste('(', general_number, ', ', general_number, ', ', general_number, ' and ', general_number, ')', sep=''), # lists
            paste('(', general_number, ', ', general_number, ' and ', general_number, ')', sep=''), # lists 
            paste('(', general_number, ' (and|to) ', general_number, ')', sep=''), # lists 
            general_number, 
            '1') 
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

## specificity / sensitivity and other related statistics;
sens_spec_words = c('correlat[a-z]*\\b',
          'specificit[a-z]*\\b',
          'sensitiv[a-z]*\\b',
          'positive predictive values?( \\(ppv\\))?', # optional acronym 
          'negative predictive values?( \\(npv\\))?', 
          'positive likelihood ratios?( \\(plr\\))?', 
          'negative likelihood ratios?( \\(nlr\\))?',
          'diagnos(tic|is) odds ratios?( \\(dor\\))?',
          '\\bppv\\b', '\\bnpv\\b', '\\bplr\\b', '\\bnlr\\b', '\\bdor\\b', # acronyms
          'accuracy',
          'youden.?s?.?j?',
          'youden.?s? ind[a-z]*\\b',
          '\\bbrier( score|statistic)?',
          '\\bf1s?( score|statistic)?\\b',
          'Δ.?aur?o?c', # difference in AUC - not wanted
          'δ.?aur?o?c',
          'delta.aur?o?c',
          'difference in aur?o?c')
operations = c('( |.? ?, ?|.? ?: ?|.? ?= ?| was | were |equal[a-z]*\\b | of |)')
numbers = c(paste(general_number, ', ', general_number, ', ', general_number, ' and ', general_number, sep=''),
            paste(general_number, ', ', general_number, ' and ', general_number, sep=''),
            paste(general_number, ' and ', general_number, sep=''),
            paste(general_number, ', ', general_number, sep=''),
            general_number) # including longer lists, eg., "0.6, 0.7 and 0.8";  sensitivity and specificity can be on percent scale
one = apply(expand.grid(sens_spec_words, operations), 1, paste, collapse=" ?")
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
