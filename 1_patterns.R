# 1_patterns.R
# patterns and key constants, part copied from  ../outside.confidence.intervals/1_find_intervals.R
# see also 1_confidence_interval_pattern.R
# December 2022

# number of words to search before and after confidence interval; used 10 in Barnett and Wren paper
words.to.search = 10 

## splitting patterns
# \p{Pd} is any type of hyphen
what.to.split = "\\p{Pd}|\\/|-|,|=|\\%|,|;|:| |~|\\)|\\(|\\]|\\[|\\{|\\}" # whole load of characters for what to split numbers on 

## what is an AUC number, can start blank or zero. up to 4 decimal places, then a break; can also be 1; ( |0) allows for '0.6' and ' .6'
# special ending for numbers; needed because decimal place gets confused with full-stop, last two characters aims to capture full stop
boundary_no_dot_start = "(?<=(,|;|:|=|\\(|\\[|^| |\\. ))" # look behind, so symbols do not get included
boundary_no_dot_end = "(?=(,|;|:|\\)|\\]|\\%| |$|\\. |\\.$))" # look ahead
#
auc_number_no_start_end = '((0| )\\.[0-9][0-9]?[0-9]?[0-9]?|1\\.0?0?0?|1)'
auc_number = paste(boundary_no_dot_start, auc_number_no_start_end, boundary_no_dot_end, sep='') # version with start and end

# general numbers; including version with percent; and versions without start/end
general_number_no_start_end = '([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]?|[0-9]?[0-9]?[0-9]?[0-9])'
general_number_percent_no_start_end = '([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]? ?\\%|[0-9]?[0-9]?[0-9]?[0-9] ?\\%)' # with optional space before %
general_number = paste(boundary_no_dot_start, general_number_no_start_end, boundary_no_dot_end, sep='')
general_number_percent = paste(boundary_no_dot_start, general_number_percent_no_start_end, boundary_no_dot_end, sep='')
# see 98_testing.R

# version with confidence intervals
ci_auc_number = paste(boundary_no_dot_start, auc_number_no_start_end, " ?(\\(|\\[|,|:|)? ?(9(0|5|9)\\% confidence interval( ?.ci.)?|9(0|5|9)\\%.?ci)? ?(\\(|\\[)?", auc_number_no_start_end, ".?.?.?.?", auc_number_no_start_end, '(\\. |\\)|\\]|,|;|$)', sep='') # used `.ci.` to cover '(ci)' and '[ci]'
ci_auc_number_percent = paste(boundary_no_dot_start, general_number_percent_no_start_end, " ?(\\(|\\[|,|:|)? ?(9(0|5|9)\\% confidence interval( ?.ci.)?|9(0|5|9)\\%.?ci)? ?(\\(|\\[)?", general_number_percent_no_start_end, ".?.?.?.?", general_number_percent_no_start_end, '(\\. |\\)|\\]|,|;|$)', sep='') # 
ci_auc_number_percent2 = paste(boundary_no_dot_start, general_number_percent_no_start_end, " ?(\\(|\\[|,|:|)? ?(9(0|5|9)\\% confidence interval( ?.ci.)?|9(0|5|9)\\%.?ci)? ?(\\(|\\[)?", general_number_no_start_end, ".?.?.?.?", general_number_percent_no_start_end, '(\\. |\\)|\\]|,|;|$)', sep='') # v2, where middle number is not a percent

## Area under curve patterns, using dots to allow for hyphens; different words for AUC; AUROCs from 28813448
auc.words.no.breaks = c(
  'auacs?',
  'aurocc?s?',
  'auprc',
  'aucrocc?s?',
  'aucc?s?',
  'rocc?s?',
  'auc.rocs?',
  'aucs?.\\(areas?.under.the.curve\\)',
  'auc.values.\\(the.area.under.the.roc.curve\\)',
  'areas?.under.the.precision.recall.curves?',
  'areas?.under.the.curves?( (\\(|\\[)aucs?(\\)|\\]))?', # with optional acronym
  'areas?.under.curves?( (\\(|\\[)aucs?(\\)|\\]))?',
  'areas?.under.the.roc.curves?( (\\(|\\[)aurocs?(\\)|\\]))?',
  'receiver.operat[a-z]*',
  'receiver.operating.characteristic( (\\(|\\[)rocs??(\\)|\\]))?',
  'receiver.operating.characteristic.curves?( (\\(|\\[)rocs??(\\)|\\]))?',
  'c.index',
  'c.indices',
  'c.statistics?'
)
# now add breaks
auc.words.no.breaks = auc.words.no.breaks[order(-nchar(auc.words.no.breaks))] # long to short
auc.words = paste('\\b', auc.words.no.breaks, '\\b', sep='')
auc.pattern = paste(auc.words, collapse='|')

## patterns for AUC in sentences
# words which can come straight after AUC-statistic:
extra_words = '(statistics? |values? )?' # question after bracket end to include no extra words
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
               ", ",
               "=",
               '') # added blank to cover when there's no link, e.g., 32620590
link_words_extra = paste(extra_words, link_words, sep='')
opener = paste(rep(auc.words.no.breaks, each = length(link_words_extra)), link_words_extra, sep = ".") # all combinations of AUC and phrases; use sep of dot to allow 'auc-value' etc
# broke into multiple because it so large
# could add "and ranged between" 35027588
numbers = list()
numbers[[1]] = paste(auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 7
numbers[[2]] = paste(auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 6
numbers[[3]] = paste(auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 5
numbers[[4]] = paste(auc_number_no_start_end, ', ?', auc_number_no_start_end, ', ?', auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 4
numbers[[5]] = paste(auc_number_no_start_end, ', ?', auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 3
numbers[[6]] = paste(auc_number_no_start_end, ',? ?and ?', auc_number_no_start_end, sep='') # lists of 2
numbers[[7]] = paste(auc_number_no_start_end, ' (\\(|\\[)', auc_number_no_start_end, ' ?(to|-) ?', auc_number_no_start_end, '(\\)|\\]),? (and|or) ',
                     auc_number_no_start_end, ' (\\(|\\[)', auc_number_no_start_end, ' ?(to|-) ?', auc_number_no_start_end, '(\\)|\\])', sep='') # two Cis
numbers[[8]] = auc_number_no_start_end # alone
sentence_pattern_aucs = list()
for (ind in 1:8){
  combs = apply(expand.grid(opener, numbers[[ind]]), 1, paste, collapse=" ?")
  combs = combs[order(-nchar(combs))] # long to short
  combs = paste(combs, boundary_no_dot_end, sep='') # must have end
  sentence_pattern_aucs[[ind]] = paste(combs, collapse='|')
}


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
              'area.under.the.curve.in.range',
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
          'positive.predictive.values?( \\(ppv\\))?', # optional acronym 
          'negative.predictive.values?( \\(npv\\))?', 
          'positive.likelihood.ratios?( \\(plr\\))?', 
          'negative.likelihood.ratios?( \\(nlr\\))?',
          '\\bppv\\b', '\\bnpv\\b', '\\bplr\\b', '\\bnlr\\b',
          'false.positive.rate( \\(fpr\\))?',
          'false.negative.rate( \\(fpr\\))?',
          '\\bfpr\\b', '\\bnfr\\b',
          '\\bratios?',
          'hazard.ratios?( \\(hrs?\\))?',
          'diagnostic.odds.ratios?( \\(dors?\\))?',
          'odds.ratios?( \\(ors?\\))?',
          'rate.ratios?( \\(rrs?\\))?',
          'relative.risks?( \\(rrs?\\))?',
          'probability.ratios?( \\(prs?\\))?',
          '\\ba?hrs?\\b', '\\ba?ors?\\b', '\\bd?ors?\\b', '\\ba?rrs?\\b', '\\ba?prs?\\b', # just acronyms 'a' = adjusted
          'kappa.?s?',
          'prediction.error',
          'standard.error', # standard error / deviation
          'standard.deviation',
          'mean.absolute.error (\\(mae\\))?', # with optional algorithm
          '\\bse\\b',
          '\\bsem\\b',
          '\\bsd\\b',
          '\\bauc.change', # exclude changes in AUC statistics
          '\\bauc.improvement',
          '\\bauc.difference',
          '\\bauc.delta',
          'drop in auc',
          'rise in auc',
          'differences? in auc',
          'Δauc.?roc',
          'δauc.?roc',
          'Δauc',
          'δauc',
          'changes? in auc',
          'improvements? in auc',
          'threshold.probability',
          'accurac[a-z]*\\b', 
          'correlat[a-z]*\\b',
          'specificit[a-z]*\\b',
          'sensitiv[a-z]*\\b',
          'cut.?offs?',
          'optimism',
          'hosmer.lemeshow(.test)?(,? p ?(=|<))?',
          'youden(.s)?(.j.statistic)?(.index)?',
          'accuracy',
          '\\bbrier(.score|statistic)?',
          '\\bf1s?(.score|statistic)?\\b',
          '\\bspec\\b', # abbreviations
          '\\bsens\\b')
operations = c('((of )?&gt;|(of )?>|(of )?≥|(of )?&lt;|(of )?<|(of )?≤|=|(of )?less than|(of )?greater than|equals?( to)?|of|was|is|)') # include `|` for no operator
numbers = c(paste('(', general_number_no_start_end, ',? ?.?95\\% ?ci:? ', general_number_no_start_end, '.?.?.?.?', general_number_no_start_end, ')', sep=''), # CI
            paste('(', general_number_no_start_end, ', ?', general_number_no_start_end, ', ?', general_number_no_start_end, ' ?and ?', general_number_no_start_end, ')', sep=''), # lists
            paste('(', general_number_no_start_end, ', ?', general_number_no_start_end, ' ?and ?', general_number_no_start_end, ')', sep=''), # lists 
            paste('(', general_number_no_start_end, ' (\\(|\\[)', general_number_no_start_end, ' ?(to|-) ?', general_number_no_start_end, '(\\)|\\]) (and|or) ',
                  general_number_no_start_end, ' (\\(|\\[)', general_number_no_start_end, ' ?(to|-) ?', general_number_no_start_end, '(\\)|\\]))', sep=''), # two Cis
            paste('(', general_number_no_start_end, ' ?(and|to) ?', general_number_no_start_end, ')', sep=''), # and
            general_number_no_start_end,
            '1') 
# create all combinations
one = apply(expand.grid(stats, operations), 1, paste, collapse=" ?")
combs = apply(expand.grid(one, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
statistics_patterns = paste(combs, collapse='|')

# plus/minus
operators = c('±','±','\u00B1','\u2213','\\+[^:alnum:]–', '–[^:alnum:]\\+', '\\+[^:alnum:]-', '-[^:alnum:]\\+', 'plus[^:alnum:]minus', 'minus[^:alnum:]plus') # using any non alpha numeric character between plus and minus; minus signs are different
combs = paste(operators, general_number_no_start_end, sep =' ?')
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
                    'youden(.s)?(.j.statistic)?(.index)?',
                    '\\bbrier( score|statistic)?',
                    '\\bf1s?( score|statistic)?\\b',
                    'Δ.?aur?o?c', # difference in AUC - not wanted
                    'δ.?aur?o?c',
                    'delta.aur?o?c',
                    'difference in aur?o?c')
operations = c('( |.? ?, ?|.? ?: ?|.? ?= ?| was | were |equal[a-z]*\\b | of |)')
numbers = c(paste(general_number, ', ', general_number, ', ', general_number, ', ', general_number, ', ', general_number, ' and ', general_number, sep=''),
            paste(general_number, ', ', general_number, ', ', general_number, ', ', general_number, ' and ', general_number, sep=''),
            paste(general_number, ', ', general_number, ', ', general_number, ' and ', general_number, sep=''),
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

## thresholds for p-values and other numbers, e.g., 30447463
operators = c('>','≥','greater than','&gt;','<','≤','less than','&lt;')
numbers = paste(general_number_no_start_end, boundary_no_dot_end, sep='')
combs = apply(expand.grid(operators, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
threshold_patterns = paste(combs, collapse='|')

## Correlations
words = c('correlations?','\\brhos?','spearman.?s?','kendall.?s?') # tau?
operations = c('(&lt;|<|=|less than|equals|greater than|&gt;|>|:)?')
numbers = c('0?\\.[0-9][0-9]?[0-9]?[0-9]?[0-9]?', '1')
numbers = paste(numbers, boundary_no_dot_end, sep='')
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
sample_size_words = c('\\b(male|female )?patients\\b',
                      '\\b(male|female )?cancer.patients\\b',
                      '\\b(male|female )?subjects\\b',
                      '\\b(male|female )?participants\\b',
                      '\\bindividuals\\b',
                      '\\bpersons\\b',
                      '\\bsamples\\b',
                      '\\b(male|female )?respondents\\b',
                      '\\b(male|female )?children\\b',
                      '\\bmen\\b',
                      '\\bwomen\\b',
                      '\\bgirls\\b',
                      '\\bboys\\b',
                      '\\bneonates\\b',
                      '\\b(male|female )?cases\\b',
                      '\\b(male|female )?controls\\b',
                      '\\b(male|female )?rats\\b',
                      '\\b(male|female )?mice\\b')
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


## AUC as a percent
# make patterns to find matching phrases
find_pattern1 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern1 = paste(find_pattern1, general_number_percent_no_start_end, sep=' ?')
find_pattern1 = paste(find_pattern1, collapse = '|')
# version with `and` where first numbers is not a percent
find_pattern2a = paste(rep(auc.words.no.breaks[9], each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern2a = paste(find_pattern2a, general_number_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern2a = paste(find_pattern2a, collapse = '|')
# version with `and`
find_pattern2 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern2 = paste(find_pattern2, general_number_percent_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern2 = paste(find_pattern2, collapse = '|')
# version with comma and `and`
find_pattern3 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern3 = paste(find_pattern3, general_number_percent_no_start_end, ', ', general_number_percent_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern3 = paste(find_pattern3, collapse = '|')
# version with comma and `and`, with only last number as a percent
find_pattern3a = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
find_pattern3a = paste(find_pattern3a, general_number_no_start_end, ', ', general_number_no_start_end, ',? and ', general_number_percent_no_start_end, sep=' ?')
find_pattern3a = paste(find_pattern3a, collapse = '|')
# version like a confidence interval - not used
#find_pattern4 = paste(rep(auc.words.no.breaks, each = length(link_words)), link_words, sep = " ?") # all combinations of AUC and phrases
#find_pattern4 = paste(find_pattern4, general_number_no_start_end, ' (\\(\\])', general_number_no_start_end, ' ?. ?', general_number_percent_no_start_end, sep=' ?')
#find_pattern4 = paste(find_pattern4, collapse = '|')
#
find_pattern_percent = paste(c(find_pattern3a, find_pattern3, find_pattern2a, find_pattern2, find_pattern1), collapse='|')
