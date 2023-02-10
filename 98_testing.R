# 98_testing.R
# testing pattern matches
# January 2023

source('1_patterns.R')


text = c('0.555, auc of 0.11, auc of 0.99; auc (0.33), auc equal to 1. auc = 0, auc=0.5')
str_extract_all(text, pattern = auc_number)

#
test_function = function(text, pattern){
  result = str_extract_all(text, pattern)
  cbind(text, as.character(result))
}
#test_function(text = c('1.0','1','0.11','2.22', '10% fox', '101.11','.xx',' .80','0.001', 'apples','crab2','33','0.22x'), auc_number)
#test_function(text = c('1.0','1','0.11','2.22', '10% fox', '101.11','.xx',' .80','0.001', 'apples','crab2','33','0.22x'), general_number_percent)


pattern = "false positive rate( \\(fpr\\))? ?((of )?&gt;|(of )?>|(of )?≥|(of )?&lt;|(of )?<|(of )?≤|=|(of )?less than|(of )?greater than|equals?( to)?|of|was|is|) ?\\b([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]?|[0-9]?[0-9]?[0-9]?[0-9])(,|;|\\(|\\)|\\%| |$|\\. |\\.$)" 
pattern = "false positive rate( \\(fpr\\))? ?((of )?&gt;|(of )?>|(of )?≥|(of )?&lt;|(of )?<|(of )?≤|=|(of )?less than|(of )?greater than|equals?( to)?|of|was|is|) ?\\b[0-9]?[0-9]?[0-9]?[0-9](,|;|\\(|\\)|\\.|\\%| |$|\\. )" 

text = tolower('Area under the ROC curve (AUC) and pAUC (partial AUC) at false positive rate (FPR) = 0.20. ')
str_extract(text, pattern)


#
general_number_percent_no_start_end = '([0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?[0-9]?\\%|[0-9]?[0-9]?[0-9]?[0-9]\\%)'
general_number = paste(boundary_no_dot_start, general_number_percent_no_start_end, boundary_no_dot_end, sep='')
general_number_percent = paste(boundary_no_dot_start, general_number_percent_no_start_end, boundary_no_dot_end, sep='')

find_pattern3 = paste(rep('auc', each = length(link_words)), link_words, sep = " ") # all combinations of AUC and phrases
find_pattern3 = paste('aucs? were ', general_number_percent_no_start_end, ', ', general_number_percent_no_start_end, ',? and ', general_number_percent_no_start_end, sep='')
text = tolower('The random forest was the best model to predict recurrence in HGEC; the AUCs were 85.2%, 74.1%, and 71.8% in the training, validation')
str_extract_all(text, find_pattern3)


####
text = tolower('The ensemble learning could improve performance with the sensitivity and F1 score of 0.77 (0.81-0.83) and 0.83 (0.77-0.89) with an AUPRC of 0.904.')
stat = "\\bf1s?( score|statistic)?\\b"
operations = c('((of )?&gt;|(of )?>|(of )?≥|(of )?&lt;|(of )?<|(of )?≤|=|(of )?less than|(of )?greater than|equals?( to)?|of|was|is|)') # include `|` for no operator
double_ci = paste(general_number_no_start_end, ' (\\(|\\[)', general_number_no_start_end, ' ?(to|-) ?', general_number_no_start_end, '(\\)|\\]) (and|or) ',
                  general_number_no_start_end, ' (\\(|\\[)', general_number_no_start_end, ' ?(to|-) ?', general_number_no_start_end, '(\\)|\\])', sep='') # two Cis
pattern = paste(stat, operations, double_ci, collapse= ' ?')
str_extract_all(text, pattern)
str_extract_all(text, statistics_patterns)

## plus/minus
test = c('10 +/- 1', '10 ± 0.22', '10 ± 22.22, apple', '10 ± 1.')
test_function(test, plus_minus_patterns)

## correlations
test = c('0.88','correlation 0.2222','correlation = 0.9. ','correlation > 0.7','rho=.1','rho = 0.x','rho=0.444','kendall: 0.22', 'rho = 1, kappa=0.2')
test_function(test, correlation_patterns)

## statistics patterns
test = c('brier score = 0.4','hopeless 2 text','chi-squared = 0.22','odds ratio')
test_function(test, statistics_patterns)


# working on thresholds like 'was > .05', added ' |' to start of general number
boundary_no_dot_start = "(?<=(,|;|:|=|\\(|^| |\\. ))" # look behind, so symbols do not get included
general_number_no_start_end = '(( |[0-9][0-9]?[0-9]?[0-9]?)\\.[0-9][0-9]?[0-9]?[0-9]?|[0-9]?[0-9]?[0-9]?[0-9])'

operators = c('>','≥','greater than','&gt;','<','≤','less than','&lt;')
numbers = paste(general_number_no_start_end, boundary_no_dot_end, sep='')
combs = apply(expand.grid(operators, numbers), 1, paste, collapse=" ?")
combs = combs[order(-nchar(combs))] # long to short
threshold_patterns = paste(combs, collapse='|')


## thresholds
test = c('all p values > .05','p < 0.05','p<0.05','p< 0.10, plus','p = 0.05','adrian > tony', '0.44,')
test_function(test, threshold_patterns)

# removing + with look ahead - decided to remove all + instead
test = c('specificity+ (92%)','difference +0.08, 95% CI')

# difficult case
test = c("with a sensitivity and for men and .83 and .81 for women, respectively" ,
         " was inversely associated with time to first major cardiac event: the hazard ratio per mL/min/1.73 m² was -0.0000015 [95% CI -0.00000078; -0.0000024] ")
boundary_no_dot_start = "(?<=(,|;|:|=|\\(|\\[|^| |\\. ))" # look behind, so symbols do not get included
boundary_no_dot_end = "(?=(,|;|:|\\)|\\]|\\%| |$|\\. |\\.$))" # look ahead
#
auc_number_no_start_end = '(-?\\.[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?|-?0\\.[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?|1\\.0?0?0?|1)'
pattern = paste(boundary_no_dot_start, auc_number_no_start_end, boundary_no_dot_end, sep='')
test_function(test, pattern)

# for long lists
#want ", " or "and" or ",and"
test = 'specificities of 87.0%, 83.3%, and 61.8%, 64.7%'
pattern = c(', ?|,? ?and ?')
test_function(test, statistics_patterns)

# additional p-value stuff
test = tolower('Men displaying the highest postprandial AUC TRL-TG were also characterized by the greatest accumulation of visceral adipose tissue (AT) (P<.05). ')
additional = 'p(.?value)? ?(<|>|=) ?\\.0(0001|000|001|01|1|5)' # number without leading 0
test_function(test, additional)

# years
test = tolower('Additionally, the CRP/Alb showed greater AUC values at 1 year (0.692), 3 years (0.659), and 5 years (0.682) than GPS, mGPS and PNI.')
pattern = '\\b[0-9][0-9]?.years?|\\byears?.[0-9][0-9]?'
test_function(test, pattern)

#
test = c('at 1 week','day 0','1-hour max')
test_function(test, time_pattern )
