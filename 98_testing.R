# 98_testing.R
# testing pattern matches
# december 2022

boundary_no_dot_start = "(?<=(,|;|:|=|\\(|^| |\\. ))" # look behind, so symbols do not get included
boundary_no_dot_end = "(?=(,|;|:|\\)|\\%| |$|\\. |\\.$))" # look ahead
#
auc_number = paste(boundary_no_dot_start, '((0| )\\.[0-9][0-9]?[0-9]?[0-9]?|1\\.0?0?0?|1)', boundary_no_dot_end, sep='')

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
