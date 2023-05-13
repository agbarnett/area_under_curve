# 5_investigate_57_validate.R
# investigating the excess of results at 0.57 using the random samples for the validation
# look for differences in software
# April 2023
library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(broom) # for confidence intervals

# Read in data that include methods
validation_results = NULL
for (auc in c(0.57, 0.58)){
  rex = read_excel('validate/check_Rex.xlsx', sheet=as.character(auc)) %>%
    mutate(person = 'Rex',
           auc = auc) %>%
    filter(correct == TRUE) # do not want to include AUCs that were not correct
  nicole = read_excel('validate/check_Nicole.xlsx', sheet=as.character(auc)) %>%
    mutate(person = 'Nicole',
           auc = auc) %>%
    filter(correct == TRUE) # do not want to include AUCs that were not correct
  adrian = read_excel('validate/check_Adrian.xlsx', sheet=as.character(auc)) %>%
    mutate(person = 'Adrian',
           auc = auc) %>%
    filter(correct == TRUE) # do not want to include AUCs that were not correct
  validation_results = bind_rows(validation_results, rex, nicole, adrian) 
}
#
table(validation_results$person)
# remove accidental duplicates
validation_results = group_by(validation_results, link, auc) %>%
  slice(1) %>%
  ungroup()
  
# minor data cleaning
validation_results = select(validation_results, -correct) %>%
  mutate(
    methods = str_replace_all(methods, '\n', ' '), # carriage returns
    methods = str_squish(methods), # remove any odd spaces
    nchar = nchar(methods),
    words = str_count(methods, '\\w+'),
    methods = tolower(methods)) %>% # case does not matter
  filter(!is.na(methods)) %>% # remove handful with no methods
  select(-contains('comments'))

## list of things to compare between methods sections
# minitab, desctools, prroc, did not appear
# popular r packages: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# good and excellent - those people discussing thresholds
to_compare = read.table(header=TRUE, sep=',', text="
label,search
excel,'\\bexcel\\b|\\bxl.?stat\\b'
prism,'\\bgraph.?pad\\b'
matlab,'\\bmatlab\\b'
medcalc,'\\bmedcalc\\b'
stata,'\\bstata[0-9]?[0-9]?\\b|\\bstatacorp\\b'
spss,'\\bspss\\b'
python,'\\bpython\\b'
sas,'\\bsas institute\\b|\\bsas[0-9]\\.[0-9]\\b|\\bsas\\b'
r,'\\br\\b|\\br.?studio'
mlwin,'\\bmlwin'
statview,'\\bstatview\\b'
bayes,'\\bbayes\\b|\\bbayesian\\b'
range,'\\brange\\b|\\branging\\b|\\bbetween\\b'
threshold,'\\bgreater than\\b|\\bless than\\b|\\bexceeding\\b|\\bthreshold\\b'
stat_sig,'statistical significance|statistically significant|\\b.05\\b|\\b0.05\\b'
adjust,'\\badjusted|\\badjusts?\\b'
c_index,'\\bc.index(.value|es)?|\\bharrells?\\b'
proc_package,'\\bproc\\b'
rocr_package,'\\brocr\\b'
good,'\\bgood\\b|\\bexcellent\\b'
non_parametric,'\\bnon.?parametric\\b'
machine_learning,'\\bmachine.learning\\b|\\bai\\b|\\bartificial.intelligence\\b|\\bneural.networks?\\b'
multivariate,'\\bmultivariate\\b'
")

# loop through searches
freq_results = diff_results = NULL
for (k in 1:nrow(to_compare)){
  this_result = mutate(validation_results,
                       positive = str_detect(methods, to_compare$search[k]))
  # extra step for r (phrase that is not the r software)
  if(to_compare$label[k] == 'r'){
    this_result = mutate(this_result,
                         not_r = '\\br.wave') %>%
      mutate(positive = ifelse(not_r == TRUE, FALSE, positive))
  }
  # counts 
  tab = group_by(this_result, auc) %>%
    summarise(n = n(),
              r = sum(positive)) %>%
    ungroup() %>%
    arrange(auc) %>%
    mutate(search = to_compare$label[k],
           p = r/n)
  # CI for difference using prop.test
  ptest = prop.test(tab$r, tab$n)
  frame = data.frame(search = to_compare$label[k], 
                     p_57 = tab$p[1],
                     p_58 = tab$p[2],
                     diff = diff(ptest$estimate), lower = ptest$conf.int[1], upper = ptest$conf.int[2], pvalue=ptest$p.value)
  row.names(frame) = NULL
  # concatenate
  freq_results = bind_rows(freq_results, tab)
  diff_results = bind_rows(diff_results, frame)
}
# look for big differences
arrange(diff_results, abs(diff))

## compare no software 
# long version
programs = c('stata','spss','sas','r','prism','python','excel','matlab','medcalc','mlwin','statview')
software = mutate(validation_results,
 excel = str_detect(methods, '\\bexcel\\b|\\bxl.?stat\\b'),
 prism = str_detect(methods, '\\bgraph.?pad\\b'),
 matlab = str_detect(methods, '\\bmatlab\\b'),
 mlwin = str_detect(methods, '\\bmlwin\\b'),
 medcalc = str_detect(methods, '\\bmedcalc\\b'),
 python = str_detect(methods, '\\bpython\\b'),
 stata = str_detect(methods, '\\bstata[0-9]?[0-9]?\\b|\\bstatacorp\\b'),
 statview = str_detect(methods, '\\bstatview\\b'),
 spss = str_detect(methods, '\\bspss\\b'),
 sas = str_detect(methods, '\\bsas\\b|\\bsas institute\\b'),
 r = str_detect(methods, '\\br\\b|\\br.?studio'),
 not_r = str_detect(methods, '\\br.wave')) %>%
 mutate(r = ifelse(not_r == TRUE, FALSE, r))
#
long = select(software, link, auc, all_of(programs)) %>%
   pivot_longer(cols=all_of(programs))
# frequency table
group_by(long, name) %>%
  summarise(N = n(),
            r = sum(value),
            p = round(100*r/N)) %>%
  arrange(r)
# counts of software
counts = group_by(long, link) %>%
  summarise(n = sum(value))
# papers with none (add to software)
none = filter(counts, n == 0) %>%
  mutate(none = TRUE)
software = full_join(software, none, by='link') %>%
  mutate(none = ifelse(is.na(none), FALSE, none))
# did any papers have multiple software, answer = yes
doubles = filter(counts, n > 1) %>%
  left_join(long, by='link') %>%
  filter(value == TRUE)
cat('There were ', length(unique(doubles$link)), ' papers that used 2 or more software packages.\n', sep='')
# table of no software by AUC
tabyl(dat = software, none, auc) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front")

## compare length of methods sections
# a) characters
model1 = glm(nchar ~ I(auc==0.58), data = validation_results)
summary(model1)
tidy(model1, conf.int = TRUE)
# b) words
model2 = glm(words ~ I(auc==0.58), data = validation_results)
summary(model2)
tidy(model2, conf.int = TRUE)
#
ggplot(data = validation_results, aes(x=factor(auc), y=words))+
  geom_boxplot()+
  scale_y_log10()
#
group_by(validation_results, auc) %>%
  summarise(median(words))

## test difference in decimal places for 0.57 vs 0.58
load('data/analysis_ready.RData')
# just for validation results
test_decimals = select(validation_results, -auc) %>%
                mutate(pmid = str_remove_all(link, 'https://pubmed.ncbi.nlm.nih.gov/|/'),
                       pmid = as.numeric(pmid)) %>%
  left_join(results, by='pmid') %>%
  filter(auc %in% c(0.57, 0.58))
#
group_by(test_decimals, auc) %>%
  summarise(n = n(),
            dp = mean(digits))

# difference in algorithm?
group_by(test_decimals, auc, source) %>%
  tally() %>%
  arrange(source, auc)
  