# 3_verify_algorithm_specific.R
# verify the algorithm using specific rounded AUC values
# random selections made in 99_random_checks.Rmd
# April 2023
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(dplyr)
library(readxl) # for reading in Excel
library(stringr)
library(binom) # for confidence interval

## part 1: read in results checked by hand
data = NULL
people = c('Adrian','Nicole','Rex') # three raters
sheets = c("0.57","0.58","0.7","0.8","0.81","1") # five sheets per file
for (person in people){
  infile = paste('validate/check_', person, '.xlsx', sep='')
  for (sheet in sheets){
    this_data = read_excel(infile, sheet = sheet) %>%
      mutate(rater = person,
           auc = sheet)
    data = bind_rows(data, this_data)
  }
}

# add whether errors are a threshold wording - not completed in text, so ignore for now
data = mutate(data,
              threshold = str_detect(tolower(comments), '^threshold'))

# quick check of differences between people (is there a harsh reviewer?)
group_by(data, rater) %>%
  summarise(n = n(),
            sum(correct))

# check that data is unique, whoops some repeats! check consistency
repeats = group_by(data, link, auc) %>%
  tally() %>%
  filter(n>1) %>%
  select(link, auc)
consistent = left_join(repeats, data, by=c('link','auc')) %>%
  group_by(link, auc) %>%
  summarise(sd = sd(correct))
# filter(consistent,sd>0)
# all SDs are zero, so raters were consistent

# remove duplicates
data = group_by(data, link, auc) %>%
  slice(1) %>%
  ungroup()

# errors by AUC level
stats = group_by(data, auc) %>%
  filter(!is.na(correct)) %>% # temporary
  summarise(n = n(),
            r = sum(correct)) %>%
  mutate(mean = binom.logit(x = r, n = n, conf.level = 0.95)$mean,
         lower = binom.logit(x = r, n = n, conf.level = 0.95)$lower,
         upper = binom.logit(x = r, n = n, conf.level = 0.95)$upper)
stats

# plot 
vplot = ggplot(data = stats, aes(x=factor(auc), y=mean, ymin=lower, ymax=upper))+
  geom_point(size=3, col='dodgerblue')+
  geom_errorbar(size=1.05, width=0, col='dodgerblue')+
  xlab('AUC')+
  ylab("Proportion correct")+
  scale_y_continuous(limits = c(NA, 1))+
  coord_flip()+
  g.theme
vplot

jpeg('figures/validate_specific.jpg', width=5, height=4, units='in', res=500)
print(vplot)
invisible(dev.off())
