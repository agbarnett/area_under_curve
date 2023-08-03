# 3_verify_in_out.R
# verify the algorithm's ability to find AUC values
# random selections made in 99_random_checks.Rmd
# July 2023
library(dplyr)
library(readxl) # for reading in Excel
library(stringr)
library(binom) # for confidence interval

## part 1: read in results checked by hand
infile = 'validate/with_without.xlsx'
sheet_1 = read_excel(infile, sheet = 1) %>%
  mutate(type = 'With')
sheet_2 = read_excel(infile, sheet = 2) %>%
  mutate(type = 'Without')
data = bind_rows(sheet_1, sheet_2)

# quick check for repeats, should all be 1:
table(table(data$pmid)) 

# errors by type, using exact binomial intervals
stats = group_by(data, type) %>%
  summarise(n = n(),
            r = sum(correct)) %>%
  mutate(mean = binom.exact(x = r, n = n, conf.level = 0.95)$mean,
         lower = binom.exact(x = r, n = n, conf.level = 0.95)$lower,
         upper = binom.exact(x = r, n = n, conf.level = 0.95)$upper)
stats

