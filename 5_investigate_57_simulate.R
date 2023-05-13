# 5_investigate_57_simulate.R
# investigating the excess of results at 0.57 using the random samples for the validation
# using a simulation
# April 2023
library(dplyr)
library(pROC) # for AUC
source('99_functions.R') # for my_bins

# key parameters
n_sim = 5000 # number of simulations
n = 200 # sample size per group
sd.error = 0.4
slope = 0.15 # strength of association with group (keep weak) - for binary
slope = 0.05 # strength of association with group (keep weak) - for categorical

#
simresults = NULL
for (k in 1:n_sim){
  simdata = data.frame(error = rnorm(n*2, mean = 0, sd = sd.error)) %>%
    mutate(i = 1:n(),
           group = as.numeric(i>n), # binary predictor
           group = as.numeric(i/n > 0.5) + as.numeric(i/n > 1)+ as.numeric(i/n > 1.5), # categorical predictor
           logit = error + slope*group,
           disease = as.numeric(logit > 0))
  #with(simdata, table(disease, group))
  # run AUC
  roc = roc(response = simdata$disease, 
            predictor = simdata$group, 
            smooth = FALSE,
            direction='auto', 
            quiet=TRUE)
  ci = ci.auc(roc, method='delong')
  ci = as.numeric(ci)
  # 
  frame = data.frame(sim = k, auc = ci[2], lower = ci[1], upper = ci[3])
  simresults = bind_rows(simresults, frame)
}

# create bins
simresults = mutate(simresults, 
                    auc = my_bin(auc, digits = 2),
                    lower = my_bin(lower, digits = 2),
                    upper = my_bin(upper, digits = 2))

# quick plot
barplot(table(simresults$auc))
barplot(table(simresults$lower))
barplot(table(simresults$upper))
