# 5_investigate_57_simulate_lower.R
# investigating the excess of results at 0.57 using the random samples for the validation
# using a simulation model, with re-analysis for lower confidence intervals that hit 0.55
# May 2023
library(dplyr)
library(pROC) # for AUC
source('99_functions.R') # for my_bins

# key parameters
n_sim = 5000 # number of simulations
n_lower = 30 # sample size per group (lower limit)
n_upper = 300 # sample size per group (upper limit)
sd.error = 0.4
slope = 0.15 # strength of association with group (keep weak) - for binary
#slope = 0.05 # strength of association with group (keep weak) - for categorical

## a) run through simulation based on leave-one-out hack
simresults = NULL
for (k in 1:n_sim){
  n = round(runif(1, min = n_lower, max = n_upper)) # random sample size
  simdata = data.frame(error = rnorm(n*2, mean = 0, sd = sd.error)) %>%
    mutate(i = 1:n(),
           group = as.numeric(i>n), # binary predictor
           group = as.numeric(i/n > 0.5) + as.numeric(i/n > 1)+ as.numeric(i/n > 1.5), # categorical predictor
           logit = error + slope*group,
           disease = as.numeric(logit > 0))
  # run AUC
  roc = roc(response = simdata$disease, 
            predictor = simdata$group, 
            smooth = FALSE,
            direction='auto', 
            quiet=TRUE)
  ci = ci.auc(roc, method='delong')
  ci = round(as.numeric(ci),2)
  # is lower ci equal to 0.55?
  if( abs(ci[1] - 0.55)> 0.001){next} # ignore this simulation, and re-create data
  #cat('Success\n')
  original = ci
  # run the re-analysis until a better lower limit is found
  better = FALSE; hack_count = 0; i = 1
  while(better==FALSE){
    hackdata = simdata[-i, ] # knock out one patient at a time
    roc_hack = roc(response = hackdata$disease, 
                   predictor = hackdata$group, 
                   smooth = FALSE,
                   direction='auto', 
                   quiet=TRUE)
    ci_hack = ci.auc(roc_hack, method='delong')
    ci_hack = round(as.numeric(ci_hack),2)
    hack_count = hack_count + 1
    i = i + 1 # move on to next patient
    if(ci_hack[1] > 0.55){better=TRUE}
    if(i > n){ # not hackable even after going through every patient, so move to next
      ci_hack = rep(NA, 3)
      better = TRUE # not really! But gets out of loop
    } 
  }
  # 
  frame = suppressMessages(bind_cols(t(original), t(ci_hack)))
  names(frame) = c('lower_original','mean_original','upper_original','lower_hack','mean_hack','upper_hack')
  frame = mutate(frame, 
                 sim = k, 
                 n = n, 
                 hack_count = hack_count)
  simresults = bind_rows(simresults, frame)
}


## b) run through simulation based on adjustment hack
simresults = NULL
for (k in 1:n_sim){
  n = round(runif(1, min = n_lower, max = n_upper)) # random sample size
  simdata = data.frame(error = rnorm(n*2, mean = 0, sd = sd.error)) %>%
    mutate(i = 1:n(),
           group = as.numeric(i>n), # binary predictor
           group = as.numeric(i/n > 0.5) + as.numeric(i/n > 1)+ as.numeric(i/n > 1.5), # categorical predictor
           logit = error + slope*group,
           disease = as.numeric(logit > 0))
  # use model predictions
  model = glm(disease ~ group, data= simdata, family=binomial())
  simdata = mutate(simdata, predictor = fitted(model))
  # run AUC
  roc = roc(response = simdata$disease, 
            predictor = simdata$predictor, 
            smooth = FALSE,
            direction='auto', 
            quiet=TRUE)
  ci = ci.auc(roc, method='delong')
  ci = round(as.numeric(ci),2)
  # is lower ci equal to 0.55?
  if( abs(ci[1] - 0.55)> 0.001){next} # ignore this simulation, and re-create data
  original = ci
  # run the re-analysis until a better lower limit is found
  better = FALSE; hack_count = 0; i = 1
  while(better==FALSE){
    # add variable to predictions
    hackdata = mutate(hackdata, random = rnorm(n()))
    model = glm(disease ~ group + random, data= hackdata, family=binomial())
    # 
    hackdata = mutate(hackdata, predictor = fitted(model))
    roc_hack = roc(response = hackdata$disease, 
                   predictor = hackdata$predictor, 
                   smooth = FALSE,
                   direction='auto', 
                   quiet=TRUE)
    ci_hack = ci.auc(roc_hack, method='delong')
    ci_hack = round(as.numeric(ci_hack),2)
    hack_count = hack_count + 1
    i = i + 1 # move on to next patient
    if(ci_hack[1] > 0.55){better=TRUE}
    if(i > n){ # not hackable even after this many noisy variables, so move to next
      ci_hack = rep(NA, 3)
      better = TRUE # not really! But gets out of loop
    } 
  }
  # 
  frame = suppressMessages(bind_cols(t(original), t(ci_hack)))
  names(frame) = c('lower_original','mean_original','upper_original','lower_hack','mean_hack','upper_hack')
  frame = mutate(frame, 
                 sim = k, 
                 n = n, 
                 hack_count = hack_count)
  simresults = bind_rows(simresults, frame)
}
barplot(table(simresults$lower_hack))
