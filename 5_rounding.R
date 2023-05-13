# 5_rounding.R
# plot decimal places by mean
# April 2023
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(dplyr)
source('99_functions.R')

# get the data
load('data/analysis_ready.RData') # from 4_combine_results.R

# get the average digits by bin
to_plot = mutate(results, binned = my_bin(auc, digits = 2)) %>%
  group_by(binned) %>%
  summarise(digits = mean(digits)) %>%
  ungroup() %>%
  filter(binned >= 0.5)

# plot
dplot = ggplot(data = to_plot, aes(x=binned, y=digits))+
  geom_line(size=1.05, col='dodgerblue')+
  geom_point(col='dodgerblue', size=2)+
  ylab('Mean number of digits')+
  xlab('AUC')+
  g.theme
dplot
jpeg('figures/mean_digits_by_auc.jpg', width=6, height=4, units='in', res=500, quality=100)
print(dplot)
dev.off()

# look at 0.55 to 0.56 in detail
to_plot2 = mutate(results, binned = my_bin(auc, digits = 3)) %>%
  group_by(binned) %>%
  filter(binned >= 0.550, binned<=0.570) %>%
  tally() %>%
  ungroup() 

# plot
dplot2 = ggplot(data = to_plot2, aes(x=binned, y=n))+
  geom_bar(stat='identity')+
  g.theme
dplot2


