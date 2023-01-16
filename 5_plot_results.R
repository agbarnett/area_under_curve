# 4_plot_results.R
# plot the main results
# December 2022

# not yet worked through, may be better as Rmarkdown

#
library(ggplot2)

# plot cumulative density functions, stratified by decimal places
ggplot(aucs, aes(x = auc, col=type)) +
  stat_ecdf(geom = "step") +
  facet_wrap(~digits)+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
