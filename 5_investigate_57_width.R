# 5_investigate_57_width.R
# investigating the excess of results at 0.57 using the random samples for the validation
# look for differences in observed confidence interval width
# April 2023
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
library(dplyr)
library(stringr)
library(janitor)
library(splines)
source('99_functions.R')

# what would rounding look like?
# example result: Mean = 0.6, Lower = 0.5, Upper = 0.7,
# wrongly round upward -> so Mean = 0.61 (but assume intervals are left unchanged)
# means upper difference will be smaller

# get the data
load('data/analysis_ready.RData') # from 4_combine_results.R
remove(abstracts, excluded) # not needed

# calculate intervals differences
differences = filter(results, type=='Difference') %>%
  rename('Mean' = 'lower', # fix up previous error
         'Lower' = 'mean') %>%
  mutate(Mean = ifelse(Mean>1, Mean/100, Mean), # adjust percents
         Lower = ifelse(Lower>1, Lower/100, Lower), 
         upper  = ifelse(upper >1, upper /100, upper ), 
         ldiff = Mean - Lower, # difference from lower limit to mean
         udiff = upper - Mean,
         fdiff = upper - Lower, # full difference
         asymmetric = udiff - ldiff, # so if positive then upper difference is larger
         binned = my_bin(Mean, digits = 2),
         category = case_when(
           abs(udiff - ldiff) < 0.0001 ~ 'Symmetric intervals',
           udiff - ldiff >= 0.0001 ~ 'Upper interval larger',
           ldiff - udiff >= 0.0001 ~ 'Lower interval larger',
         ))

# difference by binned Mean
group = group_by(differences, binned) %>%
  summarise(mean = mean(asymmetric)) %>%
  ungroup() %>%
  filter(binned >= 0.5) # remove noisy area below 0.5

# fit smooth line
smooth = lm(mean ~ ns(binned,3), data = group)
group = mutate(group, 
               pred = fitted(smooth),
               res = resid(smooth))
 
# plot residuals
rplot = ggplot(data = group, aes(x = binned, y= res))+
  geom_hline(yintercept= 0 , lty=2, col='dark blue')+
  geom_point()+
  geom_line()+
  g.theme
rplot

## plot original results with residuals
# labels
label1 = data.frame(binned = 1, mean=0, label='Larger upper limit')
label2 = data.frame(binned = 1, mean=0, label='Larger lower limit')
gplot = ggplot(data = group, aes(x = binned, y= mean))+
  geom_hline(yintercept= 0 , lty=2, col='dark blue')+
  geom_line(data = group, aes(x = binned, y=pred), lty=2, col='dark red')+
  geom_text(data = label1, aes(x = binned, y=mean, label=label), nudge_y = 0.001, adj=1)+
  geom_text(data = label2, aes(x = binned, y=mean, label=label), nudge_y = -0.001, adj=1)+
  geom_point()+
  geom_line()+
  g.theme
gplot


### second analysis, plot overall CI width by mean
group = group_by(differences, binned) %>%
  summarise(mean = mean(fdiff)) %>%
  ungroup() %>%
  filter(binned >= 0.5) # remove noisy area below 0.5
# plot
fplot = ggplot(data = group, aes(x = binned, y= mean))+
  geom_point()+
  geom_line()+
  g.theme
fplot


## categories by binned Mean
# which AUC bars to colour
colour = c(0.7, 0.8, 0.9)
group_cat = group_by(differences, binned, category) %>%
  tally() %>%
  group_by(binned) %>%
  mutate(p = prop.table(n),
         group = 1,
         fill = ifelse(binned %in% colour, 1, 2)) %>%
  ungroup() %>%
  filter(binned >= 0.5) # remove noisy area below 0.5
x.breaks = c(0, seq(0.5,1,0.1))
cplot = ggplot(data = group_cat, aes(x = binned, y= n, fill=category, group=group))+
  geom_bar(stat='identity', position='stack')+
  scale_fill_manual('Confidence\ninterval', values=c("darkgoldenrod3", "gray52", "hotpink4"))+
  xlab('Mean AUC')+
  ylab('Count')+
  scale_x_continuous(breaks = x.breaks + (0.01)/2, # nudge breaks to top of bar
                     labels = x.breaks,
                     expand=c(0,0))+
  g.theme+
  theme(legend.position = 'none')+
  facet_wrap(~category, scales='free')
cplot
#
jpeg('figures/rounding_numbers.jpg', width=6, height=4, units='in', res=400, quality=100)
print(cplot)
dev.off()
# 
x.breaks = c(0, 0.55, seq(0.5,1,0.1)) # plus 0.55
pplot = ggplot(data = group_cat, aes(x = binned, y= p, fill=category))+
  geom_bar(col='transparent', stat='identity',position='stack', size=0.1)+
  scale_fill_manual('Confidence\ninterval', values=c("darkgoldenrod3", "gray52", "hotpink4"))+
  scale_x_continuous(breaks = x.breaks + (0.01)/2, # nudge breaks to top of bar
                     labels = x.breaks,
                     expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_vline(xintercept=seq(0.5,1,0.1)+ (0.01)/2)+
  xlab('Mean AUC')+
  ylab('Proportion')+
  g.theme+
  theme( panel.spacing = unit(0, "lines"),
         panel.spacing.x = unit(0,"lines"))
pplot
jpeg('figures/rounding_proportion.jpg', width=6, height=4, units='in', res=500, quality=100)
print(pplot)
dev.off()

## alternative plot in columns and using a similar colour scheme
#
# plot
cplotc = ggplot(data = group_cat, aes(x = binned, y= n, fill=fill, group=group))+
  geom_bar(stat='identity', col='grey22', size=0.1)+
  #scale_fill_manual(NULL, values=c('dark blue','light blue'))+
  xlab('Mean AUC')+
  ylab('Frequency')+
  scale_x_continuous(breaks = x.breaks + (0.01)/2, # nudge breaks to top of bar
                     labels = x.breaks,
                     expand=c(0,0))+
  g.theme+
  theme(legend.position = 'none')+
  facet_wrap(~category, ncol=1)
cplotc
#
jpeg('figures/rounding_numbers_column.jpg', width=4, height=6, units='in', res=500, quality=100)
print(cplotc)
dev.off()

#
f = filter(differences, category=='Upper interval larger', binned == 0.82)
