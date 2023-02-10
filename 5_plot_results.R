# 5_plot_results.R
# plot the main results
# January 2023
# not yet worked through, may be better as Rmarkdown
#
library(dplyr)
library(ggplot2)
library(stringr)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())

# get the data
load('data/analysis_ready.RData') # from 4_combine_results.R

### investigate 1's
# where are the most 1's
ones = mutate(results, one = auc==1) %>%
  group_by(source) %>%
  summarise(n = n(),
            ones = sum(one)) %>%
  mutate( p =ones/n) %>%
  ungroup() %>%
  arrange(p)

###

### rounded histogram
breaks = seq(0, 1.01, 0.01) # up to 1.01 to include 1
for_histo = filter(results, 
                   digits > 1,
                   !str_detect(type, 'diff|lower|upper')) %>%
  mutate(binned = cut(auc, breaks, right = FALSE), # from lower limit up to upper limit: [lower,upper)
         num = as.numeric(binned)) %>%
  group_by(binned, num) %>%
  tally() %>%
  ungroup()
# which to colour
colour = c(0.7,0.8,0.9)
colour_num = round(colour*100) + 1
for_histo = mutate(for_histo,
                   fill = ifelse(num %in% colour_num, 1, 2))
# x-axis
minor_breaks = seq(0.05,0.95,0.1)
breaks = c(0, seq(0.5,1,0.1))
n_breaks = round(breaks*100) + 1
n_minor_breaks = round(minor_breaks*100) + 1
# plot
rounded = ggplot(data = for_histo, aes(x=num, y=n, fill=factor(fill)))+
  geom_bar(stat='identity', col='grey22')+
  scale_fill_manual(NULL, values=c('dark red','pink','red'))+
  scale_x_continuous(breaks = n_breaks, 
                     minor_breaks = n_minor_breaks,
                     labels = breaks)+
  g.theme+
  xlab('AUC')+
  ylab('Frequency')+
  theme(legend.position = 'none')
rounded

jpeg('figures/histogram.jpg', width=5, height=4, units='in', res=500)
print(rounded)
dev.off()


# compare with expected
expected = filter(results, digits == 3,
           auc > 0.5) %>%
  arrange(auc) %>%
  mutate(n = 1:n()/n(),
         n = 0.5 + n/2, # from [0,1] to [0.5 to 1] - need to round
         diff = auc - n)
#
dplot = ggplot(data = expected, aes(x = n, y = diff)) +
  geom_point()+
  geom_smooth()+
  scale_x_continuous(breaks = seq(0.5, 1, 0.1))+
  g.theme+
  coord_cartesian(xlim=c(0.5,1))
dplot


# largest per abstract
largest = group_by(results, pmid) %>%
  filter(type == 'mean') %>%
  arrange(pmid, desc(auc)) %>%
  slice(1) %>% # largest per abstract
  ungroup()
cplot = ggplot(data = largest, aes(x = auc, col = type)) +
  stat_ecdf(geom = "step") +
#  facet_wrap(~digits)+
  ylab('Cumulative proportion')+
  xlab('AUC')+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  g.theme+
  theme(legend.position = 'none')+
  coord_cartesian(xlim=c(0.5,1))
cplot
jpeg('figures/cumulative_largest.jpg', width=5, height=4, units='in', res=500)
print(cplot)
dev.off()


# abstracts with just one AUC?
# just plot unique AUCs, sometimes repeated, e.g. 33737912
# try lower CI

#### 2 decimal places ####
## histogram at every possible result, has to be grouped by decimal place
acounts = filter(results, 
           type == 'mean',
           digits %in% 2) %>% # best digits to show
  mutate(auc = as.character(auc)) %>% # temporary convert to character so that digits work
  group_by(auc) %>%
  tally() %>%
  ungroup() 
# fill in missing digits
fill = data.frame(digits = 2, auc = as.character(seq(0,1,0.01)))
to_plot = left_join(fill, acounts, by=c('auc')) %>%
  mutate(auc = as.numeric(auc),
    auc = ifelse(is.na(auc), 0, auc), # replace missing with zero
    threshold = as.numeric(auc %in% c(0.75, 0.8, 0.9, 0.95)),
    threhsold = factor(threshold, levels=0:1, labels=c('No','Yes')))
                           

#
hplot2 = ggplot(data = to_plot, aes(x=auc, y=n, ymin=0, ymax=n, col=factor(threshold)))+
  geom_linerange()+
  scale_color_manual(NULL, values=c('skyblue','dark red'))+
  scale_x_continuous(breaks=c(0,0.05,0.5,0.75,0.8,0.9,0.95,1))+
  g.theme+
  #coord_cartesian(xlim=c(0.5,1))+
  theme(legend.position = 'none')
hplot2
jpeg('figures/AUC_dp2.jpg', width=5, height=4, units='in', res=500, quality=100)
print(hplot2)
dev.off()


#### 3 decimal places ####
## histogram at every possible result, has to be grouped by decimal place
acounts = filter(results, 
                 type == 'mean',
                 digits == 3) %>% # best digits to show
  mutate(auc = as.character(auc)) %>% # temporary convert to character so that digits work
  group_by(auc) %>%
  tally() %>%
  ungroup() 
# fill in missing digits
fill = data.frame(digits = 3, auc = as.character(seq(0,1,0.001)))
to_plot = left_join(fill, acounts, by=c('auc')) %>%
  mutate(auc = as.numeric(auc),
         auc = ifelse(is.na(auc), 0, auc), # replace missing with zero
         threshold = as.numeric(auc %in% c(0.750, 0.800, 0.900, 0.950, 1)),
         threhsold = factor(threshold, levels=0:1, labels=c('No','Yes')))


#
smooth_plot = ggplot(data = to_plot, aes(x=auc, y=n, ymin=0, ymax=n))+
  geom_point()+
  geom_smooth(method = 'loess', span = 0.1)+
  scale_color_manual(NULL, values=c('skyblue','dark red'))+
  scale_x_continuous(breaks=c(0,0.05,0.5,0.75,0.8,0.85,0.9,0.95,1))+
  g.theme+
  theme(legend.position = 'none')+
  coord_cartesian(xlim=c(0.5,1))
smooth_plot

hplot3 = ggplot(data = to_plot, aes(x=auc, y=n, ymin=0, ymax=n, col=factor(threshold)))+
#  geom_point()+
  geom_linerange()+
  scale_color_manual(NULL, values=c('skyblue','dark red'))+
  scale_x_continuous(breaks=c(0,0.05,0.5,0.75,0.8,0.85,0.9,0.95,1))+
  g.theme+
  theme(legend.position = 'none')+
  coord_cartesian(xlim=c(0.5,1))
hplot3
jpeg('figures/AUC_dp3.jpg', width=5, height=4, units='in', res=500, quality=100)
print(hplot3)
dev.off()

# focus on specific sections
f1 = filter(to_plot, auc >= 0.88, auc<=0.92)
f1 = filter(to_plot, auc >= 0.78, auc<=0.82)
hplot_f1 = ggplot(data = f1, aes(x=auc, y=n, ymin=0, ymax=n, col=factor(threshold)))+
  #  geom_point()+
  geom_linerange()+
  scale_color_manual(NULL, values=c('skyblue','dark red'))+
#  scale_x_continuous(breaks=c(0,0.05,0.5,0.75,0.8,0.9,0.95,1))+
  g.theme+
  theme(legend.position = 'none')
hplot_f1

# plot trend over time in CI width (to do)

# run checks on results that are 0.95 and 1
rmarkdown::render('99_random_checks.Rmd', 
                  output_file = '99_random_checks',
                  output_format = 'word_document',
                  params = in_params,
                  envir = new.env(parent = globalenv()))
                  
