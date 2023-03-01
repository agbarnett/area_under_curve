# 5_flow_chart.R
# CONSORT flow chart
# figure used by 5_summary.Rmd
# Feb 2023
library(diagram)
library(dplyr)

# abstracts screened
load('data/pmid_list.RData')
n_screened = nrow(pmid_list)

# numbers excluded
load('data/analysis_ready.RData')
excluded_totals = group_by(excluded, reason) %>%
  summarise(total = sum(n)) %>%
  ungroup()
#
empty = filter(excluded_totals, reason == 'Empty abstract') %>% pull(total)
not_english = filter(excluded_totals, reason == 'Not English') %>% pull(total)
pk = filter(excluded_totals, reason == 'PK') %>% pull(total)
meta = filter(excluded_totals, reason == 'Meta-analysis') %>% pull(total)

# remainder
n_remain = n_screened - sum(excluded_totals$total)

# labels, big N for institutions, little n for RIAs
l1 = paste('Number of abstracts\nscreened (N=',  format(n_screened, big.mark=','), ')', sep='') # 
l2 = paste('Excluded\nempty (N=', format(empty, big.mark=','), ')', sep='') # 
l3 = paste('Excluded\nnot in English (N=', format(not_english, big.mark=','), ')', sep='') # 
l4 = paste('Excluded\nPK study (N=', format(pk, big.mark=','), ')', sep='') # 
l5 = paste('Excluded\nmeta-analysis (N=', format(meta, big.mark=','), ')', sep='') # 
l6 = paste('Analysed\n(N=', format(n_remain, big.mark=','), ')', sep='') # 
null = ''
labels = c(l1, l2, l3, l4, l5, l6, null, null, null, null)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.34	0.135
2	0.75	0.8	white	square	0.34	0.15
3	0.75	0.63	white	square	0.34	0.15
4	0.75	0.47	white	square	0.34	0.15
5	0.75	0.3	white	square	0.34	0.15
6	0.5	0.15	white	square	0.34	0.13
7	0.5	0.8	transparent	square	0.001	0.001
8	0.5	0.63	transparent	square	0.001	0.001
9	0.5	0.47	transparent	square	0.001	0.001
10	0.5	0.3	transparent	square	0.001	0.001')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[6, 1] = "' '"
M[2, 7] = "' '"
M[3, 8] = "' '"
M[4, 9] = "' '"
M[5, 10] = "' '"
M[6, 10] = "' '"
# colours
tcol = rep('black', n.labels)

## make figure 
jpeg('figures/consort.flow.jpg', width=7.5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, arr.pos = 0.25,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
dev.off()

