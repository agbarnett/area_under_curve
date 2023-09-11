# 99_share_sample_data.R
# Put sample of data on github
# Sep 2023
library(openxlsx)
library(dplyr)
load('data/analysis_ready.RData')
TeachingDemos::char2seed('wycombe')

# random selection
pmids = sample(unique(results$pmid), 400)
to_export = filter(results, pmid %in% pmids) %>%
  select(pmid, date, type, auc, mean, lower, upper)

# export to Excel
filename = 'example data/AUCs.xlsx'
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "AUCs")
freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
writeDataTable(wb, sheet = 1, x = to_export,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)