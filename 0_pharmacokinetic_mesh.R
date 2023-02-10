# 0_pharmacokinetic_mesh.R
# find mesh terms commonly used by pharmacokinetic studies
# feb 2023
library(rentrez)
#source('0_my_pubmed_key_do_not_share.R')

# search based on title and abstract
query = 'pharmacokinetic[tiab]'
p_search = entrez_search(db="pubmed", term=query)

# get mesh terms

# frequency of mesh terms

# used here instead https://www.ncbi.nlm.nih.gov/mesh/68010599