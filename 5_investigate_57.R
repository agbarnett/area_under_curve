# 5_investigate_57.R
# investigating the excess of results at 0.57
# March 2023
library(dplyr)
library(rentrez)
library(tm)
source('0_my_pubmed_key_do_not_share.R')
batch_size = 20 # fetch abstracts in larger batch rather than single

# get the data
load('data/analysis_ready.RData') # from 4_combine_results.R

# 0.57 and a comparison 0.58
are_57 = filter(results, auc == 0.57)
are_58 = filter(results, auc == 0.58)

# now get the abstracts
all_abstracts = all_keywords = NULL
n_sample = nrow(are_57)
n_runs = floor(n_sample/batch_size)
for (k in 1:n_runs){ # temp start
  start = 1 + (k-1)*batch_size
  stop = k*batch_size
  detail = entrez_fetch(db = 'pubmed', id=are_57$pmid[start:stop], rettype='xml', parsed=FALSE, api_key = my.api.key) # get detail from pubmed
  Sys.sleep(10) # to avoid limiter
  parsed = parse_pubmed_xml(detail) # parse from XML
  # loop through keywords and abstract
  this_batch = length(parsed)
  for (j in 1:this_batch){
    all_abstracts = c(all_abstracts, parsed[[j]]$abstract)
    this_key = parsed[[j]]$key_words
    if(length(this_key)>0){all_keywords = c(all_keywords, this_key)}
  }
  # could remove plurals?
}

# now get the abstracts
all_abstracts_58 = all_keywords_58 = NULL
n_sample = nrow(are_58)
n_runs = floor(n_sample/batch_size)
for (k in 1:n_runs){ # temp start
  start = 1 + (k-1)*batch_size
  stop = k*batch_size
  detail = entrez_fetch(db = 'pubmed', id=are_58$pmid[start:stop], rettype='xml', parsed=FALSE, api_key = my.api.key) # get detail from pubmed
  Sys.sleep(10) # to avoid limiter
  parsed = parse_pubmed_xml(detail) # parse from XML
  # loop through keywords and abstract
  this_batch = length(parsed)
  for (j in 1:this_batch){
    all_abstracts_58 = c(all_abstracts_58, parsed[[j]]$abstract)
    this_key = parsed[[j]]$key_words
    if(length(this_key)>0){all_keywords_58 = c(all_keywords_58, this_key)}
  }
  # could remove plurals?
}

## top key words
# 0.57
tab = table(all_keywords)
tab = tab[order(-tab)]
tab[1:20]
# 0.58
tab = table(all_keywords_58)
tab = tab[order(-tab)]
tab[1:20]

# process the text
text = Corpus(VectorSource(all_abstracts))
text = tm_map(text, removeWords, stopwords("english"))
#text = tm_map(text, removePunctuation) # not sure about this

# word frequency
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
f = head(d, 10) 


