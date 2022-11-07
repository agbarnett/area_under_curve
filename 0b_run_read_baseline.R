# 0b_run_read_baseline.R
# called by 0_read_pubmed_baseline.R
# read and process the XML data
# version using data that has been manually downloaded from ftp survey (because batch process using API was creating gaps)
# October 2022
load('../narrator/data/countries.RData') # for country affiliation

# import the XML file into R - takes a while
very_raw_pubmed = table_articles_byAuth_adapted(pubmed_data = file, # from 99_table_articles_byAuth_adapted.R
                                                 encoding = "UTF-8") # recommended encoding
start.N = very_raw_pubmed$N # store starting number of papers
very_raw_pubmed = very_raw_pubmed$papers.authors.df # reuse old data name

## further processing
raw_pubmed = mutate(very_raw_pubmed, 
              pmid = as.numeric(pmid),
              # replace_non_ascii does not get the following right; instead makes it 'copyright'
              abstract = str_replace_all(abstract, '⩽' ,' less than or equal to '), 
              abstract = str_replace_all(abstract, '⩾' ,' greater than or equal to '),
              abstract = str_replace_all(abstract, 'Ω' ,' Omega '), 
              #
              country = ifelse(country =='', NA, country), # replace empty with missing
              #
              abstract = replace_non_ascii(abstract), # from textclean, removes, e.g., Greek letters
              title = replace_non_ascii(title),
              title = str_remove(string=title, pattern="[:punct:]authors' transl[:punct:]|[:punct:]author's transl[:punct:]|[:punct:]authors transl[:punct:]|[:punct:]authors transl"), # remove this note
              title = str_remove(string=title, pattern='[:punct:]$'), # remove punctuation at end of title
              title = str_remove(string=title, pattern='\\] $|^\\[|\\]$') # remove square brackets around title
) %>% 
  dplyr::select(pmid, language, n.authors, first.author, country, jabbrv, type, date, title, abstract) # reduce the number of variables

## Paper numbers throughout the data management process
numbers = data.frame(start = start.N)
# remove non-English
raw_pubmed = filter(raw_pubmed, language=='eng') %>%
  dplyr::select(-language)
numbers$post.non.english = nrow(raw_pubmed)
# remove empty abstracts
raw_pubmed = filter(raw_pubmed, !is.na(abstract))
numbers$post.empty.abstract = nrow(raw_pubmed)

# save data for further processing; number from 0_read_pubmed_baseline.R
outfile = paste('raw/unprocessed.pubmed.baseline.', number, '.RData', sep='')
save(raw_pubmed, numbers, file=outfile)
