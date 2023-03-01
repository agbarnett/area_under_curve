# 99_article_to_df_adapted.R
# process a single article
# adapted code from github, https://github.com/cran/easyPubMed
# called by 99_table_articles_byAuth_adapted.R
# documentation for pubmed elements is here https://dtd.nlm.nih.gov/ncbi/pubmed/doc/out/180101/index.html
# August 2022

article_to_df_adapted <-
  function(pubmedArticle) 
  {
    #
    options(warn = -1)
    
    # Global Check!
    if (class(pubmedArticle) != "character" |
        regexpr("(<PubmedArticle)(.+)(\\/PubmedArticle>)", pubmedArticle) < 0 )
    {
      message("An error occurred - character")
      return(NULL)
    }
    
    # Get started
    tryCatch({
      
      tmp.article <- custom_grep(xml_data = pubmedArticle, tag = "PubmedArticle", format = "char") # extract the article
      if (is.null(tmp.article)) 
      {
        message("An error occurred - null")
        return(NULL)
      }
      
      # Fetch ID string
      tmp.paperID  <- custom_grep(xml_data = tmp.article, tag = "ArticleIdList", format = "char")
      if (is.null(tmp.paperID)) 
      {
        message("An error occurred - null ID")
        return(NULL)
      } else {
        tmp.paperID <- gsub("[[:space:]]", "", tmp.paperID[1])
      }
      
      # Get PMID
      tmp.PMID <- gsub("^(.*ArticleIdIdType=\\\"pubmed\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
      tmp.PMID <- gsub("<.*$", "", tmp.PMID)

      ## Dates, kept as pubmed date, see https://www.nlm.nih.gov/bsd/licensee/elements_descriptions.html
      dstart = str_locate(string=tmp.article, pattern='PubMedPubDate PubStatus="pubmed"')
      tmp.date = str_sub(string=tmp.article, start = dstart[2]+2, end=nchar(tmp.article)) # start from pubmed date so that next end of pubmed date work
      dend = str_locate(string=tmp.date, pattern='\\/PubMedPubDate')
      tmp.date = str_sub(string=tmp.date, start = 1, end=dend[1]-2) # now go to next end
      ystart = str_locate(string=tmp.date, pattern='\\<Year\\>') # year
      year = as.numeric(str_sub(tmp.date, ystart[2]+1, ystart[2]+4))
      mstart = str_locate(string=tmp.date, pattern='\\<Month\\>') # month
      mend = str_locate(string=tmp.date, pattern='\\<\\/Month\\>')
      month = str_sub(tmp.date, mstart[2]+1, mend[1]-1)
      if(month %in% month.abb){ # convert text month to number
        month = factor(month, levels=month.abb) #
      }
      month = as.numeric(month)
      dstart = str_locate(string=tmp.date, pattern='\\<Day\\>') # day
      dend = str_locate(string=tmp.date, pattern='\\<\\/Day\\>')
      day = as.numeric(str_sub(tmp.date, dstart[2]+1, dend[1]-1))
      if(is.na(month) == TRUE){month=6} # middle of year if month missing
      if(is.na(day) == TRUE){day = 15} # middle of month if day missing
      date = as.Date(ISOdate(year = year, 
                             month = month, 
                             day = day)) # convert day/month/year to date
      
      # number of authors
      n.authors = str_count(string=tmp.article, pattern="\\<\\/Author\\>")
      
      # first author's surname
      first.author = ''
      if(n.authors > 0){
        tmp.author <- custom_grep(xml_data = tmp.article, tag = "Author", format = "char")
        tmp.last <- custom_grep(xml_data = tmp.author, tag = "LastName", format = "char")
        first.author = ifelse(is.null(tmp.last)==TRUE, '', tmp.last)
      }
      
      # Article title
      tmp.title <- custom_grep(xml_data = tmp.article, tag = "ArticleTitle", format = "char")
      if (length(tmp.title) > 1){
        tmp.title <- paste(tmp.title, collapse = " ", sep = " ")
      } else if (length(tmp.title) < 1) {
        # use my own search because of danger of "Title" appearing elsewhere
        title.loc = str_locate(pattern='\\<Title\\>', tmp.article)
        title.loc.end = str_locate(pattern='\\</Title\\>', tmp.article)
        tmp.title = str_sub(string=tmp.article, start = title.loc[2]+1, end=title.loc.end[1]-1)
        if (length(tmp.title) > 1){
          tmp.title <- NA # if title and alternative title are missing
        }
      }
      
      # country for first author
      country = ''
      author.list = custom_grep(xml_data = tmp.article, tag = "AuthorList")
      if(length(author.list) > 0){
        aff = custom_grep(xml_data = author.list, tag = "AffiliationInfo") # narrow down to author list and affiliation info
        if(length(aff) > 0){
          address = custom_grep(xml_data = aff[[1]], tag = "Affiliation")[[1]] # first author address
          index = which(str_detect(address, country_list$match_words))
          country = country_list$english[index][1] # convert to common English name; just take one if multiple
        }
      }
      
      # Language, only want English (is excluded later so we can see numbers)
      # had to use my own search because "Language" was being picked up in abstract ...
      # ... specific code not working: tmp.language <- custom_grep(xml_data = tmp.article, tag = "Language", format = "char")
      language.loc = str_locate_all(pattern='\\<Language\\>', tmp.article)
      tmp.language <- str_sub(string = tmp.article, start=language.loc[[1]][2]+1, end=language.loc[[1]][2]+3) # extract three letter language
      tmp.language = tolower(tmp.language) # make sure it is lower case
      if(nchar(tmp.language)!=3){tmp.language = 'Missing'}

      ## Abstract & Copyright
      # remove <OtherAbstract>, which can be abstract in another language - had to do whilst still in XML format
      abstract.other.location.start <- str_locate_all(tmp.article, pattern='\\<OtherAbstract')[[1]]
      if(length(abstract.other.location.start)>0 & is.na(abstract.other.location.start)[1] == FALSE){
        n.other = nrow(abstract.other.location.start)
        abstract.other.location.end <- str_locate_all(tmp.article, pattern='\\<\\/OtherAbstract\\>')[[1]]
        first.string = str_sub(tmp.article, start = 1, end = abstract.other.location.start[1,1]-1)
        last.string = str_sub(tmp.article, start = abstract.other.location.end[n.other,2], end = nchar(tmp.article))
        tmp.article = paste(first.string, last.string) # create new article
      }  
      # remove Copyright (assume it is at end, so discard anything after); can still be in abstract and CopyrightInformation!
      abstract.copyright.location.start <- str_locate_all(tmp.article, pattern='\\<CopyrightInformation')[[1]]
      if(length(abstract.copyright.location.start)>0 & is.na(abstract.copyright.location.start)[1] == FALSE){
        abstract.copyright.location.end <- str_locate_all(tmp.article, pattern='\\<\\/CopyrightInformation\\>')[[1]]
        first.string = str_sub(tmp.article, start = 1, end = abstract.copyright.location.start[1,1]-1)
        last.string = str_sub(tmp.article, start = abstract.copyright.location.end[1,2]+1, end = nchar(tmp.article))
        tmp.article = paste(first.string, last.string) # create new article
      }  

      # get "raw" abstract from rest of entry
      abstract.raw = ''
      abstract.start <- str_locate(tmp.article, pattern='\\<Abstract') # first mention
      if(length(abstract.start)>0 & is.na(abstract.start)[1] == FALSE){
        abstract.end = str_locate_all(tmp.article, pattern='\\<\\/Abstract')[[1]]
        abstract.end = abstract.end[nrow(abstract.end),] # last ending
        abstract.raw = str_sub(tmp.article, start=abstract.start[1], end=abstract.end[2]+1)
      }  
      
      # remove systematic review registrations, e.g., 28320459[PMID] - test17.xml
      abstract.registration.location.start <- str_locate(abstract.raw, pattern='SYSTEMATIC REVIEW REGISTRATION')[1]
      if(!is.na(abstract.registration.location.start)){
        abstract.registration.location.end = str_locate_all(abstract.raw, pattern='\\<\\/AbstractText\\>')[[1]]
        end = min(abstract.registration.location.end[abstract.registration.location.end[,2] > abstract.registration.location.start, 2]) # find the next "/AbstractText" after the systematic review
        first.string = str_sub(abstract.raw, start = 1, end = abstract.registration.location.start-22) # minus 22 to also remove <AbstractText>
        last.string = str_sub(abstract.raw, start = end+1, end = nchar(abstract.raw))
        abstract.raw = paste(first.string, last.string) # create new article
      }  
      
      # remove abstract sub-headings
      sub.headings.start <- str_locate(abstract.raw, pattern='\\<AbstractText') # find first
      if(is.na(sub.headings.start)[1] == FALSE){ # first remove all </AbstractText> 
        abstract.raw = str_remove_all(string=abstract.raw, pattern='\\<\\/AbstractText\\>')
      }
      next_end = FALSE
      while(is.na(sub.headings.start)[1] == FALSE){
        first = str_sub(abstract.raw, start = 1, end = sub.headings.start[1,1]-1)
        second = str_sub(abstract.raw, start = sub.headings.start[1,2]+1, end = nchar(abstract.raw))
        next.ending = str_locate(second, '>') # now find the next ending (varying because of label length)
        second = str_sub(second, next.ending[1]+1, nchar(second))
        # add methods start and end if sub-heading is methods
        section = str_extract(abstract.raw, pattern='(?<=Label=\")\\w+\"')
        section = ifelse(is.na(section) == TRUE, '', section)
        section = str_remove(section, '\"')
        section_start = ''
        if(next_end == TRUE){
          section_start = 'results_end'
          next_end = FALSE
        }
        if(str_detect(tolower(section),c('results|findings')) == TRUE){
          section_start = 'results_start'
          next_end = TRUE
        }
        #
        abstract.raw = paste(first, section_start, second)
        sub.headings.start <- str_locate(abstract.raw, pattern='\\<AbstractText') # find next, if there is one
      }  
      
      # now extract abstract and leave behind <CopyrightInformation>
      tmp.abstract <- custom_grep(xml_data = abstract.raw, tag = "Abstract", format = "char")
      if (is.null(tmp.abstract) == FALSE){
        tmp.abstract <- paste(tmp.abstract, collapse = " ", sep = " ") # if it is a list
        # remove Copyright, sometimes ends up in abstract
        tmp.copyright <- custom_grep(xml_data = tmp.article, tag = "CopyrightInformation", format = "char")
        if(is.null(tmp.copyright)==FALSE){tmp.abstract = str_remove(fixed(tmp.abstract), pattern=fixed(tmp.copyright))}
      } else if (is.null(tmp.abstract) == TRUE) {tmp.abstract <- NA}
      
      # second pass for removing copyright (as it is not always in CopyrightSection)
      # strings to remove from abstract:
      string.to.remove = c('\\<CopyrightInformation\\>',
                           '\\<\\/CopyrightInformation\\>',
                           'No commercial re-use',
                           'This is an open access article distributed under the terms of the Creative Commons Attribution (CC BY) license',
                           'This article is protected by copyright\\. All rights reserved\\.',
                           'Published [0-9][0-9][0-9][0-9]\\.',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+\\.?)', # catch-alls for copyright statements; " .? ?" means that there might be copyright symbol followed by a space
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+\\.?)',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+\\.?)',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+) (\\S+\\.?)',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+) (\\S+),? (\\S+\\.?)', # for John Wiley & Sons, Inc
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+\\.?)',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+\\.?)',
                           'Copyright .? ?[0-9][0-9][0-9][0-9] (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+\\.?)',
                           "Published by Elsevier Ltd.",
                           "This is a work of the U.S. Government and is not subject to copyright protection in the United States.",
                           "Foreign copyrights may apply.",
                           "Published by John Wiley & Sons Ltd.",
                           "All rights reserved.",
                           "all rights reserved",
                           "PsycINFO Database Record (c) [0-9][0-9][0-9][0-9] APA",
                           'This article is a US Government work and is in the public domain in the USA\\.')
      string.to.remove = string.to.remove[order(-nchar(string.to.remove))] # long to short
      string.to.remove = paste(string.to.remove, collapse='|')
      tmp.abstract = str_remove_all(tmp.abstract, pattern=string.to.remove)
      
      # Get Journal Abbrv
      tmp.jabbrv  <- custom_grep(xml_data = tmp.article, tag = "ISOAbbreviation", format = "char")
      tmp.jabbrv <- ifelse(is.null(tmp.jabbrv), NA, tmp.jabbrv)
      tmp.jabbrv = str_remove_all(string=tmp.jabbrv, pattern='\\.') # tidy journal names to make them a bit shorter
      
      # Get article type, see list here https://www.nlm.nih.gov/mesh/pubtypes.html
      tmp.type  <- custom_grep(xml_data = tmp.article, tag = "PublicationType", format = "char")
      tmp.type <- ifelse(is.null(tmp.type), NA, tmp.type)
      
      # get mesh terms (added Feb 2023)
      mesh = NULL
      mesh.start = str_locate_all(string=tmp.article, pattern='\\<DescriptorName')[[1]]
      mesh.end = str_locate_all(string=tmp.article, pattern='\\/DescriptorName')[[1]]
      if(length(mesh.start)>0){
        for (k in 1:nrow(mesh.start)){
          start = str_sub(tmp.article, mesh.start[k,2], mesh.end[k,1] - 2)
          start_1 = str_locate(start, '\\>')
          mesh = c(mesh, str_sub(start, start_1[1]+1, nchar(start)))
        }
        mesh = paste(mesh, collapse = '; ')
      }
      if(is.null(mesh)==TRUE){mesh = ''}
      
      # frame to output
      final.mat <- data.frame(pmid = tmp.PMID, 
                      language = tmp.language,
                      n.authors = n.authors,
                      first.author = first.author,
                      country = country,
                      title = tmp.title,
                      abstract = str_squish(tmp.abstract),
                      date = date,
                      type = tmp.type,
                      mesh = mesh,
                      jabbrv = tmp.jabbrv, stringsAsFactors = FALSE)
      
      return(final.mat)
    })

  }
