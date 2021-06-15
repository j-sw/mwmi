library(jsonlite)
library(magrittr)
library(tidyverse)
library(data.table)
library(rvest)
library(XML)

options(width=140,stringsAsFactors=FALSE)

################################################################################
# Call data.gov.uk API to get list of published data - note that the return size is 10 by default
publication_query <- fromJSON("https://ckan.publishing.service.gov.uk/api/action/package_search?q=%22workforce+management+information%22&rows=200")
# Structure of return:
# List of 3:
#   help: url
#   success: boolean
#   result: list of 5
#     count:
#     sort:
#     facets:
#     results: list of 52
#       resources: list of length n = results, each containing a data frame with the results for a particular department
#         
#     search_facets:

publication_query$success
publication_query$result$count

# data on pubs
pubs <- publication_query$result$results

# Iterate through results and extract urls and some metadata
out <- NULL
for (i in 1:nrow(pubs)) {
  if (nrow(pubs$resources[[i]])>0) {
    io <- pubs$resources[[i]]
    io <- io[,names(io) %in% c("name","description","format","date","created","last_modified","datafile-date","url"),drop=FALSE] # select matching vars from resources
    io$title = pubs$organization$title[i]
    io$i <- i
    # io <- data.frame(
    #   title=pubs$organization$title[i], # department name
    #   name=pubs$resources[[i]]$name,
    #   description=pubs$resources[[i]]$description,
    #   format=pubs$resources[[i]]$format,
    #   date=pubs$resources[[1]]$date,
    #   url=pubs$resources[[i]]$url # links
    #   )
    out <- rbindlist(list(out,io),fill=TRUE)
  } else {
    print(paste0("No results for ",i,": ",pubs$organization$title[i]))
    }
}

names(out) <- make.names(names(out))

out <- data.table(out)
out <- out[!duplicated(url),] # drop duplicated links
out[,url_status:=0]

# Set up date and filter
# Note that it appears the 'date' field is not being filled out from 2019 onwards
out[,created:=as.Date(created)]

# Filter to 2020
out <- out[year(created) >=2018,]

# Classify url types
out[grep("csv$",url,ignore.case=T),type:='csv']
out[grep("xls$",url,ignore.case=T),type:='xls']
out[grep("xlsx$",url,ignore.case=T),type:='xlsx']
out[grep("ods$",url,ignore.case=T),type:='ods']
out[grep("pdf$",url,ignore.case=T),type:='pdf']


# ################################################################################
# # See if we can scrape links from indirect links
# sel = out[,is.na(type) & url_status <=0]
# isel = which(sel)
# 
# for (i in 1:sum(sel)) {
#   
#   ii = isel[i] # this shouldn't be necessary?
#   idata = out[ii,]
#   
#   doc.html <- try(read_html(idata$url))
#   if (class(doc.html)[1]!="try-error") {
#     doc <- htmlParse(doc.html)
#     links <- xpathSApply(doc, "//a/@href")
#     free(doc)
#     file_links <- grep("\\.((csv)|(ods)|(xls)|(xlsx))$",ignore.case=TRUE,links,value=TRUE)
#     if (length(file_links)>0) {
#       new_rows <- merge(idata[,setdiff(names(idata),c('url','url_status')),with=F],data.table(title=idata$title,url=file_links,url_status=0))
#       out <- rbind(out,new_rows)
#       out[isel[i],url_status:=2]
#     } else {
#       out[isel[i],url_status:=-2.5] # Failed to find links
#     }
#   } else {
#     out[isel[i],url_status:=-2] # Failed to load HTML
#   }
#   print(paste0(i,"/",sum(sel)," html scraped"))
# }
# 
# out <- out[!duplicated(url),] # drop duplicated links
# 
# out[grep("csv$",url,ignore.case=T),type:='csv']
# out[grep("xls$",url,ignore.case=T),type:='xls']
# out[grep("xlsx$",url,ignore.case=T),type:='xlsx']
# out[grep("ods$",url,ignore.case=T),type:='ods']
# out[grep("pdf$",url,ignore.case=T),type:='pdf']
# 
# ################################################################################
# Download data files
sel = out[,type%in%c('csv','xls','xlsx','ods') & url_status <= 0]
isel = which(sel)

for (i in 1:sum(sel)) {
  ii = isel[i]
  idata = out[ii,]
  directory = paste0("./raw files/",idata$title,"/")
  if (!dir.exists(directory)) dir.create(directory)
  filename = gsub(".*?([^/]+)$","\\1",idata$url) %>% # given filename
    paste0(directory,.)
  if (!file.exists(filename)) {
    iurl = idata$url
    if (!grepl("^http",iurl)) iurl <- paste0("https://www.gov.uk",iurl)
    download_success = try(download.file(iurl,filename,mode='wb')) # redirects?
    out[isel[i],url_status:=-1+2*(download_success==0)] # un / successful download
  } else {
    out[isel[i],url_status:=1] # successful download
  }
  print(paste0(i,"/",sum(sel)," downloaded"))
}

save(out,file="./meta/out00.rData")


