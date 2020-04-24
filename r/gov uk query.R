options(stringsAsFactors=FALSE)
library(jsonlite)
library(magrittr)
library(tidyverse)
# library(data.table)
# library(rvest)
# library(XML)

# Return search results for 'workforce management information' from gov.uk - max results is 1500, which is 2x the results on 17/4/2020
mwmi_search <- jsonlite::fromJSON("https://www.gov.uk/api/search.json?q=%22workforce%20management%20information%22&count=1500")

# Query content from each search result
# mwmi_content <- mwmi_search$results$link %>%
#   paste0("https://www.gov.uk/api/content",.) %>%
#   map(~ {print(.x) ; list(link=.x,content=jsonlite::fromJSON(.x))})
mwmi_content <- mwmi_search$results %>%
  split(.,1:nrow(.)) %>%
  map(~ {
    print(.x$link)
    list(
      search=.x,
      content=jsonlite::fromJSON(paste0("https://www.gov.uk/api/content",.x$link))
    )
  })

# Which results have files attached
has_attachment <- mwmi_content %>%
  map(~ !is.null(.x$content$details$attachments$url)) %>%
  unlist %>% which

# Get links to files
mwmi_files <- mwmi_content[has_attachment] %>%
  map(~ {
    a = .x$content$details$attachments %>% select(title,url)
    n_rows <- nrow(a)
    a$search <- rep(list(.x$search),n_rows)
    a$content <- rep(list(.x$content),n_rows)
    a
  }) %>%
  bind_rows

# Get acronyms
mwmi_files$acronyms <- mwmi_files$search %>% 
  map(~ .x$organisations[[1]]$acronym) %>% 
  map(~ {if (is.null(.x)) NA else paste0(.x,collapse=", ")}) %>% 
  unlist %>% tolower
mwmi_files$acronym <- mwmi_files$search %>% 
  map(~ .x$organisations[[1]]$acronym) %>% 
  map(~ {if (is.null(.x)) NA else .x[1]}) %>% 
  unlist %>% tolower

# Get file name
mwmi_files$file_name <- mwmi_files$url %>% gsub(".*?([^/]+)$","\\1",.)

# Flag anything that doesn't point to a data table
mwmi_files$data_table_file <- mwmi_files$url %>%
  str_ends(regex("(csv)|(xlsx?)|(ods)",ignore_case=TRUE))

# Create dir structure
mwmi_files$acronym %>%
  unique %>%
  paste0("./raw files/",.,"/") %>%
  map(~ dir.create(.x))

# Download
urls <- mwmi_files %>%
  filter(data_table_file==TRUE) %>%
  pull(url)
destfiles <- mwmi_files %>%
  filter(data_table_file==TRUE) %$%
  paste0("./raw files/",acronym,"/",file_name)

# Maximum files open is ~ 512
max_files_value <- ceiling((1:length(urls))/80)
map2(split(urls,max_files_value),split(destfiles,max_files_value),~ {download.file(.x,.y,mode='wb',method='libcurl'); Sys.sleep(.5)})


# curl_download(urls[1:5],destfiles[1:5],quiet=FALSE)



