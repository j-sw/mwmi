# List all files
file_meta <- data.frame(file_list=list.files("./raw files",recursive=TRUE,full.names=TRUE))

# File names, acronym, type
file_meta <- file_meta %>%
  mutate(file_name=gsub(".*?([^/]+)$","\\1",file_list)) %>%
  mutate(acronym=gsub("./raw files/([^/]+)/.*","\\1",file_list)) %>%
  mutate(file_type=gsub(".*\\.([csvxlsods]{3,4})$","\\1",file_name,ignore.case=T) %>% tolower)

# Month and year
file_meta <- file_meta %>%
  mutate(month=NA,year=NA)

# Text months
month_text_regex <- c("jan(uary)?","feb(ruary)?","mar(ch)?","apr(il)?","may","june?","july?","aug(ust)?","sept?(ember)?","oct(ober)?","nov(ember)?","dec(ember)?")
for (i in 1:12) file_meta <- file_meta %>% mutate(month = ifelse(grepl(month_text_regex[i],file_name,ignore.case=T),i,month))

# Text months and years
any_month_text_regex <- month_text_regex %>%
  paste0("(",.,")") %>%
  paste0(.,collapse="|")

year_4_digit <- file_meta$file_name %>%
  gsub(any_month_text_regex,"MMMMM",.,ignore.case=T) %>%
  gsub(".*MMMMM.(20[12][0-9]).*","\\1",.,ignore.case=T) %>%
  as.integer

year_2_digit <- file_meta$file_name %>%
  gsub(any_month_text_regex,"MMMMM",.,ignore.case=T) %>%
  gsub(".*MMMMM.([12][0-9])[^0-9].*","\\1",.,ignore.case=T) %>%
  as.integer

# Add in years
file_meta <- file_meta %>%
  mutate(year = ifelse( !is.na(year_4_digit), year_4_digit, year)) %>%
  mutate(year = ifelse( is.na(year) & !is.na(year_2_digit), year_2_digit+2000, year))


# Save
saveRDS(file_meta,"./data/file_meta.RDS")
