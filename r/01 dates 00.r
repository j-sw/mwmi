library(magrittr)
library(tidyverse)
library(data.table)
library(stringr)

# Load metadata on scraping results
load("./meta/out00.rData")

# Filter to successful downloads
out <- out[url_status%in%1,]
# File location
out <- out[,file:=gsub(".*?([^/]+)$","\\1",url)]
out <- out[,location:=paste0("./raw files/",title,"/",file)]

# Start looking for metadata (noting that this isn't included in the files... wtf)

# Regex expressions for months and years
make_regex_multi <- function(x) paste0("(",paste0(x,collapse="|"),")")
# regex for any of the 2 letter month numbers
m_2 <- make_regex_multi(str_pad(1:12,2,pad="0"))
# regex for any of the three letter month abbreviations
m_3 <- make_regex_multi(month.abb)
# regex for any of the month names
m_full <- make_regex_multi(month.name)
# regex for 4 digit years
y_full <- make_regex_multi(2011:2019)
# regex for 2 digit years
y_2 <- make_regex_multi(11:19) # this is going to cause issues in 2020, but avoids problems with matching 20XX

# regex expressions for month then year
# out[is.na(month_text) & grepl(paste0(m_2,y_2),file,ignore.case=T),file]

out[,month_text:=NULL]
out[,year_text:=NULL]

month_year <- c(paste0(rep(c(m_full,m_3,"(sept)"),each=2),"[ \\+_-]?",c(y_full,y_2)),paste0(m_2,y_full))
month_year <- c(month_year,"^defra.wmi.([01][0-9])(12)")
month_year <- c(month_year,paste0("^dcms.",m_full,".*",y_full))
month_year <- c(month_year,paste0("^co.*3[01]_?",m_2,"_?",y_2))
month_year <- c(month_year,paste0("^co.*28_?",m_2,"_?",y_2))
for (i in 1:length(month_year)) {
  out[grepl(month_year[i],file,ignore.case=T),month_text:=gsub(paste0(".*",month_year[i],".*"),"\\1",file,ignore.case=T)]
  out[grepl(month_year[i],file,ignore.case=T),year_text:=gsub(paste0(".*",month_year[i],".*"),"\\2",file,ignore.case=T)]
}

# regex for year then month
year_month <- c("DEFRA.wmi.(1[134])([01][0-9])",paste0(y_full,"[ _-]?",m_2),paste0(y_full,"[ _-]",c(m_3,m_full)),paste0(y_2,"[ _-]",c(m_3,m_full)))
year_month <- c(year_month,paste0(y_full,"\\+",y_2))
year_month <- c(year_month,paste0("(1[78])_csv_file_",m_full))
for (i in 1:length(year_month)) {
  out[grepl(year_month[i],file,ignore.case=T),month_text:=gsub(paste0(".*",year_month[i],".*"),"\\2",file,ignore.case=T)]
  out[grepl(year_month[i],file,ignore.case=T), year_text:=gsub(paste0(".*",year_month[i],".*"),"\\1",file,ignore.case=T)]
}

# Convert to numeric months / years
out[,year:=NULL]
out[,month:=NULL]

out[!is.na(year_text) & grepl(y_full,year_text),year:=as.numeric(year_text)]
out[!is.na(year_text) & is.na(year) & grepl(y_2,year_text),year:=as.numeric(paste0(20,year_text))]
out[!is.na(year_text),table(year,useNA="ifany")]

out[,month_text:=tolower(month_text)]
m_full_ <- tolower(m_full)
m_3_ <- tolower(m_3)
out[!is.na(month_text) & grepl(m_2,month_text),month:=as.numeric(month_text)]
out[!is.na(month_text) & is.na(month) & grepl(m_full_,month_text),month:=as.numeric(factor(month_text,labels=m_full_))]
out[!is.na(month_text) & is.na(month) & "sept"==month_text,month:=9]
out[!is.na(month_text) & is.na(month) & grepl(m_3_,month_text),month:=as.numeric(factor(month_text,labels=m_3_))]
out[!is.na(month_text),table(month,useNA="ifany")]

save(out,file="./meta/out01.rData")

