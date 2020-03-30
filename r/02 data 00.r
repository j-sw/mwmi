library(magrittr)
library(tidyverse)
library(data.table)
library(readxl)

# Load metadata on scraping results
load("./meta/out01.rData")

out <- out[!is.na(month_text),]
out[,file_status:=0]
out[,raw_data:=as.list(1:nrow(out))]

# CSVs
sel = out[,type=="csv"]
isel = which(sel)
dlist <- list()
for (i in 1:sum(sel)) {
  idat <- out[isel[i],]
  temp_data <- try(read.csv(idat$location,header=FALSE))
  if (class(temp_data)[1]!="try-error") {
    dlist <- append(dlist,list(temp_data))
    # out[isel[i],data:=as.list(temp_data)]
    # out$data[[isel[i]]] <- list(temp_data)
    out[isel[i],file_status:=1]
    
  } else {
    dlist <- append(dlist,list())
    out[isel[i],file_status:=-1]
  }
  print(i)
}
out[isel,raw_data:=dlist]

# excel
sel = out[,type%in%c("xls","xlsx")]
isel = which(sel)
dlist <- list()
for (i in 1:sum(sel)) {
  idat <- out[isel[i],]
  temp_data <- try(read_excel(idat$location,col_names=FALSE))
  if (class(temp_data)[1]!="try-error") {
    dlist <- append(dlist,list(temp_data))
    out[isel[i],file_status:=1]
    
  } else {
    dlist <- append(dlist,list())
    out[isel[i],file_status:=-1]
  }
  print(i)
}
out[isel,raw_data:=dlist]