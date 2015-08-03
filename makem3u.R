source("~/Local.R")
library(tools)
setwd('~/')
rm(list=ls())
load('dffDone.Rdata')
wpls=colnames(dff)

for(j in 1:ncol(dff)){
  pos1=unlist(gregexpr('loads',dff[!is.na(dff[,j]),j]))
  pos2=unlist(gregexpr('wmv',dff[!is.na(dff[,j]),j]))
  fn=paste('Z:/My Videos/RealPlayer Downloads/',substr(dff[!is.na(dff[,j]),j],pos1+6,pos2+2),sep='')
  fn=subset(fn,grepl('wmv',fn))
  writeLines(fn,paste(wpls[j],'.m3u',sep=''))
}
