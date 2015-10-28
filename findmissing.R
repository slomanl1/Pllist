source("~/Local.R")
library(tools)
setwd('~/')
if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  rm(list=ls())
  source("~/Local.R")
  shell('dir "Z:\\My Videos\\RPDNClips" /od/b > ddd.txt')
  lns=readLines('ddd.txt')
  last=as.numeric(substr(lns,1,5))
  last[is.na(last)]=as.numeric(substr(lns[is.na(last)],1,4))
}else
  print('Locker Not Found')

