source("~/Local.R")
library(tools)
setwd('~/')
rm(list=ls())
load('~/namer.RData')

setwd('Z:/My Videos/RealPlayer Downloads')

for(i in 1:nrow(namer))
  file.rename(paste('newnamer',namer[i,'oldnm'],sep=''),as.character(namer[i,'oldnm']))

              
