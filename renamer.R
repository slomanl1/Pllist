source("~/Local.R")
library(tools)
setwd('~/')
rm(list=ls())
load('~/namer.RData')
setwd('Z:/My Videos/RealPlayer Downloads')
for ( i in 1:nrow(namer)){
  print(i)
  setwd('Z:/My Videos/RealPlayer Downloads')
  #print(paste(namer[i,'oldnm'],namer[i,'newnm']))# file copy goes here
  #file.copy(as.character(namer[i,'oldnm']),paste('E:/',namer[i,'newnm'],sep=''))
  nmr=paste('RealPlayer Downloads\\',namer[i,'oldnm'],sep='')  
  newnmr=paste('RealPlayer Downloads\\',namer[i,'newnm'],sep='')
  
  setwd(paste(pldrive,'My Playlists',sep=""))
  for(j in 1:length(wpls)){
    wh=which(regexpr(nmr,dff[,j],fixed=TRUE)>0)
    if(length(wh)>0){
      print(paste('old',nmr,newnmr,dff[wh,j]))
      dff[wh,j]=sub(nmr,newnmr,dff[wh,j],fixed=TRUE)
      print(paste('new',dff[wh,j]))

      }
    }
  }


setwd('~/')
for(k in 1:ncol(dff)){
  print(k)
  #writeLines(dff[!is.na(dff[,k]),k],as.character(colnames(dff))[k])
}



