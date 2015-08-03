source("~/Local.R")
library(tools)
setwd('~/')
rm(list=ls())
shell('dir "Z:\\My Videos\\RealPlayer Downloads" /od/b > ddd.txt')
lns=readLines('ddd.txt')
unlink('ddd.txt')
setwd(paste(pldrive,'My Playlists',sep=""))
wpls=dir()
setwd('Z:/My Videos/RealPlayer Downloads')
fnfo=file.info(lns)
fnf=fnfo[order(fnfo$mtime),]
namer=data.frame(oldnm=rownames(fnf),newnm=paste(1:nrow(fnf),'.wmv',sep=''))

setwd(paste(pldrive,'My Playlists',sep=""))

ttx=NA
for(i in 1:length(wpls))
{
  print(i)
  ttx[i]=length(readLines(wpls[i]))
  
}

dff=data.frame(matrix(NA,max(ttx),length(wpls)))
colnames(dff)=wpls
for(i in 1:length(wpls))
{
  print(i)
  tt=readLines(wpls[i])
  dff[,i]=c(tt,rep(NA,max(ttx)-length(tt)))
}
save(namer,wpls,dff,file='~/namer.RData')

