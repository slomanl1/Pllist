source("~/Pllist.git/RemoveAllExceptFuncs.R") #clear bones
load('~/dfan.Rdata')
dff=subset(dfan,grepl('Yoms',DMComment)&grepl('RPDNClips',filename))
ss=strsplit(dff$DMComment,' ')
cc=sapply(1:len(ss), function(x) which(grepl('Yoms',ss[[x]])))
Yoms=data.frame(fn=dff$filename,nYoms=NA,YpluMin=NA)
Yoms$nYoms =as.integer(as.character(sapply(1:len(ss), function(x) ss[[x]][unlist(cc[x])-1] )))
Yoms$YpluMin =as.character(sapply(1:len(ss), function(x) ss[[x]][unlist(cc[x])] ))
Yoms$bn=beaner(Yoms$fn)
