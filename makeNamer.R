scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
library(tools)
setwd('~/')
shell('dir "C:\\My Videos\\RPDNClips" /od/b > ddd.txt')
lns=readLines('ddd.txt')
unlink('ddd.txt')
setwd('C:/My Videos/RPDNClips')
fnfn=file.info(lns)
fnfn$matcher=paste(fnfn$mtime,fnfn$size)
fnfn$fname=rownames(fnfn)
load('~/fnfoold.RData') # old fnfo
fnfo$fname=rownames(fnfo)
fnfo$matcher=paste(fnfo$mtime,fnfo$size)
mtch=merge(fnfn,fnfo,by='matcher')
mth=mtch[,c('fname.y','fname.x')]
names(mth)=c('oldfn','newfn')
load('~/xxxxold.RData')
mgg=merge(mth,data.frame(oldfn=lsst,xx),by='oldfn')
save(mth,mgg,file='mth.RData')
lsst=mgg$newfn
xx=mgg$xx
missing=lns[which(!lns %in% lsst)]
save(lsst,removers,wpls,xx,missing,file='~/xxxx.RData')

fmfo=file.info(missing)
fmfo$fn=rownames(fmfo)
fnfo$fn=rownames(fnfo)
sz=fmfo[fmfo$size %in% fnfo$size,c(1,4,5)]
mgm=merge(fmfo,fnfo,by='size')
mgx=mgm[,c('fn.x','fn.y','size','mtime.x','mtime.y','ctime.x','ctime.y')]

