scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd('~/')
load('xxxx (2).RData')
setwd('c:/my videos/rpdnclips')
dff=data.frame(lsst=lsst,xx[1:11435])
fnfx=file.info(as.character(dff$lsst))
dfg=cbind(dff,fnfx)
colnames(dfg)[2]='xx'

# goal find xx for each file in current dir, match with size and mtime
fnfo=file.info(dir())
fnfo$matcher=paste(fnfo$size,fnfo$mtime)
fnfo$fname=rownames(fnfo)
dfg$matcher=paste(dfg$size,dfg$mtime)
mgg=merge(dfg,fnfo,by='matcher')
save(mgg,file='~/mgg.RData')
