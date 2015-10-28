scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('~/')
load('xxxx.RData') # load lsst, wpls and xx

bn=gsub('[a-z|A-Z]','',lsst)
bc=gsub('[0-9]','',lsst)
bn=substr(bn,1,nchar(bn)-1)
namer=data.frame(ofn=lsst,bn,info=bc,xx)
setwd('c:/my videos/rpdnclips')
namer$mtime=file.mtime(as.character(namer$ofn))
namer=namer[order(namer$mtime),]
namer$nfn=1:nrow(namer)
namer$nfn=paste(namer$nfn,namer$info,sep='')
lsst=namer$nfn
answ=gconfirm('RENAME .wpl - Are you Sure?')
if(answ){
  if(any(!file.rename(as.character(namer$ofn),as.character(namer$nfn))))
  {
    print('File Rename Failed')
  }else{
    save(xx,lsst,wpls,file='~/xxxx.RData')
    fnfo=file.info(lsst)
    save(fnfo,file='~/fnfo.RData')
    save(namer,fnfo,file=paste('~/namer',gsub(':','_',Sys.time(),fixed=TRUE),sep=''))
  }
}
