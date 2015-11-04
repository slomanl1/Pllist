scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('~/')
load('~/fnfo.RData') # load fnfo, lsst, wpls and xx
lsst=fnfo$lsst
bn=gsub('[a-z|A-Z]','',lsst)
bc=gsub('[0-9]','',lsst)
bn=substr(bn,1,nchar(bn)-1)
namer=fnfo
namer$bn=bn
namer$info=bc
namer=namer[order(namer$mtime),]
namer$nfn=1:nrow(namer)
namer$nfn=gsub('.','_New.',paste(namer$nfn,namer$info,sep=''),fixed=TRUE)
lsst=namer$nfn
answ=gconfirm('RENAME .wpl - Are you Sure?')
if(answ){
  setwd('c:/my videos/rpdnclips')
  rnmd=file.rename(as.character(namer$lsst),as.character(namer$nfn))
  if(any(!rnmd))
  {
    print('File Rename Failed')
  }else{
    fnfon=file.info(lsst)
    fnfon$lsst=lsst
    fnfog=merge(namer,fnfo,by='lsst')
    fnfog$lsst=fnfog$nfn
    nms=subset(names(fnfog),!grepl('.y',names(fnfog)),fixed=TRUE)
    fnfoh=fnfog[,nms]
    names(fnfoh)=gsub('.x','',names(fnfoh),fixed=TRUE)
    fnfo=fnfoh[,names(fnfo)]
    setwd(paste(pldrive,'My NewPlaylists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    save(fnfo,wpls,file='~/fnfo.RData')
    save(namer,fnfo,fnfo,wpls,file=paste('~/namer',gsub(':','_',Sys.time(),fixed=TRUE),'.RData',sep=''))
    source('~/pllist/pllist.git/makeWPLS.R')
  }
}else{
  print('RENAME CANCELLED')
}


