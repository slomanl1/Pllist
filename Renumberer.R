scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('c:/my videos/rpdnclips')
load('~/mfnfo.RData') # load fnfo, lsst, wpls and xx
lsst=mfnfo$lsst
bn=gsub('[a-z|A-Z]','',lsst)
bc=gsub('[0-9]','',lsst)
bn=substr(bn,1,nchar(bn)-2)
namer=mfnfo
namer$bn=as.integer(bn)
namer$info=bc
namer=namer[order(namer$mtime),]
#namer=namer[order(namer$bn),]
namer$newfn=1:nrow(namer)
namer$newfn=paste(namer$newfn,'_REN',namer$info,sep='')
namer$ofn=NA
namer[file.exists(namer$lsst),]$ofn=namer[file.exists(namer$lsst),]$lsst
namer[file.exists(namer$newfn),]$ofn=namer[file.exists(namer$newfn),]$newfn

answ=gconfirm('RENAME .wpl - Are you Sure?')
if(answ){
  rnmd=file.rename(as.character(namer$ofn),as.character(namer$newfn))
  if(any(!rnmd))
  {
    print('File Rename Failed')
  }else{
    lsst=namer$newfn
    mfnfon=file.info(lsst)
    mfnfon$lsst=lsst
    mfnfog=merge(namer,mfnfo,by='lsst')
    mfnfog$lsst=mfnfog$newfn
    nms=subset(names(mfnfog),!grepl('.y',names(mfnfog)),fixed=TRUE)
    mfnfoh=mfnfog[,nms]
    names(mfnfoh)=gsub('.x','',names(mfnfoh),fixed=TRUE)
    mfnfo=mfnfoh[,names(mfnfo)]
    setwd(paste(pldrive,'My Playlists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    save(mfnfo,wpls,file='~/mfnfo.RData')
    save(namer,mfnfo,wpls,file=paste('~/namer',gsub(':','_',Sys.time(),fixed=TRUE),'.RData',sep=''))
    source('~/pllist.git/makeWPLS.R')
  }
}else{
  print('RENAME CANCELLED')
}


