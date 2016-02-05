scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('c:/MyVideos/rpdnclips')
load('~/mfnfo.RData') # load fnfo, lsst, wpls and xx
lsst=mfnfo$lsst
lsstn=gsub('_REN','',lsst)
bn=gsub('[a-z|A-Z]','',lsstn)
bc=gsub('[0-9]','',lsstn)
bn=substr(bn,1,nchar(bn)-2)
namer=mfnfo
namer$mtime=file.mtime(mfnfo$lsst)
namer$mlsst=namer$lsst
namer$lsst=gsub('_REN','',namer$lsst)
namer$bn=as.integer(bn)
namer$info=bc
namer$ofn=NA
namer=namer[!duplicated(namer$lsst),]

while(any(duplicated(namer$mtime))){ # ensure that no two entires are equal, sep by 1 second
  namer$mtime=namer$mtime+duplicated(namer$mtime)
}
namer=namer[order(as.character(namer$mtime)),]
namer$newfn=1:nrow(namer)
namer$newfn=paste(namer$newfn,'_REN',namer$info,sep='')
namer[file.exists(namer$lsst),]$ofn=namer[file.exists(namer$lsst),]$lsst
namer[file.exists(namer$newfn),]$ofn=namer[file.exists(namer$newfn),]$newfn
rens=which(grepl('_REN',namer$ofn))

if(len(rens))
  namer[rens,]$newfn=gsub('_REN','',namer[rens,]$newfn)
namer[which(!file.exists(namer$ofn)),'ofn']=namer[which(!file.exists(namer$ofn)),'mlsst']
answ=gconfirm('RENAME .wpl - Are you Sure?')
if(answ){
  rnmd=file.rename(as.character(namer$ofn),as.character(namer$newfn))
  save(namer,mfnfo,wpls,file=paste('~/namer',gsub(':','_',Sys.time(),fixed=TRUE),'.RData',sep=''))
  if(any(!rnmd))
  {
    print('File Rename Failed')
  }else{
    mfnfog=merge(namer,mfnfo,by.x='ofn',by.y='lsst')
    mfnfog$lsst=mfnfog$newfn
    nms=subset(names(mfnfog),!grepl('.y',names(mfnfog)),fixed=TRUE)
    mfnfoh=mfnfog[,nms]
    names(mfnfoh)=gsub('.x','',names(mfnfoh),fixed=TRUE)
    mfnfo=mfnfoh[,names(mfnfo)]
    setwd(paste(pldrive,'My Playlists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    save(mfnfo,wpls,file='~/mfnfo.RData')
    source('~/pllist.git/makeWPLS.R')
  }
}else{
  print('RENAME CANCELLED')
}


