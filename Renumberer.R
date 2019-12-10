scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd('D:/PNMTALL/rpdnclips')
load('~/mfnfo.RData') # load fnfo, lsst, wpls and xx
lsst=mfnfo$lsst
lsstn=gsub('_REN','',lsst)
bn=gsub('[a-z|A-Z]','',lsstn)
bn=as.integer(trim(sub('.','',bn,fixed=TRUE)))
bc=gsub('[0-9]','',lsstn)

namer=mfnfo
namer$mtime=file.mtime(mfnfo$lsst)
namer$mlsst=namer$lsst
namer$lsst=gsub('_REN','',namer$lsst)
namer$bn=bn
namer$info=bc
namer$ofn=NA
namer=namer[!duplicated(namer$lsst),]

while(any(duplicated(namer$mtime))){ # ensure that no two entires are equal, sep by 1 second
  namer$mtime=namer$mtime+duplicated(namer$mtime)
  print(sum(duplicated(namer$mtime)))
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
  save(namer,file='~/namer.RData')
  if(any(!rnmd))
  {
    print('File Rename Failed')
  }else{
    print('File Rename 1 Success')
    ofn=as.character(namer$newfn)
    nfn=sub('_REN','',ofn)
    rnmd2=file.rename(ofn,nfn)
    if(any(!rnmd2))
    {
      print('File Rename 2 Failed')
    }else{
      print('File Rename 2 Success')
      mfnfog=merge(namer,mfnfo,by.x='ofn',by.y='lsst')
      mfnfog$lsst=mfnfog$newfn
      nms=subset(names(mfnfog),!grepl('.y',names(mfnfog)),fixed=TRUE)
      mfnfoh=mfnfog[,nms]
      names(mfnfoh)=gsub('.x','',names(mfnfoh),fixed=TRUE)
      mfnfo=mfnfoh[,names(mfnfo)]
      mfnfo$lsst=sub('_REN','',mfnfo$lsst)
      setwd(paste(pldrive,'My Playlists',sep=""))
      wpls = sort(dir(pattern = '*.wpl'))
      save(mfnfo,wpls,file='~/mfnfo.RData')
      source('~/pllist.git/makeWPLS.R')
      load('~/bads.RData')
      load('~/namer.RData')
      cd('D:/PNMTALL/rpdnclips')
      mgg=merge(namer[,c('newfn','md5s')],bads,by='md5s')
      mgg$fname=sub('_REN','',normalizePath(mgg$newfn,winslash = '/'))
      bads=mgg[,names(bads)[1:3]]
      save(bads,file='~/bads.RData')
      source('~/pllist.git/fixnew.R')
    }
  }
}else{
  print('RENAME CANCELLED')
}

cd('D:/PNMTALL/rpdnclips')
load('~/dfan.RData')
load('~/namer.RData')
load('~/mfnfo.RData')
namer$newfn=sub('_REN','',namer$newfn)
dfan$lsst=sub('_REN','',basename(dfan$filename))
mgg=merge(namer,dfan,by='lsst',all.y=TRUE)
mg2=merge(dfan,mgg[,c('ofn','newfn','md5s')],by.x='lsst',by.y='ofn',all.x=TRUE)
df1=mg2[,names(dfan)[1:6]]
df1$filename=as.character(normalizePath(df1$filename,winslash = '/'))

df=basename(dfan$filename)
olds=data.frame(fn=namer[which(!namer$ofn %in% df),c('ofn','newfn')],stringsAsFactors = FALSE)

load('~/PNMTALL.RData') # load am, ttl and dts (sfname in MakeDfan...)
ax=data.frame(fn=am[ttl],stringsAsFactors = FALSE)
ax$fn=(trim(gsub('=','',ax$fn))) # remove ==== and extra space
mgax=merge(olds,ax,by.x='fn.ofn',by.y='fn')
###### write ===== and file name with path here to am
dfan=df1
#save(dfan,file='~/dfan.RData')
#writeLines(am,'~/allmetadata.txt')
bn=as.integer(gsub('[a-z|A-Z|_|-]','',dir()));
ddpp=dups(bn)
if(len(ddpp)>0)
  galert('Duplicate BNs Found')









