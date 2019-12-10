scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/WriteDate.R')

addTOmfnfo = function(mm){
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  fn=mm$newfn
  print(paste('addTomfnfo in ProcessWA adding',fn,'studio',mm$studio))
  addfnfo=file.info(fn)
  addfnfo$lsst=basename(fn)
  addfnfo$md5s=md5sum(fn)
  addfnfo$xx=2^(length(wpls)-1) # wa1.wpl
  addfnfo$bn=mm$bn
  addfnfo$studio=mm$studio
  addfnfo$cmt=mm$cmt
  addfnfo=addfnfo[,names(mfnfo)]
  nx=0
  idxp=which(grepl(mm$lsst,ppk))[1]
  mz=unlist(strsplit(ppk,' ')[[idxp]])
  mz=mz[nchar(mz)>0]
  if(len(mz)==1)
    return()
  playlists=mz[2:len(mz)]
  for(ww in playlists){
    nx=bitwOr(nx,2^(which(ww==wpls)-1))
  }
  addfnfo$xx=bitwOr(addfnfo$xx,nx)
  .GlobalEnv$mfnfo=.GlobalEnv$mfnfo[.GlobalEnv$mfnfo$lsst!=fn,]
  .GlobalEnv$mfnfo=rbind(.GlobalEnv$mfnfo,addfnfo)
  tod=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  lastr=nrow(mfnfo)
  .GlobalEnv$mfnfo[(lastr-len(fn)+1):lastr,c("mtime","ctime","atime")]=tod
}

load('~/ppk.RData') # from choosWA.R
load('~/mfnfo.rdata') # load mfnfo,wpls
mfnfo=mfnfo[!duplicated(mfnfo$lsst),]
save(mfnfo,wpls,file='~/mfnfo.RData')

fn=NULL
fnx=NULL
wpp=NULL
k=1
pb=winProgressBar(min=0,max=len(ppk))
for( i in 1:len(ppk)){
  setWinProgressBar(pb,i,"Processing ppk")
  fn[i]=gsub('.wpl','',unlist(strsplit(ppk[i],' ')))[[1]]
  if(!file.exists(fn[i])){
    print(paste(fn[i],'Not FOUND'))
    next
  }
  jelly=gsub('NA','',capture.output(cat(sort(gsub('.wpl','',unlist(strsplit(ppk,' ')[[i]]))[2:22]),sep='')))
  if(len(jelly)){
    wpp[k]=jelly
    fnx[k]=fn[i]
    k=k+1
  }
}
close(pb)
if(k>1){
  dfp=data.frame(fn=fnx,wpp,stringsAsFactors = FALSE)
  #dfp$newfn=paste(file_path_sans_ext(dfp$fn),dfp$wpp,'.',file_ext(dfp$fn),sep='')
  dfp$mtime=file.mtime(dfp$fn)
  dfp$lsst=basename(dfp$fn)
  mm=merge(mfnfo[,c("md5s" ,"lsst", "xx","studio")],dfp,by='lsst')
  rm(mfnfo)
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  load('~/mfnfo.RData')
  mm$bn=gsub('[a-z|A-Z]','',mm$lsst)
  mm$bn=trim(sub('.','',mm$bn,fixed=TRUE))
  mm$newfn=paste(mm$bn,mm$wpp,'.',file_ext(mm$lsst),sep='')
  rss=file.rename(mm$fn,mm$newfn)
  pb=winProgressBar(min=0,max=nrow(mm))
  for(ix in 1:nrow(mm)){
    setWinProgressBar(pb,ix)
    WriteDate(mm$newfn[ix],mm$mtime[ix])
    addTOmfnfo(mm[ix,])
  }
  close(pb)
  save(mfnfo,wpls,file='~/mfnfo.RData')
  galert(paste(sum(rss),'FILES RENAMED'))
  source('~/pllist.git/makeWPLS.R')
}else{
  galert('None Found')
}

 
  
