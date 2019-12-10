scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/editClipName.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)

if (file.exists('c:/my playlists/missing.M3U'))
  file.remove('c:/my playlists/missing.M3U')

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  allall=c(dir(pattern='*.asf'),dir(pattern='*.wmv'))
  load('~/mfnfo.RData')
  rn1=as.character(mfnfo$lsst)
  rn=rn1[file.exists(rn1)]
  fn=allall[(!allall %in% rn)]
  nms=names(mfnfo)
  fmt=file.mtime(mfnfo$lsst)
  fn=na.omit(c(fn,mfnfo[fmt!=mfnfo$mtime,'lsst']))
  if(len(fn)>0){
    print(paste('found',len(fn),'files to add - calculating file info'))
    mfnfo=mfnfo[fmt==mfnfo$mtime,]
    addfnfo=file.info(fn)
    print('done calculating file info')
    addfnfo$lsst=sub('REN_New','',fn)
    print('calculating md5sum')
    addfnfo$md5s=md5sum(addfnfo$lsst)
    print('done calculating md5sum')
    addfnfo$bn=beaner(addfnfo$lsst)
    addfnfo$studio=''
    addfnfo$cmt=''
    addfnfo$Obn=0
    # addfnfo=addfnfo[,names(mfnfo)]
    # if file renamed just replace xx with xx of old file before rename(otherwise it was added)
    for(i in 1:nrow(addfnfo)){
      print(paste(i,'/',nrow(addfnfo)))
      addfnfo[i,'xx']=cleanwplsfn(addfnfo[i,'lsst'])$xx
      newxx=mfnfo[which(mfnfo$md5s==addfnfo[i,'md5s']),'xx']
      if(len(newxx)!=0){
        print(paste(addfnfo[i,'lsst'],'added by BuildALT'))
        addfnfo[i,'xx']=newxx[1]
      }
    }
    print('Getting metadata')
    if(nrow(addfnfo)>1){
      writeLines(addfnfo$lsst,'~/EXCMD.TXT')
      zzz=shell("exiftool -DMComment -XMPToolkit -GPSLatitude -@ c:\\users\\larry\\documents\\EXCMD.TXT",intern = TRUE)
      zzz=c(' ',zzz)
    }else{
      cmdd='zzz=shell("exiftool -DMComment -XMPToolkit -GPSLatitude %s",intern = TRUE)'
      cmdx=sprintf(cmdd,normalizePath(addfnfo$lsst,winslash = '/'))
      eval(parse(text=cmdx))
      #if(len(zzz)==0) # NEW Clip
      zzz=c('',paste('========',addfnfo$lsst),zzz,'') # for factoring a single clip update
    }
    save(zzz,addfnfo,mfnfo,file='~/zzz.RData')
    print('Done getting metadata')
    
    zzz=zzz[1:(len(zzz)-1)]
    if(len(zzz)>0){
      fx=as.numeric(as.factor(substr(zzz,1,8)))
      zzx=data.frame(fns=substr(zzz[fx==2],10,1000),cmt=NA,studio=NA,stringsAsFactors = FALSE)
      zz=data.frame(zzz,idx=0,stringsAsFactors = FALSE)
      z=0
      for(x in 1:nrow(zz)){
        if(fx[x]==2){
          z=z+1
        }
        zz[x,'idx']=z
      }
      ss=sapply(1:len(zzz),function(x){ zz[zz$idx==x,'zzz']})
      for(i in 1:nrow(zzx)){
        resp1=substr(ss[[i]][which(grepl('DM Comment',ss[[i]]))],35,1000)
        zzx[i,'cmt']=ifelse(len(resp1),resp1,'')
        resp2=substr(ss[[i]][which(grepl('XMP Toolkit',ss[[i]]))],35,1000)
        zzx[i,'studio']=ifelse(len(resp2),resp2,'')
        resp3=substr(ss[[i]][which(grepl('GPS Latitude',ss[[i]]))],35,1000)
        zzx[i,'Obn']=ifelse(len(resp3),as.integer(strsplit(resp3,'deg')[[1]][1]),'')        
      }
      mgg=merge(addfnfo[,nms[1:11]],zzx,by.x='lsst',by.y='fns')
    }else{
      mgg=addfnfo
    }
    mfnfo=rbind(mfnfo[,nms],mgg[,nms])
    mfnfo$mtime=file.mtime(mfnfo$lsst)
    tod=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    lastr=nrow(mfnfo)
    mfnfo[(lastr-len(fn)+1):lastr,c("mtime","ctime","atime")]=tod
    save(mfnfo,wpls,file='~/mfnfo.RData')
    galert(paste(nrow(addfnfo),'records added in addfnfo'))
  }else{
    galert('No new records found in buildmfnfo',10)
    print('No new records found in buildmfnfo')
    save(mfnfo,wpls,file='~/mfnfo.RData')
  }
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  mfnfo=mfnfo[file.exists(mfnfo$lsst),]
  mfnfo=mfnfo[!duplicated(mfnfo$lsst),]
  mfnfo$mtime=file.mtime(mfnfo$lsst) # UPDATE mtimes
  mfnfo=mfnfo[,1:14]
  mfnfo$bn=beaner(mfnfo$lsst)
  mfnfo=mfnfo[order(mfnfo$bn),]
  row.names(mfnfo)=1:nrow(mfnfo)
  save(mfnfo,wpls,file='~/mfnfo.RData')
  print('Build Done')
}

