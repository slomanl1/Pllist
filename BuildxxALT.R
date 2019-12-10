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
  source('~/pllist.git/combiner.R')
  print('Combiner Done')
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  allall=c(dir(pattern='*.asf'),dir(pattern='*.wmv'))
  load('~/mfnfo.RData')
  
  setwd(paste(pldrive,'My Playlists',sep=""))
  wpls = sort(dir(pattern = '*.wpl'))
  
  lns  = NA
  xx=NULL
  lsst=NULL
  dx=data.frame(lss='',xx=0,wpl='')[0,] # create empty data frame structure
  
  for (i in 1:length(wpls)) {
    lss1=unique(readLines(wpls[i]))
    lss=NULL
    for(j in 1:length(lss1)){
      lss[j]=substr(lss1[j],regexpr('Clips',lss1[j])[1]+6,regexpr('asf|wmv',lss1[j])[1]+2)
    }
    dx=rbind(dx,data.frame(lss=lss,xx=2^(i-1),wpl=wpls[i],stringsAsFactors = FALSE,lss1=lss1))
  }
  dxu=dx[nchar(dx$lss)>0,]
  print('build dx Done')
  dx=dxu[order(as.numeric(gsub('[a-z]|_|-','',dxu$lss,ignore.case = TRUE))),]
  tl1=data.frame(table(dx$lss))
  tl=tl1[order(as.numeric(gsub('[a-z]|_|-','',tl1$Var1,ignore.case = TRUE))),]
  ts=1
  for(i in 2:(nrow(tl)+1))
  {
    ts[i]=ts[i-1]+tl[i-1,'Freq']
  }
  #ts[i+1]=ts[i]
  print('Build ts Done')
  xo=sapply(1:(nrow(tl)), function(x) dx[ts[x]:(ts[x+1]-1),'xx'])
  xn=sapply(1:(nrow(tl)), function(x) {
    xx=unlist(xo[[x]])
    xy=0
    for (i in 1:len(xx)) 
      xy=bitwOr(xy,xx[i]) 
    return(xy)
  }) 
  print('Build xo/xn Done')
  tl$xx=xn
  tl$lsst=sub('_REN','',tl$Var1)
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  print('Refreshing mfnfo')
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  mg1=merge(mfnfo,tl[,c('Var1','xx')],by.x='lsst',by.y='Var1',all.y=TRUE)
  mg1$xx=mg1$xx.y
  mfnfo=mg1[,names(mfnfo)]
  rn1=as.character(mfnfo$lsst)
  rn=rn1[file.exists(rn1)]
  fn=allall[(!allall %in% rn)]
  nms=names(mfnfo)
  fmt=file.mtime(mfnfo$lsst)
  fn=na.omit(c(fn,mfnfo[fmt!=mfnfo$mtime,'lsst']))
  if(len(fn)>0){
    print(paste('found',len(fn),'files to add'))
    mfnfo=mfnfo[fmt==mfnfo$mtime,]
    addfnfo=file.info(fn)
    print('done calculating file info')
    addfnfo$lsst=sub('REN_New','',fn)
    print('calculating md5sum')
    addfnfo$md5s=md5sum(addfnfo$lsst)
    print('done calculating md5sum')
    addfnfo$bn=gsub('[a-z|A-Z|_|-]','',addfnfo$lsst)
    addfnfo$bn=as.integer(trim(sub('.','',addfnfo$bn,fixed=TRUE)))
    addfnfo$studio=''
    addfnfo$cmt=''
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
      ############### check why clean... produces 99 in filenames ########
      zzz=shell("exiftool -DMComment -XMPToolkit -@ c:\\users\\larry\\documents\\EXCMD.TXT",intern = TRUE)
      zzz=c(' ',zzz)
    }else{
      cmdd='zzz=shell("exiftool -DMComment -XMPToolkit %s",intern = TRUE)'
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
    source('~/pllist.git/makeWPLS.R')
  }else{
    galert('No new records found',10)
    print('No new records found')
    save(mfnfo,wpls,file='~/mfnfo.RData')
  }
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  mfnfo=mfnfo[file.exists(mfnfo$lsst),]
  mfnfo=mfnfo[!duplicated(mfnfo$lsst),]
  mfnfo$mtime=file.mtime(mfnfo$lsst) # UPDATE mtimes
  mfnfo=mfnfo[,1:13]
  save(mfnfo,wpls,file='~/mfnfo.RData')
  source('~/pllist.git/makeunique.R')
  print('ORDERALL')
  source('~/pllist.git/orderallwpl.R')
  print('Build Done')
}

