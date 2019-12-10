getMetadata=function(lsst, saveResult=TRUE){
  lsst=as.character(lsst)
  ll=lsst
  print(paste('Getting metadata',len(lsst),'files'))
  load('~/mmeta.RData')
  mm=mm[file.exists(mm$fns),]
  lsst1=lsst[!lsst %in% mm$fns]
  lsst=unique(c(lsst1,lsst[!file.mtime(as.character(lsst)) %in% mm[mm$fns %in% lsst,'mtime']]))
  addfnfo=data.frame(lsst=as.character(lsst),stringsAsFactors = FALSE)
  zzz=NULL
  print(paste('Processing metadata',nrow(addfnfo),'files'))
  if(nrow(addfnfo)>1){
    writeLines(addfnfo$lsst,'~/EXCMD.TXT')
    ############### check why clean... produces 99 in filenames ########
    zzz=shell("exiftool -DMComment -CompressorID -XMPToolkit -Title -Subtitle -ImageSize -@ c:\\users\\larry\\documents\\EXCMD.TXT",intern = TRUE)
    zzz=c(' ',zzz)
  }else{
    if(nrow(addfnfo)>0){
      cmdd='zzz=shell("exiftool -DMComment -CompressorID -XMPToolkit -Title -Subtitle -ImageSize %s",intern = TRUE)'
      cmdx=sprintf(cmdd,normalizePath(addfnfo$lsst,winslash = '/'))
      eval(parse(text=cmdx))
      #if(len(zzz)==0) # NEW Clip
      zzz=c('',paste('========',addfnfo$lsst),zzz,'') # for factoring a single clip update
    }
  }
  #save(zzz,addfnfo,file='~/zzz.RData')
  print('Done getting metadata 1')
  
  zzz=zzz[1:(len(zzz)-1)]
  zzx=NULL
  
  if(len(zzz)>0){
    fx=as.numeric(as.factor(substr(zzz,1,8)))
    fz=fx[2]
    zzx=data.frame(fns=substr(zzz[fx==fz],10,1000),cmt=NA,format=NA,studio=NA,title=NA,subtitle=NA,stringsAsFactors = FALSE)
    zz=data.frame(zzz,idx=0,stringsAsFactors = FALSE)
    z=0
    for(x in 1:nrow(zz)){
      if(fx[x]==fz){
        z=z+1
      }
      zz[x,'idx']=z
    }
    ss=sapply(1:len(zzz),function(x){ zz[zz$idx==x,'zzz']})
    for(i in 1:nrow(zzx)){
      resp1=substr(ss[[i]][which(grepl('DM Comment',ss[[i]]))],35,1000)
      zzx[i,'cmt']=ifelse(len(resp1),resp1,'')
      resp2=substr(ss[[i]][which(grepl('Compressor ID',ss[[i]]))],35,1000)
      zzx[i,'format']=ifelse(len(resp2),resp2,'')
      resp3=substr(ss[[i]][which(grepl('XMP Toolkit',ss[[i]]))],35,1000)
      zzx[i,'studio']=ifelse(len(resp3),resp3,'')
      resp4=substr(ss[[i]][which(grepl('Title',ss[[i]]))],35,1000)
      zzx[i,'title']=ifelse(len(resp4),resp4,'')
      resp5=substr(ss[[i]][which(grepl('Subtitle',ss[[i]]))],35,1000)
      zzx[i,'subtitle']=ifelse(len(resp5),resp5,'')
      resp6=substr(ss[[i]][which(grepl('Image Size',ss[[i]]))],35,1000)
      zzx[i,'ImageSize']=ifelse(len(resp6),resp6,'')
    }
    zzx$mtime=file.mtime(zzx$fns)
  }
  zzx=rbind(zzx,subset(mm,mm$fns %in% ll))
  mm=zzx
  mm=mm[file.exists(mm$fns),]
  if(saveResult){
    print(paste('Saving',nrow(mm),'records of metadata'))
    save(mm,file='~/mmeta.RData')
  }
  print('Done getting metadata 2')
  return(zzx)
}
