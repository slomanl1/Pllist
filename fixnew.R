source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
cd('D:/pnmtall/rpdnClips')
load('~/mfnfo.RData')
zz=dir()
fn=mfnfo$lsst
fn=zz[which(!zz %in% fn)]
addfnfo=file.info(fn)
if(nrow(addfnfo)>0){
  addfnfo$lsst=sub('REN_New','',fn)
  addfnfo$xx=0 # not in any playlist
  addfnfo$md5s=md5sum(addfnfo$lsst)
  addfnfo$bn=gsub('[a-z|A-Z|_|-]','',addfnfo$lsst)
  addfnfo$bn=as.integer(trim(sub('.','',addfnfo$bn,fixed=TRUE)))
  addfnfo$studio=''
  addfnfo$cmt=''
  
  mm=rbind(mfnfo,addfnfo)
  mfnfo=mm
  save(mfnfo,wpls,file='~/mfnfo.RData')
  
  setwd('D:/PNMTALL/rpdnclips')
  load('~/mfnfo.RData')
  fncs=mfnfo[is.na(mfnfo$studio)|nchar(mfnfo$studio)==0,'lsst']
  k=1
  joe=NA
  pb=winProgressBar(min=1,max=len(fncs),title = 'getting studio')
  for(fnc in fncs){
    print(fnc)
    setWinProgressBar(pb,k,label=as.character(k))
    cmdv=paste("shell('exiftool -XMPToolkit %s',intern=TRUE)")
    xx=eval(parse(text=sprintf(cmdv,normalizePath(fnc,winslash = '/'))))
    if(len(xx)>0)
      joe[k]=xx
    k=k+1
  }
  close(pb)
  
  stt=data.frame(lsst=fncs,studio=trim(joe),stringsAsFactors = FALSE)
  stt$studio=gsub('XMP Toolkit                     : ','',stt$studio,fixed=TRUE)
  mgg=merge(mfnfo,stt,by='lsst',all.x=TRUE)
  mgg[is.na(mgg$studio.x)|nchar(mfnfo$studio)==0,'studio.x']=mgg[is.na(mgg$studio.x)|nchar(mfnfo$studio)==0,'studio.y']
  mgg$studio=mgg$studio.x
  mfnfo=mgg[,names(mfnfo)]
  save(mfnfo,wpls,file='~/mfnfo.RData')
  xxm=getWplsXX(stt$lsst,mfnfo)
  mfnfo=xxm
  save(mfnfo,wpls,file='~/mfnfo.RData')
}else(
  galert('NO MISSING FILES in MFNFO')
)
  
