scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function soruce

load('~/mfnfo.rdata')
dx=subset(mfnfo,nchar(studio)==0|is.na(studio)|studio=='703'|studio=='Bami2')
if(nrow(dx)>0){
  
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  st=''
  for(i in 1:nrow(dx)){
    print(i)
    fnc=dx[i,'lsst']
    cmdd=sprintf('shell("exiftool -XMPToolkit %s",intern=TRUE)',fnc)
    print(cmdd)
    zz=eval(parse(text=cmdd))
    if(len(zz)>0)
      st[i]=zz
  }
  
  stt=data.frame(lsst=dx$lsst,studio=st)
  stt$studio=gsub('XMP Toolkit                     : ','',stt$studio)
  mgg=merge(mfnfo,stt,by='lsst',all.x=TRUE)
  for(i in 1:nrow(mgg)){
    if(is.na(mgg[i,'size']))
      next
    if(nchar(mgg[i,'studio.x'])==0|is.na(mgg[i,'studio.x'])){
      mgg[i,'studio.x']=mgg[i,'studio.y']
    }
  }
  mgg1=mgg[,1:len(names(mfnfo))]
  #names(mgg1)=names(mfnfo)
  mgg2=getWplsXX(mgg1$lsst,mgg1)
  names(mgg2)=sub('.x','',names(mgg2),fixed=TRUE)
  mfnfo=mgg2
  save(mfnfo,wpls,file='~/mfnfo.RData')
}else{
  galert('NONE FOUND')
}
