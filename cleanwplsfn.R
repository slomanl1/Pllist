cleanwplsfn=function(fn){
  fnin=fn
  if(!exists('wpls'))
    load('~/mfnfo.RData')
  wplsa=gsub('.wpl','',wpls,fixed=TRUE)
  print(paste('cleanwplsfn fn=',fn))
  fn=basename(fn)
  studio=''
  posb=gregexpr('_',fn)[[1]][1]
  if(posb>0){
    studio=substr(fn,posb+1,nchar(fn)-4)
    fn=substr(fn,1,posb-1)
  }
  fx=''
  if(grepl('blah',fn)){
    fx='blah.wpl'
  }else{
    if(grepl('ah',fn))
      fx='ah.wpl'
  }
  if(grepl('bfa',fn)){
    fx=paste(fx,'bfa.wpl')
  }else{
    if(grepl('b',fn)&!grepl('b',fx)&!grepl('sbd',fn))
      fx=paste(fx,'b.wpl')
  }
  if(grepl('bl',fn) & !grepl('bl',fx))
    fx=paste(fx,'bl.wpl')
  if(grepl('ussfa',fn)){
    fx=paste(fx,'ussfa.wpl')
  }
  if(grepl('ussfd',fn)){
    fx=paste(fx,'ussfd.wpl')
  }
  if(grepl('ussf',fn) & !grepl('ussf',fx)){
    fx=paste(fx,'ussf.wpl')
  }
  if(grepl('dd',fn))
    fx=paste(fx,'dd.wpl')
  if(grepl('su',fn))
    fx=paste(fx,'su.wpl')
  if(grepl('pn',fn))
    fx=paste(fx,'pn.wpl')
  if(grepl('sbd',fn))
    fx=paste(fx,'sbd.wpl')
  if(grepl('dg',fn))
    fx=paste(fx,'dg.wpl')
  if(grepl('so',fn))
    fx=paste(fx,'so.wpl')
  if(grepl('utpfd',fn)&!grepl('utp',fx)){
    fx=paste(fx,'utpfd.wpl')
  }else{
    if(grepl('utp',fn)&!grepl('utpfd',fx)){
      fx=paste(fx,'utp.wpl')
    }else{
      if(grepl('stp',fn)){
        fx=paste(fx,'stp.wpl')}
    }
  }
  if(grepl('vva',fn)){
    fx=paste(fx,'vva.wpl')
  }else{
    if(grepl('uwa',fn)){
      fx=paste(fx,'uwa.wpl')
    }else{
      if(grepl('swa',fn)){
        fx=paste(fx,'swa.wpl')
      }else{
        if(grepl('wa',fn)){
          fx=paste(fx,'wa.wpl')
        }
      }
    }
  }
  if(grepl('cs',fn)){
    fx=paste(fx,'cs.wpl')}
  
  bn=beaner(fn)
  if(is.na(bn))
    print('bn calcs to NA')
  if(nchar(studio)>0){
    fname=gsub(' ','',paste(bn,gsub('.wpl|_','',fx),'_',studio,'.',file_ext(fnin),sep=''))
  }else{
    fname=gsub(' ','',paste(bn,gsub('.wpl|_','',fx),'.',file_ext(fnin),sep='')) 
  }
  fy=trim(gsub('.wpl','',fx))
  fsp=unlist(strsplit(fy,' '))
  xx=0
  for(b in 1:len(wplsa)){
    if(wplsa[b] %in% fsp){
      xx=bitwOr(xx,2^(b-1))
    }
  }
  
  retval=list(fy=fy,fx=fx,bn=bn,fname=fname,xx=xx)
  return(retval)
}

cleanUmUp = function(){
  resr=0
  source("~/Local.R")
  setwd('~/')
  setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
  fnx=dir('D:/PNMTALL/RPDNClips')
  load('~/mfnfo.RData')
  pb=winProgressBar(min=1,max=nrow(mfnfo))
  for(fn in mfnfo$lsst){
    fy=cleanwplsfn(fn)
    mfnfo[which(fn==mfnfo$lsst),'info']=fy$fy
    mfnfo[which(fn==mfnfo$lsst),'xx']=fy$xx
    setWinProgressBar(pb,which(mfnfo$lsst %in% fn))
    #print(which(mfnfo$lsst %in% fn))
  }
  close(pb)
  mfnfo$nfn=paste(mfnfo$bn,mfnfo$info,'_',mfnfo$studio,'.',file_ext(mfnfo$lsst),sep='')
  mfnfo$nfn=gsub(' ','',mfnfo$nfn)
  
  mrange=which(!mfnfo$nfn==mfnfo$lsst)
  ofnn=mfnfo[mrange,c('lsst')]
  nfnn=mfnfo[mrange,c('nfn')]
  if(len(mrange) & gconfirm('Rename Files')){
    resr=file.rename(ofnn,nfnn)
    mfnfo[mrange,c('lsst')]=mfnfo[mrange,c('nfn')]
    galert(paste(sum(resr),'/',len(mrange),'files renamed'))
  }else{
    galert('NO FILES RENAMED')
  }
  mfnfo=mfnfo[,names(mfnfo)[1:13]]
  rg1=as.integer(nrow(mfnfo)/2):nrow(mfnfo)
  rg2=1:(rg1[1]-1)
  mfnfo[rg1,'xx']=bitwOr(mfnfo[rg1,'xx'],2^23) # wa1
  mfnfo[rg2,'xx']=bitwOr(mfnfo[rg2,'xx'],2^22) # wa
  source('~/pllist.git/Buildmfnfo.R')
}



