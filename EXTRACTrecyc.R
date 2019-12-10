wv=function(outx,ttl='RECYCLE BIN CONTENTS, (Close to IGNORE)'){
  ww=gwindow(title=ttl,width=1300,height=min(100+(nrow(outx)*30),750))
  ggp=ggroup(cont=ww,horiz=FALSE)
  gtable(outx,cont=ggp)
  ggh=ggroup(cont=ggp)
  addSpring(ggh)
  gbutton('DELETE',cont=ggh,handler=function(h,...){
    .GlobalEnv$confirmed=TRUE
    dispose(ww)
    gtkMainQuit()
  })
  gbutton('CANCEL',cont=ggh,handler=function(h,...){
    .GlobalEnv$confirmed=FALSE
    dispose(ww)
    gtkMainQuit()
  })
  addHandlerDestroy(ww,handler=function(h,...) gtkMainQuit())
  addSpring(ggh)
  gtkMain()
}

confirmed=FALSE
cdd=cd()
cd('C:/$RECYCLE.BIN')
dd=dir(path='C:/$RECYCLE.BIN',recursive = TRUE,full.names = TRUE)
dd=c(dd,dir(path='D:/$RECYCLE.BIN',recursive = TRUE,full.names = TRUE))
if(len(dd)){
  ddd=subset(dd,grepl('$I',dd,fixed=TRUE))
  oxn=subset(dd,!grepl('$I',dd,fixed=TRUE))
  outx=NULL
  sizz=NULL
  if(len(ddd)){
    for(x in 1:len(ddd)){
      tt=shell(sprintf('hexdump "%s"',ddd[x]),translate=TRUE,intern = TRUE)
      kk=NA
      for(y in 1:len(tt)){
        ss=strsplit(tt[y],'|',fixed=TRUE)[[1]][2]
        kk[y]=gsub(' ','',ss)
      }
      outx[x]=sub('character(0)','',(capture.output(print(paste(cat(subset(kk,!is.na(kk)),sep=''),sep='')))),fixed=TRUE)
      outx[x]=paste(substr(ddd[x],1,2),outx[x],sep='')
      pos=unlist(gregexpr(':',outx[x]))
      outx[x]=substr(outx[x],pos[2]-1,nchar(outx[x]))
      sizz[x]=file.size(oxn[x])
    }
    
    outx1=c(outx,oxn)
    sizz=c(sizz,file.size(oxn))
    outx=data.frame(outx=outx1,fileSize=ifelse(nchar(file_ext(dd))>0,sizz,''),dd=dd,stringsAsFactors = FALSE)
    
    outx=outx[file.exists(dd) & !is.na(outx$outx),]
  }
  cd(cdd)
  if(!is.null(outx)){
    poss=sapply(1:len(dd),function(x) tail(unlist(gregexpr('/',dd[x])),1)) # last backslash
    dx=data.frame(dd,ax=file_path_sans_ext(substr(dd,poss,1000)),fs=file.size(dd),stringsAsFactors = FALSE)
    dx$rx=ifelse(grepl('$R',dx$ax,fixed=TRUE),substr(dx$ax,4,1000),'')
    dx$ix=ifelse(grepl('$I',dx$ax,fixed=TRUE),substr(dx$ax,4,1000),'')  
    dx1=dx[,c("dd" ,"ax" ,"fs","ix")]
    mgg=merge(dx,dx1,by.x='rx',by.y='ix',incomparables ='',all.x=TRUE)
    mgx=merge(outx,mgg,by.x='dd',by.y='dd.x')
    
    
    dirs=subset(mgx,nchar(file_ext(dd))==0& !grepl('RECYCLE.BIN',outx))[,c('outx','ix')]
    if(nrow(dirs)){
      dirs$outx=normalizePath(dirs$outx,mustWork = FALSE,winslash = '/')
      
      tt=NULL
      for(x in 1:nrow(dirs)){
        idxs=which(grepl(dirs$ix[x],mgx$outx))
        mgx[idxs,'outx']=(paste(dirs[x,'outx'],basename(mgx[idxs,'outx']),sep='/'))
      }
    }
    
    mffa=subset(mgx,!grepl('RECYCLE.BIN',outx))
    mff=mffa[,c('outx','fileSize')]
  }else{
    mff=dx[,c('dd','fs')]
  }
  wv(mff,'FINAL VALUES')
}else{
  galert('RECYCLE BIN is ALREADY EMPTY')
}
