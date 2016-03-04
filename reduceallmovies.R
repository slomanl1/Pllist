svv=function(filename,errorCode) {
  if(file.exists('~/bads.RData'))
    load('~/bads.RData')
  badsa=data.frame(fname=NA,errorC=NA,md5s=NA)
  badsa$fname=filename
  badsa$errorC=errorCode
  badsa$md5s=md5sum(filename)
  bads=rbind(bads,badsa)
  print(badsa$errorC)
  save(bads,file='~/bads.RData')
}

getDur = function(svt) {
  dur=NA
  for(i in 1:len(svt)){
    xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                   svt[i],'" ' ,sep=''),translate = TRUE, intern = TRUE)
    durx=paste(subset(xx,grepl('Format  ',xx))[2],subset(xx,grepl('Duration  ',xx))[1])
    dur[i]=gsub('  ','',durx)
  }
  return(dur)
}

file.remove(dir(pattern = 'file'))
cd('~/')
cls=NULL
dwnlds=NULL
choices=c('D:/PNMTALL','C:/PNMTALL','C:/MyVideos/RPDNclips','c:/PNMTALL/NewDownloads','REDUCE only')
sll=select.list(choices,multiple=TRUE,graphics = TRUE)
if(len(sll)>0){
  slx=which(choices %in% sll)
  if(1 %in% slx)
    cls=c(cls,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
  if(2 %in% slx){
    cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
    cls=cls[!grepl('c:/PNMTALL/NewDownloads',cls)]}
  if(3 %in% slx)
    cls=c(cls,dir('c:/MyVideos/rpdnclips',recursive = TRUE,full.names = TRUE))
  if(4 %in% slx)
    dwnlds=dir('c:/PNMTALL/NewDownloads',recursive = TRUE,full.names = TRUE)
  cls=c(cls,dwnlds)
  if(5 %in% slx)
    cls=cls[grepl('REDUCE',cls) |grepl('MyVideo',cls)]
  
  cls=cls[which(!grepl('crdownload|ini|_REN',cls))]
  fna=cls # all files (including _New's)
  cls=cls[which(!grepl('_New',cls))]
  if(len(cls)>0){
    cla=fna[which(grepl('_New',fna))]
    dfn=data.frame(cla)
    dfn$cls=sub('_New','',dfn$cla)
    dfn$sz=file.size(dfn$cls)
    dfn=dfn[order(dfn$sz),]
    dfn=dfn[!is.na(dfn$sz),]
    
    nfns=paste(file_path_sans_ext(cls),'_New.',file_ext(cls),sep='')
    dfa=data.frame(cls,sz=file.size(cls),nfns,durF=NA)
    dfa$szp=ptn(dfa$sz)
    
    dfa=subset(dfa,!file.exists(as.character(dfa$nfns))) # remove already converted to _New
    dfa=dfa[order(dfa$sz,decreasing=FALSE),]
    nfns=paste(file_path_sans_ext(cls),'_New.',file_ext(cls),sep='')
    dfa1=data.frame(cls,sz=file.size(cls),nfns,durF=NA)
    dfa1$szp=ptn(dfa1$sz)
    dfa1=subset(dfa1,!file.exists(as.character(dfa1$nfns))) # remove already converted to _New
    dfa=rbind(dfa1,dfa)
    
    bads=data.frame(fname=NA,errorC=NA,md5s=NA)
    
    if(file.exists('~/bads.RData')){
      load('~/bads.RData')
    }
    
    dfa=dfa[!toupper(dfa$cls) %in% toupper(bads$fname),]
    if(!file.exists('~/msgis.txt')){
      writeLines('','~/msgis.txt')
    }
    dfa=dfa[!duplicated(dfa$cls),]
    dfa=dfa[order(dfa$sz,decreasing=FALSE),]
    done=FALSE
    ttl=paste(nrow(dfa),'Items',ptn(sum(dfa$sz)/1000),'KBytes')
    ww=gwindow(title=ttl,width=1100,height=300)
    gtbl=gtable(dfa[,c('durF','szp','cls')],container=ww)
    addhandlerdestroy(gtbl,handler = function(h,...){
      xx=shell(paste('handle',of),intern = TRUE)
      if(any(grepl(of,xx))){
        pidx=xx[grepl('pid',xx)]
        xxx=(as.numeric(unlist(strsplit(pidx,' '))))
        pid=xxx[!is.na(xxx)]
        shell(paste('taskkill /PID',pid, '/F'))
        writeLines('progress=end','block.txt') # stop progress bar
      }else{
        print('No ffmpeg.exe found')
      }
      .GlobalEnv$done=TRUE
    })
    for(fn in dfa$cls)
    { 
      print('------------------------------------------------------------------------------')
      txl=(paste(len(dfa$cls)-which(fn==dfa$cls),'Files Remaining',
                 ptn(sum(file.size(as.character(dfa$cls)),na.rm = TRUE)/1000),'Kbytes Remaining',Sys.time()))
      svalue(ww)=txl
      print(txl)
      rng=which(fn==dfa$cls):len(dfa$cls) #range pre-calc
      #clsx=paste(as.character(dfa[rng,'cls']),ptn(dfa[rng,'sz']))
      dfa[rng[1]:rng[min(len(rng),13)],'durF']=getDur(dfa[rng[1]:rng[min(len(rng),13)],'cls'])
      gtbl[,]=dfa[rng,c('durF','szp','cls')]
      svalue(gtbl)=1
      nfn1=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
      nfn=sub('REDUCE','',nfn1)
      clflag=FALSE
      if(grepl('rpdnclips',nfn)){
        clflag=TRUE
        nfn=sub('_New','',nfn)
      }
      print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
      ddd=shell(paste('mediainfo "',fn,'"',sep=''),intern = TRUE)
      save(ddd,file='mediainfo.RData')
      hevcFlag=any(grepl('HEVC',ddd))
      
      if(clflag & hevcFlag){
        svv(fn,"Already HEVC")
      }
      if((clflag & !hevcFlag) | file.exists(fn) & !file.exists(nfn) & file.size(fn)==dfa[which(dfa$cls==fn),'sz']){
        if(!hevcFlag){
          mtime=file.mtime(fn)
          msize=file.size(fn)
          print(paste('Mtime=',mtime))
          print(subset(ddd,grepl('Duration',ddd))[1])
          of=paste(basename(tempfile()),'.mp4',sep='')
          print(of)
          unlink('block.txt')
          system('"C:\\Program Files\\R\\R-3.2.3\\bin\\rscript.exe" FFMPEGProgressBar.R',wait=FALSE)
          msgi=''
          msgi=shell(paste('c:/Users/Larry/Documents/hexDump/bin/converth265.bat "',
                          fn,'" ',of,',' ,sep=''),translate = TRUE, intern = TRUE)
          print(tail(msgi))
          if(done)
            break
          if(file.exists(of)){
            if(file.size(of)>1000){
              if(any(grepl('hevc',msgi))){
                unlink(nfn)
                if(file.copy(of,nfn)){
                  unlink(of)
                  dx=data.frame(dtn=NA,fn=NA,times=NA)
                  dx$dtn=mtime
                  dx$fn=normalizePath(as.character(fn),winslash = '/')
                  dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                                 ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
                  shell(paste('c:/Users/Larry/Documents/fdd.bat "',
                              nfn,'" ',dx$times,'' ,sep=''),translate = TRUE)
                  
                  print(paste('file mtime back to orig',file.mtime(nfn)))
                  print(paste(fn,ptn(msize)))
                  print(paste(nfn,ptn(file.size(nfn))))
                  svv(nfn,'Already HEVC')
                  if(nfn!=fn)
                    file.remove(fn)
                }
              }else{
                svv(fn,'Bad Metadata')
                unlink(of)
              }
            }else{
              svv(fn,'Bad Size')
              unlink(of)
            }
          }else{
            svv(fn,'Bad Moov')
          }
          writeLines(msgi,'~/temp.txt')
          msgi=readLines('~/temp.txt') # convert CR to CRLF
          writeLines(msgi,'~/temp.txt')
          file.append('~/msgis.txt','~/temp.txt')
        }else{
          svv(fn,'Already HEVC')
        }
      }
    }
  }else{
    print('NONE FOUND')
  }
  
  if(isExtant(ww))
    dispose(ww)
}
