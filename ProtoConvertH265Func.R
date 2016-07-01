source('~/pllist.git/FFMPEGProgressBar.R')
svv=function(x,y){print(paste(x,y))}
convH265 = function(fn, ttl,nfn=''){
  print('Starting ConvH265 Proto')
  bname=paste("C:/Users/Larry/Documents/",basename(tempfile()),sep='')
  metaFile=paste(bname,'.RData',sep='')
  .GlobalEnv$of=paste(bname,'.mp4',sep='')
  ddd=shell(paste('mediainfo "',fn,'"',sep=''),intern = TRUE)
  save(ddd,file=metaFile)
  hevcFlag=any(grepl('HEVC',ddd))
  .GlobalEnv$aborted=FALSE
  
  if(clflag & hevcFlag){
    svv(fn,"Already HEVC")
  }
  if((.GlobalEnv$clflag & !hevcFlag) | file.exists(fn) 
     #& file.size(fn)==dfa[which(dfa$fname==fn),'sz'] # move to outside of function into calling proc
  ){
    
    if(!hevcFlag){
      htt=350
      if(!exists('dfa'))
        htt=50
      if(!.GlobalEnv$tpexists){
        .GlobalEnv$ww=gwindow(title=ttl,width=1400,height=htt,visible = FALSE)
        .GlobalEnv$gp <- ggroup(horizontal = FALSE, container = ww)
        .GlobalEnv$pbx=gprogressbar(0,container = gp)
        if(exists('dfa')){
          .GlobalEnv$gtbl=gtable(dfa[0,c('fdate','durF','fsize','fname')],container=gp)
          addHandlerDoubleclick(gtbl,handler=function(h,...) {
            idx=svalue(h$obj,index=TRUE)
            shell(gtbl[idx,'fname'])
            svalue(gtbl)=which(fn == gtbl[1,]$fname)
            })
          .GlobalEnv$tpexists=TRUE
        }

        addHandlerDestroy(ww,handler = function(h,...){
          if(nchar(.GlobalEnv$of))
            gxy=galert('Terminating FFMPEG',15)
          xx=shell(paste('handle',basename(.GlobalEnv$of)),intern = TRUE)
          if(any(grepl(basename(.GlobalEnv$of),xx)) & nchar(.GlobalEnv$of)){
            pidx=xx[grepl('pid',xx)]
            xxx=(as.numeric(unlist(strsplit(pidx,' '))))
            pid=xxx[!is.na(xxx)]
            shell(paste('taskkill /PID',pid, '/F'))
            writeLines('progress=end',blockFile) # stop progress bar
            print(paste('deleting',.GlobalEnv$of,unlink(.GlobalEnv$of))) # of deleted signals aborted
            .GlobalEnv$aborted=TRUE
          }else{
            print('No ffmpeg.exe found')
            if(exists('gxy')){
              dispose(gxy)
              rm(gxy)
            }
          }
          .GlobalEnv$done=TRUE
        })
      }
      if(exists('dfa')){
        .GlobalEnv$gtbl[,]=dfa[rng[1:min(len(rng),13)],c('fdate','durF','fsize','fname')]
        svalue(gtbl)=which(fn == gtbl[1,]$fname)
      }
      
      visible(.GlobalEnv$ww)=TRUE
      getToolkitWidget(.GlobalEnv$ww)$move(0,0)
      
      mtime=file.mtime(fn)
      msize=file.size(fn)
      print(paste('Mtime=',mtime))
      print(subset(ddd,grepl('Duration',ddd))[1])
      
      .GlobalEnv$of=paste(bname,'.mp4',sep='')
      blockFile=paste(bname,'.txt',sep='')
      svt=fn
      save(svt,blockFile,metaFile,file='~/blockFileNames.RData')
      print(.GlobalEnv$of)
      cx='start /LOW /B /WAIT /AFFINITY 0xe c:/users/Larry/Documents/hexDump/bin/ffmpeg.exe -progress %s -i %s -c:v libx265 -c:a copy %s'
      cy=sprintf(cx,blockFile,fn, .GlobalEnv$of)
      print(cy)
      shell(cy,wait = FALSE)
      
      ffmpegProgressBar()
      medi=shell(paste('mediainfo "',.GlobalEnv$of,'"',sep=''),intern = TRUE)
      if(file.exists(.GlobalEnv$of)){
        if(file.size(.GlobalEnv$of)>1000){
          if(any(grepl('HEVC',medi))){
            unlink(nfn)
            if(file.copy(.GlobalEnv$of,nfn)){
              unlink(.GlobalEnv$of)
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
              svv(nfn,'Already HEVC',FALSE)
              if(nfn!=fn)
                file.remove(fn)
            }
          }else{
            svv(fn,'Bad Metadata')
            unlink(.GlobalEnv$of)
          }
        }else{
          svv(fn,'Bad Size')
          unlink(.GlobalEnv$of)
        }
      }else{
        if(!done)
          svv(fn,'Bad Moov')
      }
    }else{
      svv(fn,'Already HEVC')
    }
  }
  return(of)
}




