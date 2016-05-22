StartMyGUI <- function() {
  mtime=file.mtime(svt)
  print(paste('Gui Started - NEW',mtime))
  file.remove(dir(pattern = 'file'))
  startt=EnterStartStop()
  if(.GlobalEnv$convert){
    .GlobalEnv$convert=FALSE
    svalue(.GlobalEnv$alrt)='Close to ABORT'
    print('Convert H265')
    bname=paste("C:/Users/Larry/Documents/",basename(tempfile()),sep='')
    of=paste(bname,'.mp4',sep='')
    addHandlerDestroy(.GlobalEnv$alrt,handler = function(h,...) {
      print('ALRT DESTROYED')
      xx=shell(paste('handle',basename(of)),intern = TRUE)
      if(any(grepl(basename(of),xx))){
        pidx=xx[grepl('pid',xx)]
        xxx=(as.numeric(unlist(strsplit(pidx,' '))))
        pid=xxx[!is.na(xxx)]
        shell(paste('taskkill /PID',pid, '/F'))
        writeLines('progress=end',blockFile) # stop progress bar
        unlink(of)
      }
    })
    metaFile=paste(bname,'.RData',sep='')
    ddd=shell(paste('mediainfo "',svt,'"',sep=''),intern = TRUE)
    if(any(grepl('HEVC',ddd)))
    {
      print('Already HEVC')
      return()
    }
    save(ddd,file=metaFile)
    blockFile=paste(bname,'.txt',sep='')
    save(svt,blockFile,metaFile,file='~/blockFileNames.RData')
    print(paste(svt,of,file.mtime(svt)))
    system('"C:\\Program Files\\R\\R-3.2.4revised\\bin\\rscript.exe" "C:\\Users\\Larry\\Documents\\pllist.git\\FFMPEGProgressBar.R',wait=FALSE)
    shell(sprintf('c:/Users/Larry/Documents/hexDump/bin/converth265P.bat %s %s %s',blockFile,svt,of))
    if(file.size(of)<600){
      print('Bad Size, failed to convert')
      unlink(of)
      unlink(blockFile)
    }else{
      ofn=sub('REDUCE','',svt)
      if(!file.rename(of,ofn)){
        print('file rename back to orig failed - REDUCE')
      }else{
        unlink(of)
        print('file renamed back to orig - REDUCE')
        dx=data.frame(dtn=NA,fn=NA,times=NA)
        dx$dtn=mtime+(3600*7) # add 7hr for GMT to PDT
        dx$fn=normalizePath(as.character(ofn),winslash = '/')
        dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                       ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
        cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
        eval(parse(text=cmd))
        print('file mtime back to orig - REDUCE')
      }
    }
    startt=NULL
  }else{
    if(.GlobalEnv$Fdate){
      source('~/pllist.git/dfxprocess.R')
      .GlobalEnv$Fdate=TRUE
    }
  }
  if(!.GlobalEnv$Fdate){  
    print(paste('len startt=',len(startt)))
    if(len(startt)>0){
      print(paste('startt=',startt))
      endtt=0
      if(!.GlobalEnv$ToEnd)
        endtt=EnterStartStop("Enter End Time (mm:ss), 
                           or Enter/Esc for End of File\n",TRUE)
      
      if(len(endtt)){
        svtt='c:/RealPlayerDownloads/trimmed.mp4'
        unlink('~/temppt.mp4')
        unlink(svtt)
        file.rename(svt,'~/temppt.mp4')
        if(.GlobalEnv$ToEnd){
          endtt='23:59:59'
          .GlobalEnv$ToEnd=FALSE
        }
        if(!grepl(':',startt)){
          starttd=as.integer(startt)
          startt=paste(as.integer(starttd/60),':',starttd%%60,sep='')
          if(!grepl(':',endtt)){
            endtt=as.integer(endtt)
            endtt=paste(as.integer(endtt/3600),':',as.integer(endtt/60),':',endtt%%60,sep='')
          }
        }else{
          pos=unlist(gregexpr(':',startt))
          starttd=as.integer(substr(startt,1,(pos-1)))*60 + as.integer(substr(startt,pos+1,nchar(startt)))
        }
        if((nchar(endtt)<8) & (len(unlist(gregexpr(':',endtt))) !=2))
          endtt=paste('00:',endtt,sep='')
        dd=strptime(endtt,"%H:%M:%S") - strptime(startt,"%M:%S")
        attd=attributes(dd)$units
        if(attd=='secs')
          timef=1
        if(attd=='mins')
          timef=60
        if(attd=='hours')
          timef=3600
        endttd=as.character(dd*timef)
        cmdd=paste('shell("ffmpeg.exe -ss',starttd,' -i c:/users/Larry/Documents/temppt.mp4 -t',endttd,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
        print(cmdd)
        eval(parse(text=cmdd))
        svt1=sub('TRIM','',svt)
        svtO=paste(file_path_sans_ext(svt1),'_cut.',file_ext(svt1),sep='') # add _New to original filename
        file.rename(svtt,svtO) # replace svt has trimmed with start to end
        file.rename('~/temppt.mp4',svt) # keep original file
        print('file renamed back to orig - REDUCE')
        
        dx=data.frame(dtn=NA,fn=NA,times=NA)
        dx$dtn=mtime+(7*3600) # add 7 hours to convert PDT to GMT
        dx$fn=normalizePath(as.character(svtO),winslash = '/')
        dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                       ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
        cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
        eval(parse(text=cmd))
        print('file mtime back to orig - REDUCE')
        print(file.mtime(svtO))
        #gmessage(paste(cmd,file.mtime(svtO),svtO))
      }else
        print('Invalid start/end time')
    }
    return()
  }
}
