StartMyGUI <- function() {
  mtime=file.mtime(svt)
  print(paste('Gui Started - NEW',mtime))
  file.remove(dir(pattern = 'file'))
  startt=EnterStartStop()
  if(.GlobalEnv$convert){
    .GlobalEnv$ss=NULL
    .GlobalEnv$convert=FALSE
    .GlobalEnv$tpexists=FALSE
    .GlobalEnv$clflag=grepl('rpdnclips',svt,ignore.case = TRUE)
    if(exists('alrt'))
      if(isExtant(alrt))
        dispose(alrt)
    of=convH265(svt,ttl=svt,svt)
    if(file.exists(of))
      if(file.size(of)<600){ #test here for premature abort with of deleted by galert handler
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
          .GlobalEnv$ss=-1
        }
      }
    startt=NULL
  }else{
    if(.GlobalEnv$Fdate){
      source('~/pllist.git/dfxprocess.R')
      .GlobalEnv$Fdate=TRUE
    }
  }
  if(!.GlobalEnv$Fdate & !.GlobalEnv$Fmeta){  
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
        }else{ 
          .GlobalEnv$ss=NULL # indicates output dir is realplayerd
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
        xx=shell(paste('exiftool -r',svtt),intern = TRUE) # get metadata
        zz=fi('Duration',xx)[1]
        if(len(unlist(gregexpr(':',zz[1])))>1){
          svt1=sub('TRIM','',svt)
          svt2=paste(file_path_sans_ext(svt1),'_cut.',file_ext(svt1),sep='') 
          svtO=paste(odir,'\\',basename(svt2),sep='') # add output directory selected
          file.rename(svtt,svtO) # replace svt has trimmed with start to end
          
          file.rename('~/temppt.mp4',svt) # keep original file
          if(endtt=='23:59:59'){
            shell(sprintf('nircmd moverecyclebin "%s"',svt),translate=TRUE)
            print(paste(svt,'Moved to recycle bin'))
          }else{
            print('file renamed back to orig - REDUCE')
          }
          
          dx=data.frame(dtn=NA,fn=NA,times=NA)
          dx$dtn=mtime+(7*3600) # add 7 hours to convert PDT to GMT
          dx$fn=normalizePath(as.character(svtO),winslash = '/')
          dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                         ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
          cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
          eval(parse(text=cmd))
          print('file mtime back to orig - REDUCE')
          print(file.mtime(svtO))
        }else{
          galert('trim failed')
          file.rename('~/temppt.mp4',svt) # keep original file
          file.remove(svtt)
        }
      }else
        print('Invalid start/end time')
    }
    return()
  }
}

tailfile=function(file, n=6) {
  bufferSize <- 1024L
  size <- file.info(file)$size
  
  if (size < bufferSize) {
    bufferSize <- size
  }
  
  pos <- size - bufferSize
  text <- character()
  k <- 0L
  
  f <- file(file, "rb")
  on.exit(close(f))
  
  while(TRUE) {
    seek(f, where=pos)
    chars <- readChar(f, nchars=bufferSize)
    k <- k + length(gregexpr(pattern="\\n", text=chars)[[1L]])
    text <- paste0(text, chars)
    
    if (k > n || pos == 0L) {
      break
    }
    
    pos <- max(pos-bufferSize, 0L)
  }
  
  tail(strsplit(text, "\\n")[[1L]], n)
}
