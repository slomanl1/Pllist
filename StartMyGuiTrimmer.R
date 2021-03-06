StartMyGUI <- function(skipper=FALSE) {
  mtime=file.mtime(svt)
  print(paste('Gui Started - NEW',mtime))
  file.remove(dir(pattern = 'file')[!isLocked(dir(pattern = 'file'))&&
                                      !grepl('Facebook_files',dir(pattern = 'file'))])
  if(!skipper)
    startt=EnterStartStop()
  if(.GlobalEnv$convert){
    if(gg=='CANCEL'){
      .GlobalEnv$ss=NULL
      return()
    }
    
    .GlobalEnv$ss=NULL
    .GlobalEnv$convert=FALSE
    .GlobalEnv$tpexists=FALSE
    .GlobalEnv$clflag=grepl('rpdnclips',svt,ignore.case = TRUE)
    if(exists('alrt'))
      if(isExtant(alrt))
        dispose(alrt)

    if(gg=='H264')
      hevcFlag=FALSE
    of=convH265(svt,ttl=svt,svt,H264=(gg=='H264'),F720P=(gg=='F720P'))
    if(file.exists(of))
      if(file.size(of)<600){ #test here for premature abort with of deleted by galert handler
        print('Bad Size, failed to convert')
        unlink(of)
        unlink(blockFile)
      }else{
        ofn=sub('REDUCE','',svt)
        if(!file.copy(of,ofn,overwrite = TRUE)){
          print('file rename back to orig failed - REDUCE')
        }else{
          unlink(of)
          print('file renamed back to orig - REDUCE')
          dx=data.frame(dtn=NA,fn=NA,times=NA)
          dx$dtn=mtime+(3600*6) # add 6hr for GMT to MDT
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
      .GlobalEnv$ss=NULL
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
        if(attd=='days')
          timef=3600*24
        endttd=as.character(dd*timef)
        cmdd=paste('shell("ffmpeg.exe -ss',starttd,' -i c:/users/Larry/Documents/temppt.mp4 -t',endttd,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
        print(cmdd)
        eval(parse(text=cmdd))
        xx=shell(paste('exiftool -r',svtt),intern = TRUE) # get metadata
        zzo=fi('Duration',xx)[1]
        print(paste('zzo =',zzo))
        if(len(unlist(gregexpr(':',zzo[1])))>1){
          svt1=sub('TRIM','',svt)
          svt2=paste(file_path_sans_ext(svt1),'_cut.',file_ext(svt1),sep='') 
          .GlobalEnv$svtO=paste(odir,'\\',basename(svt2),sep='') # add output directory selected
          if(file.exists(svtO)){
            galert(paste(svtO,'already exists'))
            .GlobalEnv$ss=NULL
            return()
          }else{
            file.rename(svtt,svtO) # replace svt has trimmed with start to end
          }
          if(!.GlobalEnv$ToEnd){
            wed=gwindow("Edit Filename (*.mp4)",height=30,width=800)
            ged=gedit(.GlobalEnv$svtO,cont=wed,handler=function(h,...){
              nfn=svalue(h$obj)
              if(nfn !=.GlobalEnv$svtO){
                if(file_ext(nfn)!='.mp4'){
                  nfn1=paste(file_path_sans_ext(nfn),'.mp4',sep='')
                }else{
                  nfn1=nfn
                }
                result=file.rename(.GlobalEnv$svtO,nfn1)
                if(!result){
                  galert('File Rename failed')
                }else{
                  .GlobalEnv$svtO=nfn1 # for restore original Fdate
                  galert('File Rename Successful')
                  load('~/ConvertLog.RData')
                  nl=cnvLog[1,]
                  nl$svt=svt
                  nl$nfn=nfn1
                  nl$md5s=md5sum(nfn1)
                  cnvLog=rbind(cnvLog,nl)
                  save(cnvLog,file='~/ConvertLog.RData')
                }
              }
              dispose(wed)
              gtkMainQuit()
            })
            addHandlerDestroy(wed,handler=function(h,...){
              gtkMainQuit()
            })
            gtkMain()
          }
          file.rename('~/temppt.mp4',svt) # keep original file
          if(endtt=='23:59:59'){
            shell(sprintf('nircmd moverecyclebin "%s"',svt),translate=TRUE)
            print(paste(svt,'Moved to recycle bin'))
          }else{
            print('file renamed back to orig - REDUCE')
          }
          
          dx=data.frame(dtn=NA,fn=NA,times=NA)
          dx$dtn=mtime+(6*3600) # add 7 hours to convert MDT to GMT
          if(!dst(dx$dtn))
            dx$dtn=dx$dtn+3600 # Add one more hour for Standard Time (MST)
          dx$fn=normalizePath(as.character(.GlobalEnv$svtO),winslash = '/')
          dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                         ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
          cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
          eval(parse(text=cmd))
          print('file mtime back to orig - REDUCE')
          print(file.mtime(svtO))
          .GlobalEnv$ss=-1 #indicates trim successful
        }else{
          galert('trim failed')
          file.rename('~/temppt.mp4',svt) # keep original file
          file.remove(svtt)
          .GlobalEnv$ss=NULL #indicates trim failure
        }
      }else{
        galert('Invalid start/end time')
        .GlobalEnv$cancelFlag=TRUE
      }
    }else{
      .GlobalEnv$cancelFlag=TRUE
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
