source('~/pllist.git/FFMPEGProgressBar.R')
source('~/pllist.git/isVC1HEVC.R')
svv=function(x,y,z){print(paste(x,y))}
convH265 = function(fn, ttl,nfn='',H264=FALSE,F720P=FALSE){
  print('Starting ConvH265 Proto')
  fn=normalizePath(fn,winslash = '/')
  bname=paste("C:/Users/Larry/Documents/",basename(tempfile()),sep='')
  metaFile=paste(bname,'.RData',sep='')
  .GlobalEnv$of=paste(bname,'.mp4',sep='')
  ddd=shell(paste('medi.bat "',fn,'"',sep=''),intern = TRUE)
  save(ddd,file=metaFile)
  dddur=ddd
  xx='ddd=shell("exiftool %s -Title -XMPToolkit -DMComment -Subtitle -GPSLatitude",intern=TRUE,translate=TRUE)'
  cmdd=sprintf(xx,fn)
  eval(parse(text=cmdd))
  
  hevcFlag=any(grepl('HEVC',ddd)) & !H264
  xxm=fi('XMP Toolkit',ddd)
  xxt=fi('Title',ddd)
  xxc=fi('DM Comment',ddd)
  xxs=fi('SubTitle',ddd)
  xxo=fi('GPS Latitude',ddd)
  
  studio=''
  if(len(xxm)>0){
    studio=trim(strsplit(xxm,':')[[1]][2])
    print(paste('read studio:',studio))
  }
  Title=''
  if(len(xxt)>0){
    Title=trim(strsplit(xxt,':')[[1]][2])
    print(paste('read Title:',Title))
  }
  DMComment=''
  if(len(xxc)>0){
    DMComment=trim(strsplit(xxc,':')[[1]][2])
    print(paste('read DM Comment:',DMComment))
  }  
  Subtitle=''
  if(len(xxs)>0){
    Subtitle=trim(strsplit(xxs,':')[[1]][2])
    print(paste('read Subtitle:',Subtitle))
  }
  
  Obn=''
  if(len(xxo)>0){
    Obn=trim(strsplit(xxo,':|deg')[[1]][2])
    print(paste('read Obn:',Obn))
  }  
  
  .GlobalEnv$aborted=FALSE
  
  if(clflag & hevcFlag){
    svv(fn,"Already HEVC")
  }
  if((.GlobalEnv$clflag & !hevcFlag) | file.exists(fn) 
     #& file.size(fn)==dfa[which(dfa$fname==fn),'sz'] # move to outside of function into calling proc
  ){
    
    if(!hevcFlag){
      htt=370
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
          addHandlerClicked(gtbl, handler=function(h,...) sprintf("You selected %s", svalue(h$obj)))
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
        .GlobalEnv$gtbl[,]=dfa[rng[1:min(len(rng),17)],c('fdate','durF','fsize','fname')]
        svalue(gtbl)=which(fn == gtbl[1,]$fname)
      }
      
      visible(.GlobalEnv$ww)=TRUE
      getToolkitWidget(.GlobalEnv$ww)$move(0,0)
      
      mtime=file.mtime(fn)
      msize=file.size(fn)
      print(paste('Mtime=',mtime))
      print(subset(dddur,grepl('Duration',dddur))[2])
      
      .GlobalEnv$of=paste(bname,'.mp4',sep='')
      blockFile=paste(bname,'.txt',sep='')
      svt=fn
      save(svt,blockFile,metaFile,file='~/blockFileNames.RData')
      print(.GlobalEnv$of)
      gx=''
      if(isVC1(fn))
        H264=TRUE
      if(isVC1(fn)|H264){
        if(F720P){
          if(any(grepl('720P',ddd)))
            return('')
        }
        nfn=sub('_New','_mp4',nfn)
        galert('Converting to H264 and DOWNSCALE TO 720P')
        print('Converting to H264 and DOWNSCALE TO 720P')
      }
      if(!H264 & F720P){
        galert('Converting to H265 and DOWNSCALE TO 720P')
        print('Converting to H265 and DOWNSCALE TO 720P')
        cx='start /LOW /B /WAIT /AFFINITY 0xe c:/users/Larry/Documents/hexDump/bin/ffmpeg.exe -progress %s -i %s -c:v libx265 -vf scale=-1:720 %s'
      }
      if(H264 & !F720P){
        cx='start /LOW /B /WAIT /AFFINITY 0xe c:/users/Larry/Documents/hexDump/bin/ffmpeg.exe -progress %s -i %s -c:v libx264 %s'
        gx=galert('Converting VC-1/Bad to mp4',300)
        print('Converting VC-1/Bad to mp4')
        galert('Converting VC-1/Bad to mp4')
      }
      load('~/bads.RData')
      bads=bads[!grepl('_mp4',bads$fname),]
      save(bads,file='~/bads.RData')
      if(!H264 & !F720P){
        galert('Converting to H265')
        nfn=sub('_mp4','',nfn)
        cx='start /LOW /B /WAIT /AFFINITY 0xe c:/users/Larry/Documents/hexDump/bin/ffmpeg.exe -progress %s -i %s -c:v libx265 -c:a copy %s'
      }
      cy=sprintf(cx,blockFile,fn, .GlobalEnv$of)
      print(cy)
      shell(cy,wait = FALSE)
      
      ffmpegProgressBar()
      if(isExtant(gx))
        dispose(gx)
      medi=shell(paste('mediainfo "',.GlobalEnv$of,'"',sep=''),intern = TRUE)
      if(file.exists(.GlobalEnv$of)){
        if(file.size(.GlobalEnv$of)>1000){
          if(any(grepl('HEVC|AVC',medi))){
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
              if(nchar(studio)>0 | nchar(Title)>0 | nchar(DMComment)>0 | nchar(Subtitle)>0 | nchar(Obn)>0)
              {
                mx=data.frame(lsst=nfn,studio=studio,dmComment=DMComment,Title=Title,SubTitle=Subtitle,Obn=Obn,stringsAsFactors = FALSE)
                writeStudio(mx,FALSE)
                print(paste('metadata written',studio,DMComment,Title,Subtitle))
              }
              print(paste('file mtime back to orig',file.mtime(nfn)))
              print(paste(fn,ptn(msize)))
              print(paste(nfn,ptn(file.size(nfn))))
              svv(nfn,'Already HEVC',FALSE)
              if(normalizePath(nfn) != normalizePath(fn)){
                file.remove(fn)
                print(paste(fn,'NOT REMOVED!!!!!!!!'))
              }
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




