scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/editClipName.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
idd="D:/PNMTALL/NewDownloads"
flss=NULL
fls=NULL
while(TRUE){
  idd=choose.dir(idd,caption='Choose Input Directory')
  if(is.na(idd))
    break
  print(paste('idd chosen=',default=idd))
  cd(idd)
  cd()
  ww=gwindow(parent = c(0,0))
  while(!is.na(idd)){
    fls=NULL
    print(paste('idd chosen=',idd))
    files=dir()
    files=files[order(file.mtime(files),decreasing = TRUE)]

    xx=gtable(files,cont=ww,handler=function(h,...){
      print('xx handler')
      .GlobalEnv$fls=svalue(xx)
      print(fls)
      gtkMainQuit()
    })
    addHandlerDestroy(ww,handler=function(h,...){
      fls=NULL
      print('Destroy Handler')
      gtkMainQuit()
    })
    gtkMain()
    print('main exited')
    
    if(!len(fls))
      break
    flss=c(flss,fls)
    if(exists('tbl'))
      if(isExtant(tbl))
        dispose(tbl)
    gtw=gwindow(parent=c(500,0))
    gtwg=ggroup(cont=gtw)
    tbl=gtable(flss,cont=gtwg)
    btn=gbutton("Done",cont=gtwg,handler=function(h,...){
      print('btn handler')
      dispose(btn)
      dispose(ww)
    })
  }
  if(!len(fls))
    break
}

if(len(flss)>1){
  od=choose.dir(caption='Choose Output Directory',default=idd)
  if(!is.na(od)){
    svtti=ginput('Enter output file name (.mp4)')
    svtt=make.names(paste(file_path_sans_ext(svtti),'.mp4',sep=''))
    svtt1=gsub('.','',svtt,fixed = TRUE) # remove extra periods
    svtt2=sub('mp4','.mp4',svtt1) # put back .mp4 extension
    ofx=paste(od,svtt2,sep = '\\') # add output directory
    writeLines(' ',ofx)
    of=normalizePath(ofx,winslash='/')
    unlink(ofx)
    if(len(svtt) & len(svtti)){
      flss=paste("file '",flss,"'",sep='')
      writeLines(flss,'~/mylist.txt') 
      cmdd=paste('shell("ffmpeg.exe -f concat -i c:/users/Larry/Documents/mylist.txt -c copy',of,'",mustWork=NA,translate=TRUE)')
      print(cmdd)
      eval(parse(text=cmdd))
    }else{
      print('Invalid output file name')
    }
  }else{
    print('no directory chosen')
  }
  dispose(tbl)
}else{
  print('Invalid Input, at least 2 files required')
}

