scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
reorgppk = function(topix){
  ppki=c(ppk,ppk[1:(topix-1)])
  ppki=ppki[topix:len(ppki)]
  return(ppki)
}

load('~/mfnfo.rdata')
load('~/ppk.RData')
xx=sapply(1:len(ppk),function(x) {strsplit(ppk[x],' ')[[1]][1]})
ppk=subset(ppk,file.exists(xx))
if(!file.exists('C:\\My playlists\\missing.m3u'))
{
  galert('No Files in Missing.m3u Found')
  fns=ppk  
}else{
  fns=readLines('C:\\My playlists\\missing.m3u')
  fns=subset(fns,file.exists(fns))
}

looped=FALSE
k=0
tpexist=FALSE
gconfer=FALSE
while(TRUE){
  if(!looped){
    k=k+1
    ix=which(grepl(fns[k],ppk))
  }else{
    k=which(grepl(strsplit(fx,' ')[[1]][1],fns))
  }
  if(len(ix)==0){
    ix=len(ppk)+1
    ppk[ix]=fns[k]
  }
  
  looped=FALSE 
  shell(fns[k])
  print(paste('shelled',fns[k]))
  
  if(!tpexist){
    w=gwindow(parent = c(1000,0),height = 820)
    ggp=ggroup(cont=w,horiz=FALSE)
    addHandlerDestroy(w,handler=function(h,...) {
      .GlobalEnv$breaker=TRUE
      .GlobalEnv$fx=''
      gtkMainQuit()
    })
    breaker=FALSE
    wplsx=wpls[1:(len(wpls)-2)]
    for(x in wplsx) gbutton(x,cont=ggp,action=x,handler=function(h,...) {
      print(h$action)
      if(!grepl(h$action,ppk[ix]))
        .GlobalEnv$ppk[ix]=paste(.GlobalEnv$ppk[ix],h$action)
      print(ppk[ix])
      gtb[,]=.GlobalEnv$ppk
      svalue(gtb)=ix
    })
    gtb=gtable(ppk,cont=ggp,handler=function(h,...){
      print('Gtable Handler')
      .GlobalEnv$breaker=TRUE
      ixd=svalue(gtb,index = TRUE)
      .GlobalEnv$fx=strsplit(ppk[ixd],' ')[[1]][1]
      if(!is.na(.GlobalEnv$fx))
      {
        .GlobalEnv$ppk=reorgppk(ixd)
        gtb[,]=.GlobalEnv$ppk
        gtkMainQuit()
      }
    })
    ghh=ggroup(cont=ggp)
    xb=gbutton("EXIT",cont=ghh, handler=function(h,...){
      print(paste('EXIT HANDLER Main Level=',gtkMainLevel()))
      .GlobalEnv$breaker=TRUE
      .GlobalEnv$fx=''
      if(gtkMainLevel()==0)
        return()
      dispose(ggp)
      gtkMainQuit()
    })
    xc=gbutton("NEXT",cont=ghh, handler=function(h,...){
      .GlobalEnv$breaker=FALSE
      ixd=svalue(gtb,index = TRUE)
      print(ixd)
      if(ixd>5){
        .GlobalEnv$ppk=reorgppk(ixd)
        gtb[,]=.GlobalEnv$ppk
      }
      gtkMainQuit()
    })
    xd=gbutton("DELETE",cont=ghh,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        fnd=strsplit(ppk[ixd],' ')[[1]][1]
        yn=gconfirm(paste('delete',ppk[ixd]))
        if(yn){
          shell(sprintf('nircmd moverecyclebin "%s"',fnd),translate=TRUE)
          .GlobalEnv$ppk[ixd]=NA
          .GlobalEnv$breaker=FALSE
          gtb[,]=.GlobalEnv$ppk
          gtkMainQuit()
        }
      }
    })
    xe=gbutton("BackSpace",cont=ghh,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        field=strsplit(.GlobalEnv$ppk[ixd],' ')[[1]]
        if(len(field)>1){
          field=field[1:len(field)-1]
          .GlobalEnv$ppk[ixd]=capture.output(cat(field))
          print(.GlobalEnv$ppk[ixd])
          gtb[,]=.GlobalEnv$ppk
          svalue(gtb)=ixd
        }
      }
    })
    xd=gedit(cont=ghh,handler=function(h,...){
      print(svalue(xd))
      ixd=which(grepl(svalue(xd),.GlobalEnv$ppk))[1]
      if(ixd>5){
        .GlobalEnv$ppk=reorgppk(ixd)
        gtb[,]=.GlobalEnv$ppk
      }
      .GlobalEnv$breaker=FALSE
      gtkMainQuit()
    })
    svalue(gtb)=ix
    keep_above(w)
    tpexist=TRUE
  }
  ndone=sum(sapply(1:len(ppk), function(x) sum(grepl(' ',trim(ppk[x])))>0))
  svalue(w)=paste(fns[k],len(ppk)-ndone,'files remaining') #title
  svalue(gtb)=ix
  focus(gtb)=TRUE
  pss=''
  FUN1 <- function(data) {
    tryCatch.W.E({
      pss=c(pss,shell('handle -p wmplayer',intern = TRUE))
      ssum=sum(grepl('My Playlists',pss))
      if(!any(grepl('pid',pss))|ssum>1){
        print('BROKER')
        .GlobalEnv$breaker=FALSE
        gtkMainQuit()
      }
    })
  }
  a1 <- gtimer(250, FUN1)
  focus(w)=TRUE
  gtkMain()
  print('EXITED MAIN')
  a1$stop_timer()
  
  shell('nircmd win close title "Windows Media Player"')
  Sys.sleep(1)
  save(ppk,file='~/ppk.RData')
  
  if(breaker){
    if(nchar(fx)){
      ix=which(grepl(fx,ppk))
      looped=TRUE
      next
    }
    gconfer=gconfirm('Build')
    break
  }
}  
if(isExtant(w)){
  dispose(w)
}

if(gconfer)
  source('~/pllist.git/processWA.R')

