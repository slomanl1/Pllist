scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/cleanwplsfn.R')
source('~/pllist.git/editClipName.R')
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
gotSt=FALSE
newfnn=''

reorgpkd = function(topix){
  pkdi=c(pkd,pkd[1:(topix-1)])
  pkdi=pkdi[topix:len(pkdi)]
  return(pkdi)
}

buttonHandler=function(action) {
  enabled(xb)=FALSE
  if(grepl(' ',pkd[k])) {
    if(nchar(filterx)){
      .GlobalEnv$gotSt=TRUE
      .GlobalEnv$pkd[k]=paste(strsplit(pkd[k],' ')[[1]][1],sub('btn_','',action))
      print(paste('replaced',filterx,'with',sub('btn_','',action)))
    }
  }else{
    .GlobalEnv$gotSt=TRUE
    .GlobalEnv$pkd[k]=paste(.GlobalEnv$pkd[k],sub('btn_','',action)) # fix to delete 1st studio whaen adding another (use xx)
  }
  
  gtb[k,1]=.GlobalEnv$pkd[k]
  print(paste('New studio Added',.GlobalEnv$pkd[k]))
  if(exists('wcg')){
    if(isExtant(wcg))
      dispose(wcg)
  }
  svalue(gtb)=k # select the line
  .GlobalEnv$breaker=FALSE
  visible(g)=FALSE
  Sys.sleep(.3)
  visible(g)=TRUE
  gtkMainQuit()
}

load('~/dirtbl.rdata')
load('~/mfnfo.RData')
dd=data.frame(table(mfnfo$studio))
ee=dd[order(dd$Freq),]
mgg=merge(dirtbl,ee,by='Var1',all.x = TRUE)
mgg[is.na(mgg$Freq.y),'Freq.y']=1
dirtbl=mgg[order(mgg$Freq.y,decreasing = TRUE),]
names(dirtbl)=c('Var1','removv','Freq')
dirtbl=dirtbl[,c('Var1','Freq')]

mfnfo$bn=gsub('[a-z|A-Z]','',mfnfo$lsst)
mfnfo$bn=as.integer(trim(sub('.','',mfnfo$bn,fixed=TRUE)))
mfnfo=mfnfo[order(mfnfo$bn),]
fns=mfnfo$lsst
fns=subset(fns,file.exists(fns))
load('~/pkd.RData') # load(pkd)
xx=sapply(1:len(pkd),function(x) {strsplit(pkd[x],' ')[[1]][1]})
pkd=subset(pkd,file.exists(xx))
addtopkd=fns[which(!fns %in% xx)]
pkd=c(pkd,addtopkd)
pkd=unique(pkd)
pkd=trim(sub('NA','',pkd))
looped=FALSE
k=0
tpexist=FALSE
gconfer=FALSE
filterx=''
while(TRUE){
  print('MAIN LOOP')
  if(!looped){
    print(paste('looping',k))
    k=k+1
    if(k>len(pkd))
      k=1
    ss=strsplit(pkd[k],' ')
    fx=ss[[1]][1]
    stud=ss[[1]][2]
    if(!is.na(stud)){
      if(nchar(filterx)){
        if(!stud %in% filterx)
          next
      }else{
        next
      }
    }else{
      if(nchar(filterx))
        next
    }
    if(k>7 & !tpexist){
      .GlobalEnv$pkd=reorgpkd(k)
    }
    ix=which(grepl(fx[1],fns))[1]
  }
  if(len(ix)==0){
    ix=len(fns)+1
    pkd[k]=fns[ix]
  }
  if(k>7 & tpexist){
    .GlobalEnv$pkd=reorgpkd(k)
    gtb[,]=.GlobalEnv$pkd
    k=1
  }
  if(tpexist)
    svalue(gtb)=k
  looped=FALSE 
  shell(pkd[k])
  print(paste('Playing',pkd[k]))
  shell("nircmd win setsize process wmplayer.exe 0 0 900 600")
  
  if(!tpexist){
    w=gwindow(parent = c(1000,0),height = 820,width=550,visible=FALSE)
    keep_above(w,TRUE)
    g=gvbox(cont=w,use.scrollwindow = TRUE)
    ggp <- gformlayout(container=g)
    addHandlerDestroy(w,handler=function(h,...) {
      .GlobalEnv$breaker=TRUE
      .GlobalEnv$fx=''
      if(exists('wcg')){
        if(isExtant(wcg))
          dispose(wcg)
      }
      gtkMainQuit()
    })
    gtb=gtable(pkd,cont=ggp,handler=function(h,...){
      print('Gtable Handler')
      .GlobalEnv$breaker=TRUE
      ixd=svalue(gtb,index = TRUE)
      
      if(.GlobalEnv$fx==strsplit(pkd[ixd],' ')[[1]][1]){
        ofn=fx
        .GlobalEnv$fx=editClipName(.GlobalEnv$fx)
        print('main clipname returned')
        if(.GlobalEnv$gotSt){
          .GlobalEnv$pkd[ixd]=paste(.GlobalEnv$fx,strsplit(.GlobalEnv$pkd[ixd],' ')[[1]][2])
          
        }
        if(!file.rename(ofn,fx))
          galert('RENAME of clip failed')
      }else{
        .GlobalEnv$fx=strsplit(pkd[ixd],' ')[[1]][1]
        print('file rename success')
      }
      
      if(!is.na(.GlobalEnv$fx))
      {
        if(exists('ofn'))
          print(paste('GT hand (old)=',ofn,'(new) fx=',fx))
        if(!gotSt)
          .GlobalEnv$pkd[ixd]=fx
        .GlobalEnv$pkd=reorgpkd(ixd)
        visible(w)=TRUE
        gtb[,]=.GlobalEnv$pkd
        svalue(gtb)=1
        
        .GlobalEnv$k=1
        if(gotSt){
          .GlobalEnv$gotSt=FALSE
          gtkMainQuit()
        }else{
          shell(pkd[1])
          print(paste('gtable handler Playing',pkd[1]))
        }
      }
    })
    svalue(gtb)=1
    
    ghh=ggroup(cont=ggp)
    xb=gbutton("EXIT",cont=ghh, handler=function(h,...){
      print(paste('EXIT HANDLER Main Level=',gtkMainLevel()))
      .GlobalEnv$breaker=TRUE
      .GlobalEnv$fx=''
      dispose(ggp)
      gtkMainQuit()
    })
    xc=gbutton("NEXT",cont=ghh, handler=function(h,...){
      .GlobalEnv$breaker=FALSE
      ixd=svalue(gtb,index = TRUE)
      print(paste('ixd=',ixd))
      if(ixd>7){
        .GlobalEnv$pkd=reorgpkd(ixd)
        gtb[,]=.GlobalEnv$pkd
        svalue(gtb)=1
      }
      gtkMainQuit()
    })
    xdl=gbutton("DELETE",cont=ghh,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        fnd=strsplit(pkd[ixd],' ')[[1]][1]
        yn=gconfirm(paste('delete',pkd[ixd]))
        if(yn){
          shell(sprintf('nircmd moverecyclebin "%s"',fnd),translate=TRUE)
          .GlobalEnv$pkd[ixd]=NA
          .GlobalEnv$breaker=FALSE
          gtb[,]=.GlobalEnv$pkd
          gtkMainQuit()
        }
      }
    })
    xe=gbutton("BackSpace",cont=ghh,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        field=strsplit(.GlobalEnv$pkd[ixd],' ')[[1]]
        if(len(field)>1){
          field=field[1:len(field)-1]
          .GlobalEnv$pkd[ixd]=capture.output(cat(field))
          gtb[,]=.GlobalEnv$pkd
          svalue(gtb)=ixd
        }
      }
    })
    xaa=gbutton('ADD',cont=ghh,handler=function(h,...){
      ccx=gconfirm('Are you sure you want to add to DIRTBL?')
      if(!ccx)
        return()
      dad=dirtbl[1,]
      dad$Var1=trim(svalue(xd))
      dad$Freq=1
      dirtbl=rbind(dirtbl,dad)
      save(dirtbl,file='~/dirtbl.Rdata')
    })
    flb=gbutton('Filt',cont=ghh,handler=function(h,...){
      if(nchar(filterx)){
        .GlobalEnv$filterx=''
        gtkMainQuit()
      }else{
        .GlobalEnv$ggw=gwindow("Choose Group",parent=c(600,100))
        .GlobalEnv$ggpw=gvbox(cont=ggw,use.scrollwindow = TRUE)
        for(i in 1:len(btns)){
          bz=gbutton(sub('btn_','',btns[i]),action=i,cont=ggpw,handler=function(h,...){
            cmdd=paste('btn_',i,'=bz',sep='')
            eval(parse(text=cmdd))
            dispose(ggw)
            .GlobalEnv$filterx=(sub('btn_','',btns[h$action]))
            shell("nircmd setcursor 1250 200")
            gtkMainQuit()
          })
        }
      }
    })
    xd=gedit(cont=ghh,handler=function(h,...){
      if(nchar(trim(svalue(xd)))==0)
        return()
      bn=as.integer(gsub('[a-z|A-Z|_|/|:]','',pkd))
      ixd=which(as.integer(svalue(xd))==bn)[1]
      if(!is.na(ixd)){
        if(ixd>5){
          .GlobalEnv$pkd=reorgpkd(ixd)
          gtb[,]=.GlobalEnv$pkd
          ixd=1
        }
        .GlobalEnv$k=ixd-1
        svalue(gtb)=1
        .GlobalEnv$breaker=FALSE
        gtkMainQuit()
      }else{
        .GlobalEnv$btnn=which(grepl(trim(svalue(xd)),btns,ignore.case = TRUE))
        
        if(toupper(trim(svalue(xd))) %in% toupper(dirtbl$Var1)){
          galert(paste('EXACT match DETECTED-',sub('btn_','',btns[btnn[1]])))
          buttonHandler(btns[btnn[1]])
        }else{
          if(len(btnn)){
            if(len(btnn)==1){
              galert(paste('PARTIAL match DETECTED-',sub('btn_','',btns[btnn[1]])))
              buttonHandler(btns[btnn])
            }else{
              .GlobalEnv$ggw=gwindow("Choose Group",parent=c(600,100))
              .GlobalEnv$ggpw=gvbox(cont=ggw,use.scrollwindow = TRUE)
              for(i in 1:len(btnn)){
                bz=gbutton(sub('btn_','',btns[btnn[i]]),action=i,cont=ggpw,handler=function(h,...){
                  cmdd=paste('btn_',i,'=bz',sep='')
                  eval(parse(text=cmdd))
                  if(exists('ggw')){
                    if(isExtant(ggw))
                      dispose(ggw)
                  }
                  buttonHandler(btns[btnn[h$action]])
                  shell("nircmd setcursor 1250 200")
                })
              }
           # }
            addHandlerKeystroke(ggw,handler=function(h,...){
              if(h$key=="\r"){
                buttonHandler(btns[btnn[1]])
              }
            })
            addHandlerDestroy(ggw,handler=function(h,...){
              enabled(xb)=TRUE # EXIT button
            })
            shell("nircmd setcursor 800 150")
            }
          }
        }
        .GlobalEnv$breaker=FALSE
        if(len(btnn))
          for(i in 1:len(btnn)){
            eval(parse(text=paste('.GlobalEnv$bx=',btns[btnn[i]])))
            font(.GlobalEnv$bx) <- c(color="red", weight="bold") # initial
          }
      }
    })
    breaker=FALSE
    wplsx=dirtbl[1:(nrow(dirtbl)),1]
    for(x in wplsx) 
    {
      btn=gbutton(x,cont=ggp,action=x,handler=function(h,...) {
        buttonHandler(h$action)
      })
      
      cmdd=paste('btn_',trim(gsub('-','_',x)),'=btn',sep='')
      eval(parse(text=cmdd))
    }
    tpexist=TRUE
    .GlobalEnv$btns=fi('btn_',ls(envir=.GlobalEnv))
  }
  # if(nchar(trim(svalue(xd))))
  #   svalue(xd)=''
  ndone=sum(sapply(1:len(pkd), function(x) sum(grepl(' ',trim(pkd[x])))>0))
  svalue(w)=paste(pkd[k],'new',.GlobalEnv$newfnn,len(pkd)-ndone,'files remaining') #title
  svalue(gtb)=k
  
  FUN1 <- function(data) {
    tryCatch.W.E({
      svalue(w)=paste(pkd[k],'new',.GlobalEnv$newfnn,len(pkd)-ndone,'files remaining',filterx)
      .GlobalEnv$pss=shell('handle -p wmplayer',intern = TRUE)
      if(!any(grepl('pid',.GlobalEnv$pss))){
        print('BROKER')
        .GlobalEnv$breaker=FALSE
        gtkMainQuit()
      }
      ixd=svalue(gtb,index = TRUE)
      if(!is.null(ixd))
        enabled(xe)=len(strsplit(.GlobalEnv$pkd[ixd],' ')[[1]])>1
    })
  }
  addSpring(g)
  size(gtb) <- c(200, 200)
  if(!visible(w)){
    visible(w)=TRUE
    focus(w)=TRUE
    Sys.sleep(.6)
  }
  clr='black'
  if(nchar(filterx))
    clr='red'
  
  font(flb) <- c(color=clr, weight="bold") # initial
  focus(xd)=TRUE
  a1 <- gtimer(250, FUN1)
  print('gtkmain')
  gtkMain()
  ###################################################
  print('EXITED MAIN')
  visible(w)=FALSE
  a1$stop_timer()
  #shell('nircmd win close title "Windows Media Player"')
  Sys.sleep(.1)
  enabled(xb)=TRUE
  pkd=unique(pkd)
  pkd=trim(sub('NA','',pkd))
  save(pkd,file='~/pkd.RData')
  #  dispose(xxd)
  if(exists('btnn')){
    if(len(btnn))
      for(i in 1:len(btns)){
        eval(parse(text=paste('.GlobalEnv$bx=',btns[i])))
        font(.GlobalEnv$bx) <- c(color="black", weight="normal") # initial        }
      }
    if(exists('ggpw')){
      if(isExtant(ggpw))
        dispose(ggpw)
    }
  }
  if(breaker){
    if(nchar(fx)){
      fxz=strsplit(fx,' ')[[1]][1]
      k=which(grepl(fxz,pkd))[1]
      looped=TRUE
      next
    }
    break
  }
}  
if(isExtant(w)){
  dispose(w)
}

shell('nircmd win close title "Windows Media Player"')
studio=sapply(1:len(pkd),function(x) {strsplit(pkd[x],' ')[[1]][2]})
lsst=sapply(1:len(pkd),function(x) {strsplit(pkd[x],' ')[[1]][1]})
studios=data.frame(lsst,studio)
mfnfo=merge(mfnfo[,1:11],studios,by='lsst')
save(mfnfo,wpls,file='~/mfnfo.RData')
if(gconfirm('BUILD?'))
  source('~/pllist.git/addStudioToDmfnfo.R') # calls BuildALT.R , ProcessWA.R, MakeDfanFromAM.R







