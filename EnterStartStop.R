editMeta=function() {
  cd('~/')
  svt=normalizePath(svt,winslash = '/')
  print(svt)
  cmdd=paste('shell("mediainfo.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
  print(cmdd)
  eval(parse(text=cmdd))
  cmdd=paste('shell("exiftool',svt,' >>meta.txt",mustWork=NA,translate=TRUE)')
  print(cmdd)
  eval(parse(text=cmdd))
  .GlobalEnv$meta=readLines('meta.txt')
  unlink('meta.txt')
  
  wm <- gwindow(paste("Metadata-",svt),width=700,visible = FALSE)
  gpm<- ggroup(horizontal=FALSE, container=wm)
  tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE,
                 handler = NULL)
  
  meta=meta[nchar(meta)>0]
  mm=matrix(NA,len(meta),2)
  pos=gregexpr(':',meta)
  for (i in 1:len(meta))
    pos[i]=pos[[i]][1]
  pos=unlist(pos)
  pos[pos==-1]=1
  meta[pos==1]=paste(':',meta[pos==1])
  mm[,1]=substr(meta,1,pos-1)
  mm[,2]=substr(meta,pos+1,nchar(meta))
  mg=data.frame(mm,stringsAsFactors = FALSE)
  cmts=mg[grepl('title|comment',mg$X1,ignore.case = TRUE),]
  cmts=cmts[!duplicated(paste(trim(cmts$X1),trim(cmts$X2))),]
  mg=rbind(cmts,mg)
  tabm[,]=mg
  .GlobalEnv$metadata = mg
  visible(wm) <- TRUE
  bgm <- ggroup(container=gpm)
  addSpring(bgm)
  
  rgx=gedit(' ',cont=bgm,handler=function(h,...){
    mdd=trim(paste(.GlobalEnv$metadata[,1],.GlobalEnv$metadata[,2]))
    rng=which(grepl(svalue(h$obj),mdd,ignore.case = TRUE))
    tabm[,]=.GlobalEnv$metadata[rng,]
  })
  
  gbutton("dismiss", container=bgm, handler = function(h,...) {
    visible(wm) <- FALSE;
    gtkMainQuit()
  })
  focus(rgx)=TRUE
  addHandlerDestroy(wm,function(h,...) {
    gtkMainQuit()
  })
  gtkMain()
}

ALTGinput = function(x="Enter Start Time (secs) or (mm:ss)",allowEnter){
  .GlobalEnv$bOK=TRUE
  .GlobalEnv$ss=-1
  .GlobalEnv$ToEnd=FALSE
  ww=gwindow(height=30,title=x) #,parent = alrt)
  ggp=ggroup(cont=ww)
  getToolkitWidget(ww)$move(0,100)
  obj <- gedit(container=ggp, handler=function(h,...) 
  { .GlobalEnv$ss=svalue(h$obj)
  dispose(ww)
  gtkMainQuit()
  })
  focus(obj)=TRUE
  
  if(allowEnter)
    .GlobalEnv$odir="C:\\RealPlayerDownloads"
  else
    .GlobalEnv$odir=dirname(svt)
  
  .GlobalEnv$chosen=FALSE
  if(!.GlobalEnv$Fdate){
    gx=gedit(text=.GlobalEnv$odir, cont=ggp, handler=function(h,...){
      if(!.GlobalEnv$chosen){
        .GlobalEnv$chosen=TRUE
        odirnew=choose.dir()
        if(!is.na(odirnew)){
          svalue(gx)=odirnew
        }else{
          .GlobalEnv$chosen=FALSE
        }
      }
    })
  }
  
  addHandlerKeystroke(obj, handler = function(h,...){
    if(nchar(svalue(h$obj))==0){ 
      .GlobalEnv$ss=NULL
      dispose(ww)
      gtkMainQuit()
    }else{
      #print(svalue(h$obj))
      .GlobalEnv$ToEnd=FALSE
      .GlobalEnv$ss=svalue(h$obj)}
  })
  addHandlerDestroy(ww, handler = function(h,...) {
    if(!.GlobalEnv$bOK)
      .GlobalEnv$ss=NULL
    gtkMainQuit()})
  
  olabel='TO'
  if(.GlobalEnv$Fdate)
    olabel='OK'
  
  obutton=gbutton(olabel, container=ggp,handler=function(h,...)
  {
    .GlobalEnv$bOK=FALSE
    if(ss >= 0){
      .GlobalEnv$bOK=TRUE
      dispose(ww)
      gtkMainQuit()
    }else{
      focus(obj)=TRUE
    }
  })
  if(!.GlobalEnv$Fdate){
    tbutton=gbutton("ToEnd", container=ggp,handler=function(h,...)
    {
      .GlobalEnv$ToEnd=TRUE
      if(ss > 0 | allowEnter){
        dispose(ww)
        gtkMainQuit()
      }else{
        focus(obj)=TRUE
      }
    })
    
    if(!allowEnter)
      fbutton=gbutton("FDATE", container=ggp,handler=function(h,...)
      {
        .GlobalEnv$Fdate=TRUE
        dispose(ww)
        gtkMainQuit()
      })
    
    if(!allowEnter)
      Cbutton=gbutton("Convert", container=ggp,handler=function(h,...){
        .GlobalEnv$convert=TRUE
        dispose(ww)
        gtkMainQuit()
      })
  }
  if(!allowEnter)
    mdbutton=gbutton("Metadata", container=ggp,handler=function(h,...) 
    {
      editMeta()
      .GlobalEnv$Fmeta=TRUE # exit immediately after ww close
      dispose(ww)
      gtkMainQuit()
    })
  
  xbutton=gbutton("Cancel", container=ggp,handler=function(h,...) 
  {
    .GlobalEnv$ss=NULL
    dispose(ww)
    gtkMainQuit()
  })
  focus(ww)=TRUE
  gtkMain()
}

EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n",allowEnter=FALSE){
  .GlobalEnv$ss=NULL
  while(TRUE){
    ALTGinput(x,allowEnter)
    startt= .GlobalEnv$ss   
    if(len(startt)>0){
      if(!is.na(as.integer(startt))){
        break # good integer
      }else{
        cpos=regexpr(':',startt)
        if(cpos>0){
          f1=as.integer(substr(startt,1,cpos-1))
          f2=as.integer(substr(startt,cpos+1,nchar(startt)))
          if (f1>=0 & f1<60 & f2>=0 & f2<60){
            break # good mm:ss
          }
        }else{
          if(allowEnter){
            startt=10000
            break;
          }
          
        }
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}

galert=function(msg,delay=3)
{
  vvv=gwindow(height = 50)
  getToolkitWidget(vvv)$move(0,0)
  addHandlerDestroy(vvv,handler=function(h,...) {a$stop_timer()})
  g <- gvbox(cont=vvv)
  if(nchar(msg)>25){
    gtext(msg,cont=g,font.attr = list(size=21))
    size(vvv)[1]=size(vvv)[1]*(as.integer(nchar(msg))/25)
  }else{
    gtext(msg,cont=g,font.attr = list(size=21))
  }
  FUNz=function(data) dispose(vvv)
  a <- gtimer(delay*1000,one.shot=TRUE,FUNz)
  return(vvv)
}
