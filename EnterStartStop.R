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
  
  addHandlerKeystroke(obj, handler = function(h,...){
    if(nchar(svalue(h$obj))==0){ 
      .GlobalEnv$ss=NULL
      dispose(ww)
      gtkMainQuit()
    }else{
      print(svalue(h$obj))
      .GlobalEnv$ToEnd=FALSE
      .GlobalEnv$ss=svalue(h$obj)}
  })
  addHandlerDestroy(ww, handler = function(h,...) {
    if(!.GlobalEnv$bOK)
      .GlobalEnv$ss=NULL
    gtkMainQuit()})
  
  olabel='OK'
  if(allowEnter &!.GlobalEnv$Fdate)
    olabel='To'
  
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
  xbutton=gbutton("Cancel", container=ggp,handler=function(h,...) 
  {
    .GlobalEnv$ss=NULL
    dispose(ww)
    gtkMainQuit()
  })

  focus(obj)=TRUE
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
    gtext(msg,cont=g)
  }else{
    gtext(msg,cont=g,font.attr = list(size=21))
  }
  FUN=function(data) dispose(vvv)
  a <- gtimer(delay*1000,one.shot=TRUE,FUN)
  return(vvv)
}
