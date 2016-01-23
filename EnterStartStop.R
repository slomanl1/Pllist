ALTGinput = function(x="Enter Start Time (secs) or (mm:ss)",allowEnter){
  .GlobalEnv$bOK=TRUE
  .GlobalEnv$ss=-1
  .GlobalEnv$ToEnd=FALSE
  ww=gwindow(height=25,title=x)	
  obj <- gedit(container=ww)
  addhandlerchanged(obj, handler=function(h,...) 
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
  addHandlerDestroy(obj, handler = function(h,...) {
    if(!.GlobalEnv$bOK)
      .GlobalEnv$ss=NULL
    gtkMainQuit()})
  
  obutton=gbutton("OK", container=ww,handler=function(h,...)
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
    tbutton=gbutton("ToEnd", container=ww,handler=function(h,...)
    {
      .GlobalEnv$ToEnd=TRUE
      if(ss > 0 | allowEnter){
        dispose(ww)
        gtkMainQuit()
      }else{
        focus(obj)=TRUE
      }
    })
    
    fbutton=gbutton("FDATE", container=ww,handler=function(h,...)
    {
      .GlobalEnv$Fdate=TRUE
      dispose(ww)
      gtkMainQuit()
    })
    
    if(!allowEnter)
      tbutton=gbutton("Convert", container=ww,handler=function(h,...){
        .GlobalEnv$convert=TRUE
        dispose(ww)
        gtkMainQuit()
      })
  }
  xbutton=gbutton("Cancel", container=ww,handler=function(h,...) 
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
