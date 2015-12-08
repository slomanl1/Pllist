ALTGinput = function(x="Enter Start Time (secs) or (mm:ss)",allowEnter){
  .GlobalEnv$ss=-1
  .GlobalEnv$ToEnd=FALSE
  ww=gwindow(height=25,title=x)	
  obj <- gedit(container=ww)
  addhandlerchanged(obj, handler=function(h,...) 
  { .GlobalEnv$ss=svalue(h$obj);dispose(ww);gtkMainQuit()})
  addHandlerKeystroke(obj, handler = function(h,...){
    if(nchar(svalue(h$obj))==0){ 
      .GlobalEnv$ss=NULL
      dispose(ww)
      gtkMainQuit()
    }else{
      .GlobalEnv$ss=svalue(h$obj)}
  })
  addHandlerDestroy(obj, handler = function(h,...) {
    if(!.GlobalEnv$bOK)
      .GlobalEnv$ss=NULL
    gtkMainQuit()})
  
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
  obutton=gbutton("OK", container=ww,handler=function(h,...)
  {
    .GlobalEnv$bOK=FALSE
    if(ss > 0){
      .GlobalEnv$bOK=TRUE
      dispose(ww)
      gtkMainQuit()
    }else{
      focus(obj)=TRUE
    }
  })
  xbutton=gbutton("Cancel", container=ww,handler=function(h,...) 
  {
    .GlobalEnv$ss=NULL
    dispose(ww)
    gtkMainQuit()})
  focus(obj)=TRUE
  gtkMain()
}
