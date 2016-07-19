keep_above <- function(w, val=TRUE) w$widget$setKeepAbove(val)
gdfd=function(dfx) {
  
  .GlobalEnv$hyChanged=0
  .GlobalEnv$doneflag=FALSE
  ofx=dfx # ssave original value for no change (w/o NA fill of ' ')
  .GlobalEnv$dfy=dfx
  for(x in 1:ncol(dfx)){
    if(is.na(dfx[,x]))
      dfx[,x]=''
  }
  
  dhll= function(h,...) {
    .GlobalEnv$dfy[1,h$action]=svalue(h$obj,drop = FALSE)
    if(.GlobalEnv$hyChanged==0)
      .GlobalEnv$hyChanged=1
  }

  hx=gwindow('Edit Filename, metadata',height=30,width=1080)
  ggp=ggroup(container = hx)
  hy1=gedit(dfx[1,1],cont=ggp,handler=dhll,action=1,width=nchar(dfx[1,1]),initial.msg = 'Filename')
  hy2=gedit(dfx[1,2],cont=ggp,handler=dhll,action=2,initial.msg = 'Title')
  hy3=gedit(dfx[1,3],cont=ggp,handler=dhll,action=3,initial.msg = 'Comment')
  hy4=gedit(dfx[1,4],cont=ggp,handler=dhll,action=4,initial.msg = 'Sub Title')

  addSpace(ggp,10)
  xxx=gbutton("OK",cont=ggp,handler=function(h,...){
    .GlobalEnv$doneflag=TRUE
    dispose(hx)
  })
  keep_above(hx,TRUE)

  IDD=addHandlerDestroy(hx,handler=function(h,...) {
    if(!.GlobalEnv$doneflag){
      .GlobalEnv$dfy=ofx # discard changes
      galert('Changes Discarded')
    }
    #print('hx destroyed');
    gtkMainQuit()})
  
  addHandlerKeystroke(hx, handler = function(h,...){
    if(h$key=='\r'){
      if(.GlobalEnv$hyChanged==2){
        .GlobalEnv$doneflag=TRUE
        .GlobalEnv$hyChanged=FALSE
        dispose(hx)
      }else{
        .GlobalEnv$hyChanged=2
        enabled(xxx)=TRUE
      }

    }

  })
  
  enabled(xxx)=FALSE
  gtkMain()
  dfyy=.GlobalEnv$dfy
  return(dfyy)
}
