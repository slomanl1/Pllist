keep_above <- function(w, val=TRUE) w$widget$setKeepAbove(val)
gdfd=function(dfx) {
  .GlobalEnv$hyChanged=0
  .GlobalEnv$doneflag=FALSE
  #print(paste('gdf double clicked-',.GlobalEnv$doubleClicked))
  .GlobalEnv$dfy=dfx
  for(x in 1:ncol(dfx)){
    if(is.na(dfx[,x]))
      dfx[,x]=' '
  }
  hx=gwindow('Edit Filename, metadata',height=30,width=1080)
  ggp=ggroup(container = hx)
  hy=gdf(dfx[,1:4],cont=ggp)
  addSpace(ggp,10)
  xxx=gbutton('DONE',cont=ggp,handler=function(h,...){
    .GlobalEnv$doneflag=TRUE
    dispose(hx)
  })
  keep_above(hx,TRUE)
  addHandlerChanged(hy, handler = function(h,...) {
    #print('changed handler hy')
#    enabled(xxx)=TRUE
    .GlobalEnv$dfy=svalue(h$obj,drop = FALSE)
    .GlobalEnv$hyChanged=1
  })
  IDD=addHandlerDestroy(hx,handler=function(h,...) {
    if(!.GlobalEnv$doneflag){
      .GlobalEnv$dfy=dfx # discard changes
      galert('Changes Discarded')
    }
    #print('hx destroyed');
    gtkMainQuit()})
  
  addHandlerKeystroke(hx, handler = function(h,...){
    print('handler KS in GDF')
    if(h$key=='\r'){
      #print('return detected')
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
