gdfd=function(dfx) {
  .GlobalEnv$doneflag=FALSE
  print(paste('gdf double clicked-',.GlobalEnv$doubleClicked))
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
  addHandlerChanged(hy, handler = function(h,...) {
    enabled(xxx)=TRUE
    print(svalue(h$obj, drop = FALSE))
    .GlobalEnv$dfy=svalue(h$obj,drop = FALSE)
  })
  IDD=addHandlerDestroy(hx,handler=function(h,...) {
    if(!.GlobalEnv$doneflag){
      .GlobalEnv$dfy=dfx # discard changes
      galert('Changes Discarded')
    }
    print('hx destroyed');
    gtkMainQuit()})
  enabled(xxx)=FALSE
  gtkMain()
  dfyy=.GlobalEnv$dfy
  return(dfyy)
}
