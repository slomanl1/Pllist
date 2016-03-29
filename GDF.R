gdfd=function(dfx) {
  .GlobalEnv$dfy=dfx
  linerw=gwindow(height = 30, title='Edit Record')
  ggp=ggroup(cont=linerw)
  obj1 =  gedit(dfx[,1], width=nchar(trim(dfx[,1]))*1.15,editable=TRUE, container = ggp)
  addHandlerChanged(obj1,handler=function(h,...){
    .GlobalEnv$dfy[,1]=svalue(obj1)
    gtkMainQuit()})
  obj2 =  gedit(dfx[,2], editable=TRUE, container = ggp)
  addHandlerChanged(obj2,handler=function(h,...){
    .GlobalEnv$dfy[,2]=svalue(obj2)
    gtkMainQuit()})
  obj3=  gedit(dfx[,3], editable=TRUE, container = ggp)
  addHandlerChanged(obj3,handler=function(h,...){
    .GlobalEnv$dfy[,3]=svalue(obj3)
    gtkMainQuit()})
  obj4=  gedit(dfx[,4], editable=TRUE, container = ggp)
  addHandlerChanged(obj4,handler=function(h,...){
    .GlobalEnv$dfy[,4]=svalue(obj4)
    gtkMainQuit()})

  
  
  addHandlerBlur(obj1,handler=function(h,...){
    .GlobalEnv$dfy[,1]=svalue(obj1)
  })
  
  addHandlerBlur(obj2,handler=function(h,...){
    .GlobalEnv$dfy[,2]=svalue(obj2)
  })
  
  addHandlerBlur(obj3,handler=function(h,...){
    .GlobalEnv$dfy[,3]=svalue(obj3)
  })
  
  addHandlerBlur(obj4,handler=function(h,...){
    .GlobalEnv$dfy[,4]=svalue(obj4)
  })
  

  IDD=addHandlerDestroy(linerw,handler=function(h,...) {gtkMainQuit()})
  gtkMain()
  
  if(isExtant(linerw))
      dispose(linerw)
  return(.GlobalEnv$dfy)
}

