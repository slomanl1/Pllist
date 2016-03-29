destr=TRUE
sll=NULL
choices=c('D:/PNMTALL','C:/PNMTALL','C:/MyVideos/RPDNclips','c:/PNMTALL/NewDownloads','REDUCE only')
ww=gwindow('Choose',height=180,width=400)
txl=gtable(choices,container=ww,multiple = TRUE,handler=function(h,...){.GlobalEnv$cb=svalue(h$obj)})
ggx=ggroup(container=ww)
vv=gradio(c('INCREASING','DECREASING'),container = ggx)
gcheckbox('OK',container=ggx,use.togglebutton=TRUE,handler=function(h,...){
  .GlobalEnv$destr=FALSE
  gtkMainQuit()})
IDD=addHandlerDestroy(ww,handler=function(h,...){
  gtkMainQuit()
})
if(file.exists('~/ChooseDIRS.RData')){
  load('~/ChooseDIRS.RData') # load pre-select from sll
  svalue(txl)=sll
  svalue(vv)=slv
}
gtkMain()
if(!destr){
  decreasing=FALSE
  if(svalue(vv)=='DECREASING')
    decreasing=TRUE
  sll=svalue(txl)
  slv=svalue(vv)
  save(sll,slv,file='~/ChooseDIRS.RData')
  if(exists('ww')){
    removeHandler(ww, ID=IDD) # prevent gtkMainQuit() error
    dispose(ww)
  }

}else{
  sll=NULL
}
