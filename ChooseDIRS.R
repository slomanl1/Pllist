destr=FALSE
sll=NA

ww=gwindow('Choose',height=180,width=700)
txl=gtable(choices,container=ww,multiple = TRUE,handler=function(h,...){.GlobalEnv$cb=svalue(h$obj)})
vv=gradio(c('INCREASING','DECREASING'),container = ww)
gcheckbox('OK',container=ww,use.togglebutton=TRUE,handler=function(h,...){gtkMainQuit()})
addhandlerdestroy(ww,handler=function(h,...){
  .GlobalEnv$destr=TRUE
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
  if(exists('ww'))
    dispose(ww)
}
