destr=TRUE
sll=NULL

ww=gwindow('Choose',height=180,width=400)
ggx=ggroup(container=ww)
txl=gtable(choices,container=ggx,multiple = TRUE,handler=function(h,...){.GlobalEnv$cb=svalue(h$obj)})
vv=gradio(c('INCREASING','DECREASING'),container = ggx)
gcheckbox('OK',container=ggx,use.togglebutton=TRUE,handler=function(h,...){
  .GlobalEnv$destr=FALSE
  gtkMainQuit()})
IDD=addHandlerDestroy(ww,handler=function(h,...){
  gtkMainQuit()
})
shell('nircmd win activate title "Choose"')
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
