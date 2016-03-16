destr=FALSE
sll=NA
ww=gwindow('Choose',height=180,width=700)
#cb=rep(TRUE,len(choices))
#for(i in 1:len(choices))
#  gcheckbox(choices[i],container=ww,action=i,checked=TRUE,handler=function(h,...){.GlobalEnv$cb[svalue(h$action)]=svalue(h$obj)})
txl=gtable(choices,container=ww,multiple = TRUE,handler=function(h,...){.GlobalEnv$cb=svalue(h$obj)})
vv=gradio(c('INCREASING','DECREASING'),container = ww)
gcheckbox('OK',container=ww,handler=function(h,...){gtkMainQuit()})
addhandlerdestroy(ww,handler=function(h,...){
  .GlobalEnv$destr=TRUE
  gtkMainQuit()
})
gtkMain()
if(!destr){
  decreasing=FALSE
  if(svalue(vv)=='DECREASING')
    decreasing=TRUE
  sll=svalue(txl)
  if(exists('ww'))
    dispose(ww)
}
