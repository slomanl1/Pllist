library(gWidgets2)
options(guiToolkit = "RGtk2") 
scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
undoer=''

buttonHandler = function(zaction){
  print(zaction)
  .GlobalEnv$undoer=svalue(ge)
  if(zaction=='CLEAR')
    svalue(ge)=''
  if(zaction=='UNDO')
    svalue(ge)=undoer
  if(zaction %in% c('AND','OR','(',')','EQ','STUDIO','WPLS','NOT')){
    svalue(ge)=paste(svalue(ge),zaction)
  }
}

w=gwindow(width=50)
g1=ggroup(cont=w,horiz=FALSE)
ge=gedit('hello',cont=g1)

gbutton('STUDIO',cont=g1,action='STUDIO',handler= function(h,...){buttonHandler(h$action)})
gbutton('WPLS',cont=g1,action='WPLS',handler= function(h,...){buttonHandler(h$action)})
gbutton('=',cont=g1,action='EQ',handler= function(h,...){buttonHandler(h$action)})

gbutton('AND',cont=g1,action='AND',handler= function(h,...){buttonHandler(h$action)})
gbutton('OR',cont=g1,action='OR',handler= function(h,...){buttonHandler(h$action)})
gbutton('NOT',cont=g1,action='OR',handler= function(h,...){buttonHandler(h$action)})
gbutton('(',cont=g1,action='(',handler= function(h,...){buttonHandler(h$action)})
gbutton(')',cont=g1,action=')',handler= function(h,...){buttonHandler(h$action)})
gbutton('UNDO',cont=g1,action='UNDO',handler= function(h,...){buttonHandler(h$action)})
gbutton('REDO',cont=g1,action='REDO',handler= function(h,...){buttonHandler(h$action)})
gbutton('CLEAR',cont=g1,action='CLEAR',handler= function(h,...){buttonHandler(h$action)})

gbutton('DONE',cont=g1,handler = function(h,...){
  dispose(w)
})



