scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
load('~/zz.RData')
dirtbl=as.data.frame(table(as.character(dirname(zz))))
dirs=as.character(dirtbl$Var1)
dd=select.list(dirs,multiple = TRUE,graphics = TRUE)
ww=gwindow(title = 'From Regular Expression')
xx=gedit('',cont=ww,handler = function(h,...){
  .GlobalEnv$from=svalue(xx)
  dispose(ww)
  gtkMainQuit()
})
focus(xx)=TRUE
gtkMain()

ww=gwindow(title = 'To Expression')
xx=gedit('',cont=ww,handler = function(h,...){
.GlobalEnv$toto=svalue(xx)
  dispose(ww)
  gtkMainQuit()
})
focus(xx)=TRUE
gtkMain()
load('~/dfan.rdata')
ww=which(grepl(from,dfan$DMComment))
print(len(ww))

dd=sub(from,toto,dfan[ww,'DMComment'])
print(paste(dd,'=====',dfan[ww,'DMComment']))

