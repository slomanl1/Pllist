library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn

newStudio=''
load('~/mfnfo.RData')
dtbl=as.data.frame(table(mfnfo$studio),stringsAsFactors = FALSE)
sll=select.list(dtbl$Var1,graphics = TRUE)
w=gwindow()
gg=gedit(sll,cont=w,handler=function(h,...){
  print(svalue(gg))
  .GlobalEnv$newStudio=svalue(gg)
  gtkMainQuit()
})
addHandlerDestroy(w,handler=function(h,...) {
  gtkMainQuit()
})
focus(gg)=TRUE
gtkMain()
dispose(gg)

if(!newStudio %in% dtbl$Var1){
  cnf=gconfirm(paste('Are you sure you want to add',newStudio,'?'))
  if(cnf){
    ms=subset(mfnfo,studio==sll)[,c('lsst','studio','cmt')]
    ms$studio=newStudio
    cd('D:/PNMTALL/RPDNClips')
    wrStud(ms$lsst,ms$studio)
    ofn=ms$lsst
    nfn=sub(sll,newStudio,ofn)
    rss=file.rename(ofn,nfn)
    galert(paste(sum(rss),'Files Renamed'))
  }else{
    galert('CANCELLED!')
  }
}else{
  galert('None Found!')
}



