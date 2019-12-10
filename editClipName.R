source('~/pllist.git/cleanwplsfn.R')
.GlobalEnv$wcg=NA
editClipName= function(fn){
  if(isExtant(wcg))
    return(fn)
  
  load('~/mfnfo.Rdata')
  wc=gwindow(parent = c(800,0),height = 820,width=80,title=fn,visible = FALSE)
  .GlobalEnv$wcg=wc
  addHandlerDestroy(wc,handler=function(h,...) gtkMainQuit())
  ggp=ggroup(cont=wc,horiz=FALSE)
  fnb=gbutton(basename(fn),cont=ggp)
  enabled(fnb)=FALSE
  wplsx=sub('.wpl','',wpls)[1:(len(wpls)-2)]
  .GlobalEnv$fnx=file_path_sans_ext(fn)
  .GlobalEnv$bn=as.integer(gsub('[a-z|A-Z|_|-]','',basename(fn)))
  for(x in wplsx) {
    gbutton(x,cont=ggp,action=x,handler=function(h,...) {
      print(h$action)
      if(!grepl(h$action,.GlobalEnv$fnx)){
        .GlobalEnv$fnx=paste(.GlobalEnv$fnx,h$action,sep='')
        svalue(fnb)=fnx
        .GlobalEnv$newfnn=fnx
      }
    })
  }
  gbutton('Done',cont=ggp,handler=function(h,...){
    dispose(wc)
  })
  gbutton('BackSpace',cont=ggp,handler=function(h,...){
    fbs=cleanwplsfn(.GlobalEnv$fnx)
    ll=unlist(strsplit(trim(fbs$fy),' '))
    if(len(ll)<2){
      lmx=''
    }else{
      lmx=capture.output(cat(ll[1:(len(ll)-1)],sep=''))
    }
    fxx=cleanwplsfn(paste(fbs$bn,lmx,sep=''))
    .GlobalEnv$fox=paste(.GlobalEnv$bn,gsub(' ','',fxx$fy),'.',file_ext(fn),sep='')
    svalue(fnb)=basename(.GlobalEnv$fox)
    .GlobalEnv$newfnn=basename(.GlobalEnv$fox)
    .GlobalEnv$fnx=file_path_sans_ext(fox)
  })

  visible(wc)=TRUE
  focus(ggp)=TRUE
  gtkMain()
  fx=cleanwplsfn(.GlobalEnv$fnx)
  fout=paste(bn,gsub(' ','',fx$fy),'.',file_ext(fn),sep='')
  print('clip name fout')
  return(fout)
}
