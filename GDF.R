gdfd=function(dfx) {
  .GlobalEnv$mwrit=FALSE
  ew=.GlobalEnv$eww
  hx=ew
  .GlobalEnv$hyChanged=0
  .GlobalEnv$doneflag=FALSE
  .GlobalEnv$discarmsg=FALSE
  ofx=dfx # save original value for no change (w/o NA fill of ' ')
  .GlobalEnv$dfy=dfx
  for(x in 1:ncol(dfx)){
    if(is.na(dfx[,x]))
      dfx[,x]=''
  }
  
  dhll= function(h,...) {
    .GlobalEnv$dfy[1,1]=svalue(hy1)
    .GlobalEnv$dfy[1,2]=svalue(hy2)
    .GlobalEnv$dfy[1,3]=svalue(hy3)
    .GlobalEnv$dfy[1,4]=svalue(hy4)
    
    if(h$action==5 & .GlobalEnv$hyChanged==0){
      print(paste('ACTION 5, 66',hyChanged))
      .GlobalEnv$hyChanged=66
      slct=select.list(as.character(sort(dirtbl$Var1)),graphics = TRUE, 
                       preselect=ifelse(any(grepl(svalue(hy5),as.character(dirtbl$Var1),ignore.case = TRUE)),
                                        svalue(hy5),dfx[1,5]))
      if(nchar(slct)){
        svalue(hy5)=slct
        .GlobalEnv$dfy[1,5]=svalue(hy5) # NOTE: Triggers handler with write to svalue
        enabled(xxx)=TRUE
        enabled(UNB)=TRUE
        enabled(WRT)=TRUE
      }
    }
    if(.GlobalEnv$hyChanged==0)
      .GlobalEnv$hyChanged=1
  }
  
  hy1=gedit(dfx[1,1],cont=.GlobalEnv$ggex,handler=dhll,action=1,width=as.integer(nchar(dfx[1,1])*1.2),initial.msg = 'Filename')
  glabel('Title',cont=.GlobalEnv$ggex)
  hy2=gedit(dfx[1,2],cont=.GlobalEnv$ggex,handler=dhll,action=2)
  glabel('Comment',cont=.GlobalEnv$ggex)
  hy3=gedit(dfx[1,3],cont=.GlobalEnv$ggex,handler=dhll,action=3)
  glabel('Sub Title',cont=.GlobalEnv$ggex)
  hy4=gedit(dfx[1,4],cont=.GlobalEnv$ggex,handler=dhll,action=4)
  if(grepl('RPDNClips',dfx[1,1])){
    glabel('Studio',cont=.GlobalEnv$ggex)
    hy5=gedit(dfx[1,5],cont=.GlobalEnv$ggex,handler=dhll,action=5)
    glabel(paste('Obn',dfx[1,6]),cont=.GlobalEnv$ggex)
    }
  
  addSpace(.GlobalEnv$ggex,10)
  xxx=gbutton('OK',cont=.GlobalEnv$ggex,handler=function(h,...){
    .GlobalEnv$doneflag=TRUE
    .GlobalEnv$discarmsg=FALSE
    h$action=0
    dhll(h)
    .GlobalEnv$hyChanged=0
    dispose(hx)
  })
  UNB=gbutton('UNDO',cont=.GlobalEnv$ggex,handler=function(h,...){
    gcff=gconfirm('ARE YOU SURE?')
    if(gcff){
      enabled(UNB)=FALSE
      .GlobalEnv$doneflag=FALSE
      .GlobalEnv$discarmsg=FALSE
      .GlobalEnv$dfy=ofx # discard changes
      svalue(hy1)=dfx[1,1]
      svalue(hy2)=dfx[1,2]
      svalue(hy3)=dfx[1,3]
      svalue(hy4)=dfx[1,4]
      svalue(hy5)=dfx[1,5]
      galert('Changes Discarded')
    }
  })
  
  WRT=gbutton('WriteMeta',cont=.GlobalEnv$ggex,handler=function(h,...){
    respp=TRUE
    if(svalue(hy1)!=dfx$filename){
      respp=file.rename(dfx$filename,svalue(hy1))
      if(!respp){
        galert('File Rename Failed')
        dispose(hx)
        gtkMainQuit()
      }
    }
    if(respp){
      .GlobalEnv$dfy[1,1]=svalue(hy1)
      .GlobalEnv$dfy[1,2]=svalue(hy2)
      print(paste('1 Title from hy2',svalue(hy2),.GlobalEnv$dfy[1,2]))
      .GlobalEnv$dfy[1,3]=svalue(hy3)
      .GlobalEnv$dfy[1,4]=svalue(hy4)
      if(grepl('RPDNClips',dfx[1,1])){
        print('Writing metadata x')
        wrStud(lsst=svalue(hy1),Title=svalue(hy2),dmComment=svalue(hy3), studio=svalue(hy5),
               SubTitle = svalue(hy4))
        .GlobalEnv$dfy[1,5]=svalue(hy5)
      }else{# no studio write for non-rpdn clips
        wrStud(lsst=svalue(hy1),Title=svalue(hy2),dmComment=svalue(hy3),SubTitle = svalue(hy4),studio=NA)
      }
      .GlobalEnv$mwrit=TRUE
      save(dfy,file='~/dfysave.RData')
      enabled(UNB)=FALSE
      enabled(xxx)=FALSE
      dispose(hx)
    }
  })
  
  keep_above(hx,TRUE)
  
  IDD=addHandlerDestroy(hx,handler=function(h,...) {
    .GlobalEnv$SelectChanged=FALSE
    if(!.GlobalEnv$doneflag){
      .GlobalEnv$dfy=ofx # discard changes
    }
    if(.GlobalEnv$discarmsg & !mwrit){
      galert('Changes Discarded')
    }
    
    gtkMainQuit()
  })
  
  addHandlerKeystroke(hx, handler = function(h,...){
    if(h$key=='\r'){
      if(.GlobalEnv$hyChanged==2){
        .GlobalEnv$doneflag=TRUE
        .GlobalEnv$hyChanged=FALSE
        .GlobalEnv$discarmsg=FALSE
        dispose(hx)
      }else{
        .GlobalEnv$hyChanged=2
      }
      
    }else{
      enabled(xxx)=TRUE
      enabled(UNB)=TRUE
      enabled(WRT)=TRUE
      .GlobalEnv$discarmsg=!mwrit
    }
    
  })
  visible(ew) = TRUE
  focus(ew)=TRUE
  enabled(xxx)=FALSE
  enabled(UNB)=FALSE
  enabled(WRT)=FALSE
  gtkMain()
  if(mwrit){
    load('~/dfysave.RData') #restore edited dfy
  }else{
    dfyy=.GlobalEnv$dfy
  }
  return(dfy)
}


