selList = function(items) {
  .GlobalEnv$retval=NULL
  w <- gwindow("selList",visible = FALSE,height=50)
  g <- ggroup(cont=w,horizontal = FALSE)
  ph <- ggroup(cont=g);  
  gt=gtable(items=items, cont=ph,multiple = FALSE)
  pg <- ggroup(cont=g); 
  rr=gbutton("Remove", cont=pg,handler=function(h,...){
    #print('Remove Handler')
    ix=svalue(gt,index = TRUE)
    xx=c(gt[1:ix-1,],gt[(ix+1):nrow(gt),])
    gt[1:len(xx),1]=xx
    gt[nrow(gt),1]=''
    enabled(uu)=TRUE
    enabled(ss)=TRUE
  }); 
  cc=gbutton("Clear All", cont=pg,handler=function(h,...){
    gt[,]=gt[0,]
    enabled(uu)=TRUE
    enabled(ss)=TRUE    
  }); 
  kk=gbutton("OK", cont=pg,handler=function(h,...){
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=FALSE)
    dispose(w)
    gtkMainQuit()
  }); 
  nn=gbutton("Cancel", cont=pg,handler=function(h,...){
    getAndExp(disposer=TRUE)
    dispose(w)
    gtkMainQuit()
  })
  ss=gbutton("SAVE", cont=pg,handler=function(h,...){
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=TRUE)
    dispose(w)
    gtkMainQuit()
  }) 
  uu=gbutton("UNDO", cont=pg,handler=function(h,...){
    dispose(w)
    gtkMainQuit()
  })  
  ee=gbutton("EDIT", cont=pg,handler=function(h,...){
    enabled(uu)=FALSE
    enabled(ss)=FALSE
    enabled(rr)=FALSE
    enabled(cc)=FALSE
    enabled(kk)=FALSE
    enabled(ee)=FALSE
    andexp=items[svalue(gt,index=TRUE)]
    andexp=substr(andexp,17,nchar(andexp)) # remove REGEXP FILTER:
    andexp=unlist(strsplit(andexp,'&')) # restore AND array
    aexp=unlist(getAndExp(andexp[1],andexp[2],andexp[3]))
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=FALSE)
    dispose(w)
    gtkMainQuit()
  })  
  
  addHandlerDestroy(w,handler=function(h,...){
    gtkMainQuit()
  })  
  addHandlerDoubleclick(gt,handler=function(h,...){
    #print('Double Click Handler')
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=FALSE)
    dispose(w)
    gtkMainQuit()
  })
  addHandlerSelectionChanged(gt,handler=function(h,...){
    #print('Select Chg Handler')
    #print(svalue(gt,index=TRUE))
    enabled(rr)=TRUE
    })
  size(gt)=c(400,400)
  enabled(uu)=FALSE
  enabled(ss)=FALSE
  enabled(rr)=FALSE
  keep_above(w,TRUE)
  visible(w)=TRUE
  gtkMain()
  return(.GlobalEnv$retval)
}
# 
# library(gWidgets2)
# options(guiToolkit = "RGtk2")
# load('~/aexp.RData')
# choices=''
# andexp=''
# for(i in 1:nrow(aexp)){
#   andexp[1]=as.character(aexp$andexp.1.[i])
#   andexp[2]=as.character(aexp$andexp.2.[i])
#   andexp[3]=as.character(aexp$andexp.3.[i])
#   andexp[4]=as.character(aexp$andexp.4.[i])
#   svh=as.character(aexp$svh[i])
#   anders = capture.output(cat(as.character(andexp[1:(len(andexp)-1)]),sep=' & '))
#   choices[i]=gsub('& . ','',paste('REGEXP FILTER ',svh,' & ', ifelse(as.logical(andexp[4]),'','!'),anders,' '))
# }
# rval=selList(choices)
# print(rval)

