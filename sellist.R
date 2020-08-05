selList = function(items) {
  .GlobalEnv$retval=NULL
  w <- gwindow("selList",visible = FALSE,height=50)
  g <- ggroup(cont=w,horizontal = FALSE)
  ph <- ggroup(cont=g);  
  gt=gtable(items=items, cont=ph,multiple = FALSE)
  pg <- ggroup(cont=g); 
  rr=gbutton("Remove", cont=pg,handler=function(h,...){
    print('Remove Handler')
    ix=svalue(gt,index = TRUE)
    if(nrow(aexp)>0)
      .GlobalEnv$aexp=aexp[-1*ix,] # remove row ix
    xx=c(gt[1:ix-1,],gt[(ix+1):nrow(gt),])
    gt[1:len(xx),1]=xx
    gt[nrow(gt),1]=''
    enabled(uu)=TRUE
    enabled(ss)=TRUE
  }); 
  cc=gbutton("Clear All", cont=pg,handler=function(h,...){
    gt[,]=gt[0,]
    .GlobalEnv$aexp=.GlobalEnv$aexp[0,]
    enabled(uu)=TRUE
    enabled(ss)=TRUE    
    enabled(ee)=FALSE # EDIT
    enabled(rr)=FALSE # REMOVE
    enabled(kk)=FALSE # OK
  }); 
  kk=gbutton("OK", cont=pg,handler=function(h,...){
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=0,aexpadd=NULL)
    dispose(w)
    gtkMainQuit()
  }); 
  nn=gbutton("Cancel", cont=pg,handler=function(h,...){
    getAndExp(disposer=TRUE)
    .GlobalEnv$retval=1
    dispose(w)
    gtkMainQuit()
  })
  ss=gbutton("SAVE", cont=pg,handler=function(h,...){
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=1,aexpadd=NULL)
    dispose(w)
    gtkMainQuit()
  }) 
  uu=gbutton("UNDO", cont=pg,handler=function(h,...){
    .GlobalEnv$retval=NULL # indicate UNDO
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
    #browser()
    idxx=svalue(gt,index=TRUE)
    andexp1=items[idxx]
    andexp2=substr(andexp1,15,nchar(andexp1)) # remove REGEXP FILTER:
    andexp3=unlist(strsplit(andexp2,'&')) # restore AND array
    andexp3=substr(andexp3,2,nchar(andexp3)-1) # remove 1 leading and trailing space
    bgx=gwindow(height=40)
    gex=gedit(andexp3[1],container=bgx, handler=function(h,...){
      svh=svalue(gex)
      #browser()
      andexp4=unlist(getAndExp(andexp3[2],andexp3[3],andexp3[4]))
      andexp=trim(andexp4)
      print('andexp=')
      print(andexp)
      anders = capture.output(cat(as.character(andexp[1:(len(andexp)-1)]),sep=' & '))
      gt[idxx,1]=paste('REGEXP FILTER: ',svh,' & ', ifelse(as.logical(andexp[4]),'','!'),anders,' ',sep='')
      aexpadd=data.frame(andexp[1],andexp[2],andexp[3],andexp[4],svh)
      .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=2,aexpadd=aexpadd)
      dispose(bgx)
      gtkMainQuit()
    })
    focus(gex)=TRUE
    gtkMain()
    dispose(w)
    gtkMainQuit()
  })  
  
  addHandlerDestroy(w,handler=function(h,...){
    gtkMainQuit()
  })  
  addHandlerDoubleclick(gt,handler=function(h,...){
    #print('Double Click Handler')
    .GlobalEnv$retval=list(rv=svalue(gt),gt=data.frame(gt[,],stringsAsFactors = FALSE)[nchar(gt[,])>0,],saved=FALSE,aexpadd=NULL)
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
  #  focus('kk')=TRUE
  visible(w)=TRUE
  gtkMain()
  return(.GlobalEnv$retval)
}


