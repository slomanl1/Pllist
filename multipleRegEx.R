testRegex = function(rgx) {
  err=capture.output(tryCatch.W.E(grepl(rgx,'testregexponly')))
  if(any(grepl('invalid regular expression',err))){
    return(FALSE)
  }
  return(TRUE)
}

getAndExp = function(){
  .GlobalEnv$notFlg=FALSE
  .GlobalEnv$a11='.'
  .GlobalEnv$a22='.'
  .GlobalEnv$a33='.'
  wwx1=gwindow('REGEX 1',height=30,parent = c(200,200))
  ww1=ggroup(cont=wwx1)
  gbb=gbutton('NOT',cont=ww1,handler = function(h,...){
    focus(a1)=TRUE
    .GlobalEnv$notFlg=!.GlobalEnv$notFlg
    svalue(wwx1)=ifelse(notFlg,'NOT REGEX 1','REGEX 1')
  })
  a1=gedit(cont=ww1,handler=function(h,...){
    if(nchar(svalue(a1))==0){
      dispose(a1)
      gtkMainQuit()
    }else{
      .GlobalEnv$a11=svalue(a1)
      if(!testRegex(.GlobalEnv$a11)){
        galert('BAD REGEX 1')
        return()
      }
      ww2=gwindow('REGEX 2',height=30,parent = c(200,275))
      a2=gedit(cont=ww2,handler=function(h,...){
        if(nchar(svalue(a2))==0){
          dispose(a2)
          dispose(a1)
          gtkMainQuit()
        }else{
          .GlobalEnv$a22=svalue(a2)
          if(!testRegex(.GlobalEnv$a22)){
            galert('BAD REGEX 2')
            return()
          }
          ww3=gwindow('REGEX 3',height=30,parent = c(200,350))
          a3=gedit(cont=ww3,handler=function(h,...){
            .GlobalEnv$a33=svalue(a3)
            if(!testRegex(.GlobalEnv$a33)){
              galert('BAD REGEX 3')
              return()
            }
            dispose(a2)
            dispose(a1)
            dispose(a3)
            gtkMainQuit()
          })
          addHandlerDestroy(ww3,handler=function(h,...){
            if(isExtant(ww2))
              dispose(ww2)
            gtkMainQuit()
          })
          focus(a3)=TRUE
        }
      })
      addHandlerDestroy(ww2,handler=function(h,...){
        dispose(ww1)
        gtkMainQuit()
      })
      
      focus(a2)=TRUE
    }
  })
  focus(a1)=TRUE
  focus(a1)=FALSE
  focus(a1)=TRUE
  addHandlerDestroy(wwx1,handler=function(h,...){
    gtkMainQuit()
  })
  gtkMain()
  return(list(.GlobalEnv$a11,.GlobalEnv$a22,.GlobalEnv$a33,!as.logical(.GlobalEnv$notFlg)))
}


