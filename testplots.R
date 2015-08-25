idxs=NULL
for(x in 1:nrow(fnames)) 
  idxs=c(idxs,which(grepl(fnames[x,'fnx'],an[ttl],fixed=TRUE)))
print(.GlobalEnv$changed)
if (!.GlobalEnv$tpexist) {
  avail = FALSE
  renamed = FALSE
  ssv = NULL
  
  fwind=gdf(fnames[1,], container=gwindow("Edit File Details",width=1900,height = 20))
  enabled(fwind) <- FALSE
  addhandlerchanged(fwind, handler = function(h,...) 
  { print('changed handler (fwind)')
    .GlobalEnv$changed=TRUE
    .GlobalEnv$avail=TRUE
    enabled(fwind) <- FALSE
  })
  
  addHandlerDestroy(fwind, handler = function(h,...) 
  { print('destroy handler (fwind)')
    .GlobalEnv$avail=TRUE
    .GlobalEnv$ofnx=NULL
    if(isExtant(w))
      dispose(w)
    tpexist=FALSE
  }) 
  
  heit=min((nrow(fnames)*25),650)
  w <- gwindow(paste(liner,"Choose One or More Files, Click Right to Edit Filename and Comments\n"),width = 1900,height=heit,parent = c(0,0))
  gp <- ggroup(horizontal = FALSE, container = w)
  .GlobalEnv$tpexist <- TRUE
  tab <- gtable(fnames, container = gp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  print(svalue(h$obj))
                  .GlobalEnv$unsorted=is.unsorted(tab[,'cdts'])
                  print(paste('hdl isunsorted=',.GlobalEnv$unsorted))
                  .GlobalEnv$ssv = as.character(svalue(h$obj))
                  .GlobalEnv$avail = TRUE
                }
  )
  addHandlerRightclick(
    tab, handler = function(h,...) {
      if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
        .GlobalEnv$idx=which(grepl(basename(as.character(svalue(h$obj)))[1],fnames$fnx,fixed = TRUE))
        print(paste('idx=',idx))
        .GlobalEnv$ofnx=fnames[idx,]
        ofnxa=fnames[idx,]
        nfn=NULL
        fwind[,]=ofnxa # supply select idx item in editing window fwinf
        enabled(fwind) <- TRUE
        visible(w) <- FALSE
      }
    }
  )
  addHandlerDestroy(
    tab, handler = function(h,...) {
      .GlobalEnv$ssv = NULL
      .GlobalEnv$avail = TRUE
      .GlobalEnv$tpexist <- FALSE
      print('tab destroyed handler')
      if(isExtant(fwind))
        dispose(fwind) # gdf window
      .GlobalEnv$gdfopen=FALSE
    }
  )
  print('bghello')
  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)

  dbutton=gbutton("Delete", container = bg, handler = function(h,...) {
    answ=gconfirm('Are you Sure?')
    if(answ)
      unlink(svalue(tab))

  }
  )
  tbutton=gbutton("TRIM", container = bg, handler = function(h,...) {
    print(svalue(tab))
    cmdd=paste('shell("trimfile.bat',svalue(tab),'",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    shell("trimfile.bat")
  }
  )
  gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$avail = TRUE
    dispose(w)
    dispose(fwind)
    .GlobalEnv$gdfopen=FALSE
  }
  )
}else
  visible(w)=TRUE


