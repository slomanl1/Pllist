idxs=NULL
for(x in 1:nrow(fnames)) 
  idxs=c(idxs,which(grepl(fnames[x,'fnx'],an[ttl],fixed=TRUE)))
print(.GlobalEnv$changed)
if (!.GlobalEnv$tpexist) {
  avail = FALSE
  renamed = FALSE
  ssv = NULL
  
  fw=gwindow("Edit File Details",width=1900,height = 20)
  fwind=gdf(dfan[1,1:4], container=fw)
  getToolkitWidget(fw)$move(0,100)
  visible(fw) <- FALSE
  addhandlerchanged(fwind, handler = function(h,...) 
  { print('changed handler (fwind)')
    .GlobalEnv$changed=TRUE
    .GlobalEnv$avail=TRUE
    .GlobalEnv$Passt=TRUE
    visible(w) <- FALSE
    enabled(fw) <- FALSE
    tpexist=FALSE
  })
  
  addHandlerDestroy(fwind, handler = function(h,...) 
  { print('destroy handler (fwind)')
    .GlobalEnv$avail=TRUE
    .GlobalEnv$ofnx=NULL
    if(isExtant(w))
      dispose(w)
    tpexist=FALSE
  }) 

  heit=min(100+(nrow(fnames)*30),750)
  w <- gwindow(paste(liner,nrow(fnames),"Choose One or More Files, Click Right to Edit Filename and Comments\n"),width = 1900,height=heit,parent = c(0,0))
  getToolkitWidget(w)$move(0,0)
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
        print(paste('RC Handler idx=',idx))
        .GlobalEnv$ofnx=fnames[idx,]
        nfn=NULL  # supply select idx item in editing window fwinf
        tmpdf=dfan[grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE),]
        tmpdf$Comment=tmpdf$DMComment
        fwind[,] = tmpdf[,1:4]
        visible(fw) <- TRUE
        enabled(fw) <- TRUE
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
    if(answ){
      print(paste('Deleting',svalue(tab)))
      .GlobalEnv$idx=which(grepl(as.character(svalue(tab)),fnames$fnx,fixed=TRUE))
      print(paste('idx=',.GlobalEnv$idx))
      if(unlink(svalue(tab)))
        print('delete FAILED')
      else{
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$avail=TRUE
        .GlobalEnv$Passt=TRUE
        visible(w) <- FALSE
        tpexist=FALSE
        writeLines(svalue(tab),'file.tmp')
        file.append('deletelog.txt','file.tmp') # update delete log
        unlink('file.tmp')
      }
    }
  }
  )
  tbutton=gbutton("TRIM", container = bg, handler = function(h,...) {
    print(svalue(tab))
    cmdd=paste('shell("exx.bat',svalue(tab),'",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
  }
  )
  gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$ofnx=NULL
    dispose(w)
    dispose(fwind)
    .GlobalEnv$gdfopen=FALSE
  }
  )
}else
  visible(w)=TRUE

#exiftool BSBMp4Tester.mp4 -Subtitle="asdasd"
#exiftool BSBMp4Tester.mp4 -Title="askndppp"
