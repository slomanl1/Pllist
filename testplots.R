getFnx = function() return(tab[svalue(tab,index=TRUE),'fnx'])
idxs=NULL
for(x in 1:nrow(fnames)) 
  idxs=c(idxs,which(grepl(fnames[x,'fnx'],an[ttl],fixed=TRUE)))

if (!.GlobalEnv$tpexist) {
  renamed = FALSE
  ssv = NULL
  
  fw=gwindow("Edit File Details",width=1900,height = 20)
  fwind=gdf(dfan[1,1:4], container=fw)
  getToolkitWidget(fw)$move(0,100)
  visible(fw) <- FALSE
  addhandlerchanged(fwind, handler = function(h,...) 
  {
    .GlobalEnv$changed=TRUE
    .GlobalEnv$Passt=TRUE
    visible(w) <- FALSE
    enabled(fw) <- FALSE
    tpexist=FALSE
    gtkMainQuit()
  })
  
  addHandlerDestroy(fwind, handler = function(h,...) 
  { 
    .GlobalEnv$ofnx=NULL
    if(isExtant(w))
      dispose(w)
    tpexist=FALSE
    gtkMainQuit()
  }) 
  
  heit=min(100+(nrow(fnames)*30),750)
  linerd=liner
  if(!ANDflag)
    linerd=gsub(' ','|',liner)
  w <- gwindow(paste(linerd,nrow(fnames),"Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n"),width = 1900,height=heit,parent = c(0,0))
  getToolkitWidget(w)$move(0,0)
  gp <- ggroup(horizontal = FALSE, container = w)
  .GlobalEnv$tpexist <- TRUE
  
  tab <- gtable(fnames, container = gp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  .GlobalEnv$unsorted=is.unsorted(tab[,'cdts'])
                  .GlobalEnv$ssv = getFnx()
                  gtkMainQuit()
                }
  )
  addHandlerClicked(tab, handler = function(h,...) {
    .GlobalEnv$lenn=len(getFnx())
    if(lenn==1){
      enabled(dbutton)=(len(svalue(tab))!=0) # delete button
      enabled(tbutton)=(len(svalue(tab))!=0) # TRIM button
      enabled(mbutton)=(len(svalue(tab))!=0) # metadata button
      enabled(ebutton)=(len(svalue(tab))!=0) # edit button
    }else{ 
      enabled(dbutton)=FALSE # delete button
      enabled(tbutton)=FALSE # TRIM button
      enabled(mbutton)=FALSE # metadata button
      enabled(ebutton)=FALSE # edit button
    }
  })
  
  addHandlerRightclick(
    tab, handler = function(h,...) {
      if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
        .GlobalEnv$idx=svalue(h$obj,index = TRUE)
        .GlobalEnv$ofnx=fnames[idx,]
        .GlobalEnv$mtme=file.mtime(fnames[idx,'fnx'])
        nfn=NULL  # supply select idx item in editing window fwinf
        tmpdf=dfan[grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE),]
        if(!is.na(tmpdf$DMComment))
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
      .GlobalEnv$tpexist <- FALSE
      if(isExtant(fwind))
        dispose(fwind) # gdf window
      .GlobalEnv$gdfopen=FALSE
      if(isExtant(wm))
        visible(wm) <- FALSE
      gtkMainQuit()
    }
  )
  
  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)
  #rb <- gtable(fnames$fnx[1:3], container=bg,width=500)
  ebutton=gbutton("Edit", container = bg, handler = function(h,...) {
    if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
      .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
      idx=which(fnames$fnx==.GlobalEnv$svt)
      .GlobalEnv$idx=idx
      print(paste('svt,idx=',svt,idx))
      .GlobalEnv$ofnx=fnames[idx,]
      .GlobalEnv$mtme=file.mtime(fnames[idx,'fnx'])
      nfn=NULL  # supply select idx item in editing window fwinf
      tmpdf=dfan[grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE),]
      if(!is.na(tmpdf$DMComment))
        tmpdf$Comment=tmpdf$DMComment
      fwind[,] = tmpdf[,1:4]
      visible(fw) <- TRUE
      enabled(fw) <- TRUE
      visible(w) <- FALSE
    }
  })
  
  
  dbutton=gbutton("Delete", container = bg, handler = function(h,...) {
    answ=gconfirm('Are you Sure?')
    if(answ){
      print(paste('Deleting',getFnx()))
      .GlobalEnv$idx=svalue(tab,index=TRUE)
      if(unlink(getFnx()))
        print('delete FAILED')
      else{
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$Passt=TRUE
        visible(w) <- FALSE
        tpexist=FALSE
        writeLines(getFnx(),'file.tmp')
        file.append('deletelog.txt','file.tmp') # update delete log
        unlink('file.tmp')
        gtkMainQuit()
      }
    }
  }
  )
  tbutton=gbutton("TRIM", container = bg, handler = function(h,...) {
    .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
    startt=NULL
    print(paste('svt=',.GlobalEnv$svt))
    StartMyGUI()
  }
  )
  
  wm <- gwindow("Metadata",width=700)
  visible(wm) <- FALSE
  gpm<- ggroup(horizontal=FALSE, container=wm)
  tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE,
                 handler = NULL)
  bgm <- ggroup(container=gpm)
  addSpring(bgm)
  addHandlerDestroy(
    tabm, handler = function(h,...) {
      if(exists('w')) 
        if(isExtant(w)) 
          dispose(w)
      gtkMainQuit()})
  
  gbutton("dismiss", container=bgm, handler = function(h,...) {
    visible(wm) <- FALSE;    
    if(exists('w')) 
      if(isExtant(w)) 
        enabled(w) <- TRUE
  })
  
  mbutton=gbutton("Metadata", container = bg, handler = function(h,...) {
    enabled(w) <- FALSE
    svt=normalizePath(getFnx(),winslash = '/')
    print(svt)
    cmdd=paste('shell("mediainfo.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    cmdd=paste('shell("exiftool',svt,' >>meta.txt",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    .GlobalEnv$meta=readLines('meta.txt')
    unlink('meta.txt')
    visible(wm) <- TRUE
    mm=matrix(NA,len(meta),2)
    mm[,1]=substr(meta,1,41)
    mm[,2]=substr(meta,42,nchar(meta))
    mg=data.frame(mm,stringsAsFactors = FALSE)
    tabm[,]=mg
  }
  )
  
  gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$ofnx=NULL
    dispose(w)
    dispose(fwind)
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  enabled(dbutton)=FALSE # delete button
  enabled(tbutton)=FALSE # TRIM button
  enabled(mbutton)=FALSE # metadata button
  enabled(ebutton)=FALSE # edit button  
  
}else{
  visible(w)=TRUE
}
gtkMain()


