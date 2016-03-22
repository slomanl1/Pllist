getFnx = function() return(tab[svalue(tab,index=TRUE),'fnx'])

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
    lenn=len(getFnx())
    if(lenn==1){
      enabled(dbutton)=(len(svalue(tab))!=0) # delete button
      enabled(tbutton)=(len(svalue(tab))!=0) # TRIM button
      enabled(mbutton)=(len(svalue(tab))!=0) # metadata button
      enabled(ebutton)=(len(svalue(tab))!=0) # edit button
      enabled(xbutton)=(len(svalue(tab))!=0) # explore button
    }else{ 
      enabled(dbutton)=FALSE # delete button
      enabled(tbutton)=FALSE # TRIM button
      enabled(mbutton)=FALSE # metadata button
      enabled(ebutton)=FALSE # edit button
      enabled(xbutton)=FALSE # explorer button
    }
  })
  
  addHandlerRightclick(
    tab, handler = function(h,...) {
      if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
        .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
        idx=which(fnames$fnx==.GlobalEnv$svt)
        print(paste('svt,idx=',svt,idx))
        .GlobalEnv$mtme=file.mtime(fnames[idx,'fnx'])
        # supply select idx item in editing window fwinf
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
  
  MLButton=gbutton("MakeLast.R", container = bg, handler = function(h,...) {
    enabled(MLButton) = FALSE
    shell('"c:/Program Files/R/R-3.2.4revised/bin/rscript.exe" c:/Users/Larry/Documents/Pllist.git/makelast.R',translate = TRUE)
    shell('nircmd.exe win close class "CabinetWClass"')
  }
  )
  
  ANDButton=gbutton("AND", container = bg, handler = function(h,...) {
    .GlobalEnv$ANDflag = TRUE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    gtkMainQuit()
  }
  )
  font(ANDButton) <- c(color="yellow4" , style="bold") # initial RED to indicate 'AND' condition
  .GlobalEnv$ANDflag = TRUE
  
  ORButton=gbutton("OR", container = bg, handler = function(h,...) {
    .GlobalEnv$ANDflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    gtkMainQuit()
  }
  )
  font(ORButton) <- c(color="blue", style="bold") # initial 

  xe=gedit(container=bg, initial.msg='Enter Search Criteria', handler = function(h,...) {
    if(enabled(ANDButton)){
      .GlobalEnv$ANDflag = TRUE
      .GlobalEnv$avail = TRUE
      .GlobalEnv$Passt=TRUE
      gtkMainQuit()
    }
  })
  
  addhandlerkeystroke(xe, handler = function(h,...) {
    enabled(ANDButton) = FALSE #
    enabled(ORButton) = FALSE #
    if(nchar(svalue(h$obj))){
      enabled(ANDButton) = TRUE #
      enabled(ORButton) = TRUE #
      .GlobalEnv$liner=svalue(h$obj)
    }
  })
  
   ge=gedit(container=bg, initial.msg='Enter Search RegExp Filter', handler = function(h,...) {
    rng=which(grepl(svalue(h$obj),paste(fnames$fnx,fnames$comments),ignore.case = TRUE))
    tab[,]=fnames[rng,]
  })

  xbutton=gbutton("Explore", container = bg, handler = function(h,...) {
    fn=getFnx()
    shell(paste('c:/Users/Larry/Documents/hexDump/bin/explorerselect.bat "',fn,'" ',',' ,sep=''),translate = TRUE, 
          intern = TRUE)
    enabled(MLButton) = TRUE
  }
  )
  
  ebutton=gbutton("Edit", container = bg, handler = function(h,...) {
    if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
      .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
      idx=which(fnames$fnx==.GlobalEnv$svt)
      print(paste('svt,idx=',svt,idx))
      .GlobalEnv$mtme=file.mtime(fnames[idx,'fnx'])
      # supply select idx item in editing window fwinf
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
    .GlobalEnv$alrt=galert(svt,delay=10000)
    StartMyGUI()
    dispose(alrt)
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
    meta=meta[nchar(meta)>0]
    mm=matrix(NA,len(meta),2)
    pos=gregexpr(':',meta)
    for (i in 1:len(meta))
      pos[i]=pos[[i]][1]
    pos=unlist(pos)
    pos[pos==-1]=1
    meta[pos==1]=paste(':',meta[pos==1])
    mm[,1]=substr(meta,1,pos-1)
    mm[,2]=substr(meta,pos+1,nchar(meta))
    mg=data.frame(mm,stringsAsFactors = FALSE)
    tabm[,]=mg
  }
  )
  
  gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    dispose(w)
    dispose(fwind)
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  enabled(dbutton)=FALSE # delete button
  enabled(tbutton)=FALSE # TRIM button
  enabled(mbutton)=FALSE # metadata button
  enabled(ebutton)=FALSE # edit button  
  enabled(xbutton)=FALSE # explore button 
  enabled(MLButton) = FALSE # MakeLast
  enabled(ANDButton) = FALSE #
  enabled(ORButton) = FALSE #

}else{
  tab[,]=fnames
  linerd=liner
  if(!ANDflag)
    linerd=gsub(' ','|',liner)
  svalue(w)=paste(linerd,nrow(fnames),"Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n")
  visible(w)=TRUE
}
gtkMain()


