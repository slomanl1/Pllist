source('~/pllist.git/GDF.R')
getFnx = function() return(tab[svalue(tab,index=TRUE),'fnx'])
gdfopen=FALSE
metadata=''
tt=as.numeric(proc.time())[3]
Epasst=TRUE
if (!tpexist) {
  renamed = FALSE
  ssv = NULL
  
  heit=min(100+(nrow(fnames)*30),750)
  linerd=liner
  if(ANDflag){
    linerd=gsub(' ','&',liner)
  }else{
    linerd=gsub(' ','|',liner)
  }
  w <- gwindow(paste(linerd,nrow(fnames),"Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n"),
               width = 1900,height=heit,parent = c(0,0),visible=FALSE)
  getToolkitWidget(w)$move(0,0)
  gp <- ggroup(horizontal = FALSE, container = w)
  .GlobalEnv$tpexist <- TRUE
  
  tab <- gtable(fnames, container = gp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  visible(ew)=FALSE
                  .GlobalEnv$ssv = getFnx()
                  enabled(w)=FALSE
                  if(any(grepl('.flv',ssv)))
                  {
                    writeLines(gsub('/','\\\\',ssv),'fns.m3u') # Write playlist
                    shell('"C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe " fns.m3u')
                  }else{
                    writeLines(ssv,'fns.m3u') # Write playlist
                    shell('mpc-hc64.exe fns.m3u') 
                  }
                  unlink('~/fns.m3u')
                  enabled(w)=TRUE
                }
  )
  
  gf = function(h,...) {
    visible(ew)=FALSE
    if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
      .GlobalEnv$gdfopen=TRUE # block edit
      print('gdfopen set')
      .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
      idx=which(fnames$fnx==.GlobalEnv$svt)
      print(paste('svt,idx=',svt,idx))
      .GlobalEnv$mtme=file.mtime(fnames[idx,'fnx'])
      # supply select idx item in editing window 
      tmpdf=dfan[grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE),]
      svalue(tab)=0 # unselect
      if(!is.na(tmpdf$DMComment))
        tmpdf$Comment=tmpdf$DMComment
      tmpx=tmpdf[,1:4]
      enabled(dbutton)=FALSE # delete button
      enabled(tbutton)=FALSE # TRIM button
      enabled(mbutton)=FALSE # metadata button
      enabled(ebutton)=FALSE # edit button
      enabled(xbutton)=FALSE # explorer button
      enabled(tab)=FALSE
      enabled(dbgbutton)=FALSE # DISMISS button
      enabled(rbb)=FALSE # rebuild button
      .GlobalEnv$fwind=gdfd(tmpx)
      .GlobalEnv$gdfopen=FALSE
      enabled(tab)=TRUE # rebuild button
      enabled(dbgbutton)=TRUE # DISMISS button
      enabled(rbb)=TRUE
      for(i in 1:4)
        if(is.na(tmpx[,i]))
          tmpx[,i]=' '
      if(!all(tmpx==fwind)){
        .GlobalEnv$changed=TRUE
        .GlobalEnv$Passt=TRUE
        gtkMainQuit()
      }
    }
  }
  
  ew=gwindow(width=30,height=30,visible=FALSE,parent = c(0,0))
  ewb=gbutton('EDIT',cont=ew,handler=gf)
  addHandlerDestroy(ew,handler=function(h,...){
    .GlobalEnv$Passt=.GlobalEnv$Epasst |  .GlobalEnv$rebuild
    .GlobalEnv$tpexist=FALSE
    if(isExtant(w))
      dispose(w)
    gtkMainQuit()
  })
  
  addHandlerSelectionChanged(tab, handler = function(h,...) {
    fnx=getFnx()
    lenn=len(fnx)
    if(lenn==1){
      visible(ew) = TRUE
      focus(ew)=TRUE
      fxx=svalue(tab,index=TRUE)
      if(fxx<7)
        fxx=fxx+5
      if(fxx>25)
        fxx=25
      getToolkitWidget(ew)$move(nchar(fnx)*7,(fxx-1)*20)
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
  
  addHandlerDestroy(w, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$gdfopen=FALSE
    if(isExtant(wm))
      visible(wm) <- FALSE
    if(isExtant(ew)){
      .GlobalEnv$Epasst=FALSE
      dispose(ew)
    }
    gtkMainQuit()
  })
  
  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)
  
  rbb=gbutton("REBUILD", container=bg, handler = function(h,...) {
    .GlobalEnv$nxflag=TRUE
    .GlobalEnv$rebuild=TRUE
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$liner=NULL
    dispose(w)
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  
  MLButton=gbutton("MakeLast.R", container = bg, handler = function(h,...) {
    gxx=galert('MAKELAST - WORKING',delay=1000)
    enabled(rbb) = FALSE   # rebuild button
    enabled(MLButton) = FALSE
    enabled(dbutton)=FALSE # delete button
    enabled(tbutton)=FALSE # TRIM button
    enabled(mbutton)=FALSE # metadata button
    enabled(ebutton)=FALSE # edit button  
    enabled(xbutton)=FALSE # explore button 
    shell('"c:/Program Files/R/R-3.2.5/bin/rscript.exe" c:/Users/Larry/Documents/Pllist.git/makelast.R',translate = TRUE)
    shell('nircmd.exe win close class "CabinetWClass"')
    if(exists('dbutton'))
      if(isExtant(dbutton)){
        enabled(dbutton)=TRUE # delete button
        enabled(tbutton)=TRUE # TRIM button
        enabled(mbutton)=TRUE # metadata button
        enabled(ebutton)=TRUE # edit button  
        enabled(xbutton)=TRUE # explore button
        enabled(rbb) = TRUE   # rebuild button
      }
    dispose(gxx)
  })
  
  ANDButton=gbutton("AND", container = bg, handler = function(h,...) {
    .GlobalEnv$ANDflag = TRUE
    .GlobalEnv$ORflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$srchF=TRUE
    gtkMainQuit()
  })
  font(ANDButton) <- c(color="yellow4" , weight="bold") # initial RED to indicate 'AND' condition
  
  ORButton=gbutton("OR", container = bg, handler = function(h,...) {
    .GlobalEnv$ORflag = TRUE
    .GlobalEnv$ANDflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$srchF=TRUE
    gtkMainQuit()
  })
  font(ORButton) <- c(color="blue", weight="bold") # initial 
  
  xe=gedit(container=bg, initial.msg='Enter Search Criteria', handler = function(h,...) {
    if(enabled(ANDButton)){
      .GlobalEnv$ANDflag = TRUE
      .GlobalEnv$avail = TRUE
      .GlobalEnv$Passt=TRUE
      .GlobalEnv$srchF=TRUE
      gtkMainQuit()
    }
  })
  
  addHandlerKeystroke(xe, handler = function(h,...) {
    enabled(ANDButton) = FALSE #
    enabled(ORButton) = FALSE #
    if(nchar(svalue(h$obj))){
      enabled(ANDButton) = TRUE #
      enabled(ORButton) = TRUE #
      .GlobalEnv$liner=svalue(h$obj)
    }
  })
  
  initMain <- function() {
    ge=gedit(container=bg, initial.msg='Enter Search RegExp Filter', handler = function(h,...) {
      rng=which(grepl(svalue(h$obj),paste(fnames$fnx,fnames$comments),ignore.case = TRUE))
      tab[,]=fnames[rng,]
      .GlobalEnv$rang=rng
      enabled(btns)=TRUE
      svalue(w)=paste('REGEXP FILTER',svalue(h$obj),len(rng),'files')
    })
    
    btn=gbutton('Clear RegEx',cont=bg)
    list(
      run = function(partner) {
        addHandlerChanged(btn, handler = function(h, ...) {
          svalue(partner$ge) <- ''
          svalue(w)=paste(linerd,nrow(fnames),"Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n")
          enabled(btns)=FALSE
        } )
        visible(w) <- TRUE
      },
      ge = ge
    )
  }
  
  xw=initMain()
  xw$run(xw)
  btns=gbutton('Select',cont=bg,handler = function(h, ...) {
    tab[,]=fnames
    svalue(tab)=.GlobalEnv$rang
  })
  
  xbutton=gbutton("Explore", container = bg, handler = function(h,...) {
    fn=getFnx()
    shell(paste('c:/Users/Larry/Documents/hexDump/bin/explorerselect.bat "',fn,'" ',',' ,sep=''),translate = TRUE, 
          intern = TRUE)
    enabled(MLButton) = TRUE
  })
  
  ebutton=gbutton("Edit", container = bg, handler = gf)
  
  dbutton=gbutton("Delete", container = bg, handler = function(h,...) {
    answ=gconfirm('Are you Sure?')
    if(answ){
      .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
      print(paste('Deleting',.GlobalEnv$svt))
      if(unlink(.GlobalEnv$svt))
        print('delete FAILED')
      else{
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$Passt=TRUE
        dispose(w)
        .GlobalEnv$tpexist=FALSE
        writeLines(.GlobalEnv$svt,'file.tmp')
        file.append('deletelog.txt','file.tmp') # update delete log
        unlink('file.tmp')
        gtkMainQuit()
      }
    }
  })
  
  tbutton=gbutton("TRIM", container = bg, handler = function(h,...) {
    .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
    startt=NULL
    .GlobalEnv$alrt=galert(svt,delay=10000)
    StartMyGUI()
    if(exists('alrt'))
      if(isExtant(alrt))
        dispose(alrt)
    .GlobalEnv$nxflag=TRUE
    .GlobalEnv$rebuild=TRUE
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$liner=NULL
    .GlobalEnv$trimmed=TRUE
    dispose(w)
    .GlobalEnv$tpexist=FALSE
    gtkMainQuit()
  })
  
  wm <- gwindow("Metadata",width=700,visible = FALSE)
  gpm<- ggroup(horizontal=FALSE, container=wm)
  tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE,
                 handler = NULL)
  bgm <- ggroup(container=gpm)
  addSpring(bgm)
  addHandlerDestroy(
    wm, handler = function(h,...) {
      if(exists('w')) 
        if(isExtant(w)) 
          dispose(w)
      gtkMainQuit()})
  
  gedit(metadata,cont=bgm,handler=function(h,...){
    mdd=trim(paste(.GlobalEnv$metadata[,1],.GlobalEnv$metadata[,2]))
    rng=which(grepl(svalue(h$obj),mdd,ignore.case = TRUE))
    tabm[,]=.GlobalEnv$metadata[rng,]
  })
  
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
    .GlobalEnv$metadata = mg
  })
  
  dbgbutton=gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    dispose(w)
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  
  enabled(dbutton)=FALSE # delete button
  enabled(tbutton)=FALSE # TRIM button
  enabled(mbutton)=FALSE # metadata button
  enabled(ebutton)=FALSE # edit button  
  enabled(xbutton)=FALSE # explore button 
  enabled(MLButton) = FALSE # MakeLast
  enabled(rbb) = FALSE
  enabled(ANDButton) = FALSE #
  enabled(ORButton) = FALSE #
  enabled(rbb) = TRUE
  enabled(btns)=FALSE
  
}else{
  tab[,]=fnames
  linerd=liner
  if(!ANDflag)
    linerd=gsub(' ','|',liner)
  svalue(w)=paste(linerd,nrow(fnames),"Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n")
}
visible(w)=TRUE
getToolkitWidget(w)$move(0,0)
if(exists('gxy'))
  if(isExtant(gxy))
    dispose(gxy)
srchF=FALSE
focus(xe)=TRUE
#focus(ge)=TRUE
focus(tab)=TRUE # refresh initial message

gtkMain()

if(srchF){
  srchF=FALSE
  lnr=liner
  if(ORflag){
    lnr=gsub(' ',' | ',lnr)
  }else{
    lnr=gsub(' ',' & ',lnr)
  }
  save(liner,ORflag,ANDflag,file='~/liner.RData')
  gxy=galert(paste('Searching for',lnr),delay=1000)
  Sys.sleep(1)
}







