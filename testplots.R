source('~/pllist.git/GDF.R')
source('~/pllist.git/FFMPEGProgressBar.R')
getFnx = function() return(tab[svalue(tab,index=TRUE),'fnx'])
gdfopen=FALSE
eww=NA
metadata=''
tt=as.numeric(proc.time())[3]
Epasst=TRUE
rang=1:nrow(fnames)
rang=which(!grepl('RPDNClips',fnames$fnx))
checked=FALSE
if(len(linerd))
  if(toupper(linerd)=='RPDNCLIPS'){
    checked=TRUE
    rang=which(grepl('RPDNClips',fnames$fnx))
  }
regexfilt=''
chula="Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n"
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
  w <- gwindow(paste(linerd,nrow(fnames[rang,]),'files',chula),
               width = 1900,height=heit,parent = c(0,0),visible=FALSE)
  getToolkitWidget(w)$move(0,0)
  gp <- ggroup(horizontal = FALSE, container = w)
  .GlobalEnv$tpexist <- TRUE
  
  tab <- gtable(fnames[rang,], container = gp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  if(isExtant(.GlobalEnv$eww))
                    dispose(.GlobalEnv$eww)
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
    print(svalue(h$action))
    dispose(.GlobalEnv$eww)
    if ((length(.GlobalEnv$fnx) > 0) & !.GlobalEnv$gdfopen) {
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
      if(!identical(tmpx,fwind)){
        .GlobalEnv$changed=TRUE
        .GlobalEnv$Passt=TRUE
        gtkMainQuit()
      }
    }
  }
  
  addHandlerSelectionChanged(tab, handler = function(h,...) {
    zz=unlist(strsplit(shell('GetCursorPos.exe',intern = TRUE)," "))
    .GlobalEnv$fnx=getFnx()
    lenn=len(.GlobalEnv$fnx)
    if(lenn==1){
      if(isExtant(.GlobalEnv$eww))
        dispose(.GlobalEnv$eww)
      shell('nircmd win close title "Action"')
      .GlobalEnv$eww=gwindow('Action',width=30,height=30,visible=FALSE,parent = c(0,0))
      ew=.GlobalEnv$eww
      getToolkitWidget(ew)$move(ifelse(as.numeric(zz[1])>1200,"1200",zz[1]),zz[2])
      gpp=ggroup(cont=ew)
      ewb=gbutton('EDIT',cont=gpp,handler=gf)
      ewb1=gbutton('DELETE',cont=gpp,handler=function(h,...) {
        dispose(ew)
        .GlobalEnv$svt=fnx
        unlink(fnx)
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$Passt=TRUE
        dispose(w)
      })
      ewb2=gbutton("TRIM",cont=gpp,handler=function(h,...) {
        .GlobalEnv$Fdate=FALSE
        dispose(ew)
        .GlobalEnv$svt=fnx
        StartMyGUI()
        if(!is.null(.GlobalEnv$ss)){
          .GlobalEnv$nxflag=TRUE
          .GlobalEnv$rebuild=TRUE
          .GlobalEnv$tpexist <- FALSE
          .GlobalEnv$Passt=TRUE
          .GlobalEnv$liner=NULL
          .GlobalEnv$trimmed=TRUE
          dispose(w)
        }
      })
      ewb3=gbutton('PLAY',cont=gpp,handler=function(h,...) {
        visible(ew)=FALSE
        shell(fnx)
        visible(ew)=TRUE
      })
      ewb4=gbutton('EXPLORE',cont=gpp,handler=function(h,...) {
        dispose(ew)
        shell(paste('c:/Users/Larry/Documents/hexDump/bin/explorerselect.bat "',fnx,'" ',',' ,sep=''),translate = TRUE, 
              intern = TRUE)
      })
      mvb=gbutton('MOVE',cont=gpp,handler=function(h,...) {
        dispose(ew)
        xx=c(dir('C:/pnmtall',full.names = TRUE),dir('d:/pnmtall',full.names = TRUE))
        drr=xx[menu(xx,graphics=TRUE)]
        rrss=gconfirm('Are You Sure you wanna move')
        if(rrss){
          rslt=file.rename(fnx,paste(drr,'/',basename(fnx),sep=''))
          if(!result){
            galert('File Move NOT Successful')
          }else{
            galert('File Move was Successful')
            .GlobalEnv$Passt=TRUE
            dispose(w)
          }
        }
      })
      ewb=gbutton('METADATA',cont=gpp,handler=function(h,...) {
        dispose(ew)
        editMeta()
      })
      
      
      visible(ew) = TRUE
      focus(ew)=TRUE
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
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  
  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)
  
  gg=gcheckbox('Include RPDN',cont=bg, checked=checked,handler = function(h,...) {
    if(svalue(gg)){
      rng=which(grepl('.',paste(fnames$fnx,fnames$comments),ignore.case = TRUE))
      rang=rng
    }else{
      rng=which(!grepl('RPDNClips',paste(fnames$fnx,fnames$comments),ignore.case = TRUE))
      
    }
    rng=rng[rng %in% rang] # if rexExp filter in use rang contains the subset
    tab[,]=fnames[rng,]
    svalue(w)=paste(len(rng),'files',chula)
    if(len(rang)!=nrow(fnames))
      svalue(w)=paste('REGEXP FILTER',.GlobalEnv$regexfilt,len(rng),'files',chula)
  })
  
  rbb=gbutton("REBUILD", container=bg, handler = function(h,...) {
    .GlobalEnv$nxflag=TRUE
    .GlobalEnv$rebuild=TRUE
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$liner=NULL
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
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
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    .GlobalEnv$ANDflag = TRUE
    .GlobalEnv$ORflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$srchF=TRUE
    gtkMainQuit()
  })
  font(ANDButton) <- c(color="yellow4" , weight="bold") # initial RED to indicate 'AND' condition
  
  ORButton=gbutton("OR", container = bg, handler = function(h,...) {
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
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
      if(isExtant(.GlobalEnv$eww)) ################# PUT rpdn check box filter here #############
      dispose(.GlobalEnv$eww)
      rng=which(grepl(svalue(h$obj),paste(fnames$fnx,fnames$comments),ignore.case = TRUE))
      if(!svalue(gg)){
        rng=which(grepl(svalue(h$obj),paste(fnames$fnx,fnames$comments),ignore.case = TRUE)&
                    !grepl('RPDNClips',paste(fnames$fnx,fnames$comments)))
      }
      tab[,]=fnames[rng,]
      .GlobalEnv$rang=rng
      enabled(btns)=TRUE
      svalue(w)=paste('REGEXP FILTER',svalue(h$obj),len(rng),'files',chula)
      .GlobalEnv$regexfilt=svalue(h$obj)
    })
    
    btn=gbutton('Clear RegEx',cont=bg)
    list(
      run = function(partner) {
        addHandlerChanged(btn, handler = function(h, ...) {
          if(isExtant(.GlobalEnv$eww))
            dispose(.GlobalEnv$eww)
          svalue(partner$ge) <- ''
          .GlobalEnv$regexfilt=''
          svalue(w)=paste(linerd,nrow(fnames[rang,]),'files',chula)
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
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    tab[,]=fnames
    svalue(tab)=.GlobalEnv$rang
  })
  
  xbutton=gbutton("Explore", container = bg, handler = function(h,...) {
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    fn=getFnx()
    shell(paste('c:/Users/Larry/Documents/hexDump/bin/explorerselect.bat "',fn,'" ',',' ,sep=''),translate = TRUE, 
          intern = TRUE)
    enabled(MLButton) = TRUE
  })
  
  ebutton=gbutton("Edit", container = bg, handler = gf)
  
  dbutton=gbutton("Delete", container = bg, handler = function(h,...) {
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
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
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
    startt=NULL
    .GlobalEnv$alrt=galert(svt,delay=10000)
    StartMyGUI()
    if(exists('alrt'))
      if(isExtant(alrt))
        dispose(alrt)
    if(!is.null(.GlobalEnv$ss)){
      .GlobalEnv$nxflag=TRUE
      .GlobalEnv$rebuild=TRUE
      .GlobalEnv$tpexist <- FALSE
      .GlobalEnv$Passt=TRUE
      .GlobalEnv$liner=NULL
      .GlobalEnv$trimmed=TRUE
      dispose(w)
      .GlobalEnv$tpexist=FALSE
      gtkMainQuit()
    }
  })
  
  editMeta=function() {
    enabled(w) <- FALSE
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
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
    
    wm <- gwindow(paste("Metadata-",svt),width=700,visible = FALSE)
    gpm<- ggroup(horizontal=FALSE, container=wm)
    tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE
                   ,handler=function(h,...){
                     lnn=tabm[svalue(h$obj,index=TRUE),]
                     if(grepl('Date',lnn$X1)){
                        gxx=trim(unlist(strsplit(lnn$X2,' |:|\\-')))
                        fss=paste(gxx[2],'-',gxx[3],'-',gxx[4],' ',gxx[5],':',gxx[6],':',gxx[7],sep='')
                        if(len(fss)){
                          print(paste('fss=',fss,svt))
                          dx=data.frame(dtn=NA,fn=NA,times=NA)
                          dx[1,'dtn']=fss
                          dx$dtn=as.POSIXlt(dx$dtn)+(7*3600) # add 7 hours to make GMT
                          dx[1,'fn']=normalizePath(as.character(svt),winslash = '/')
                          dx[1,'times']=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                                              ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
                          
                          cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
                          eval(parse(text=cmd))
                          dispose(wm)
                        }
                     }
                     })
    
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
    cmts=mg[grepl('title|comment',mg$X1,ignore.case = TRUE),]
    cmts=cmts[!duplicated(paste(trim(cmts$X1),trim(cmts$X2))),]
    mg=rbind(cmts,mg)
    tabm[,]=mg
    .GlobalEnv$metadata = mg
    visible(wm) <- TRUE
    bgm <- ggroup(container=gpm)
    addSpring(bgm)
    
    rgx=gedit(' ',cont=bgm,handler=function(h,...){
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
    focus(rgx)=TRUE
    addHandlerDestroy(wm,function(h,...) {
      enabled(w) <- TRUE
    })
  }
  
  mbutton=gbutton("Metadata", container = bg, handler = function(h,...) {
    editMeta()
  })
  
  dbgbutton=gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
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
  enabled(gg)=!checked
  
}else{
  tab[,]=fnames
  linerd=liner
  if(!ANDflag)
    linerd=gsub(' ','|',liner)
  svalue(w)=paste(linerd,nrow(fnames),'files',chula)
}
getToolkitWidget(w)$move(0,0)
visible(w)=TRUE
if(exists('gxy'))
  if(isExtant(gxy))
    dispose(gxy)
srchF=FALSE
focus(xe)=TRUE
#focus(ge)=TRUE
focus(tab)=TRUE # refresh initial message

FUN1 <- function(data) {
  dd=shell('dir C:\\RealPlayerDownloads /S/B/A',intern = TRUE)
  dd=dd[grepl('.mp4|.mov',dd)]
  rrxx=file.size(dd)
  if(exists('MLButton')){
    Sys.sleep(.5)
    if(isExtant(MLButton)){
      enabled(MLButton)=sum(rrxx)>129
    }
  }
}
a1 <- gtimer(250, FUN1)

gtkMain()
a1$stop_timer()

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







