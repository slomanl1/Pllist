if(!exists('rchecked')){
  load('~/testplots.RData')
}else{
  svalue(ggp)=FALSE
  save.image('~/testplots.RData')
}
source('~/pllist.git/GDF.R')
source('~/pllist.git/FFMPEGProgressBar.R')
source('~/pllist.git/WriteDate.R')
source('~/pllist.git/isVC1HEVC.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/multipleRegEx.R')
source('~/pllist.git/rmmovname.R')
source('~/pllist.git/selList.R')
TESTER=0
mddold=0
load('~/dirtbl.RData')
blockg=FALSE
blockr=FALSE # for AND regexp pop-up
pchecked=FALSE
rchecked=TRUE
playState=0
.GlobalEnv$STOPPED=FALSE
mwrit=FALSE
.GlobalEnv$andexp='.'

getFnx = function() {
  if(len(svalue(tab,index=TRUE))==1){
    if(svalue(tab,index=TRUE)==1 & nrow(tab)==1){
      fnxx=tab[1,]$fnx
    }else{
      fnxx=tab[svalue(tab,index=TRUE),'fnx']
    }
  }else{
    fnxx=tab[svalue(tab,index=TRUE),'fnx']
  }
  return(normalizePath(fnxx,winslash = '/'))
}

gdfopen=FALSE
SelectChanged=FALSE #init entry
eww=NA
metadata=''
tt=as.numeric(proc.time())[3]
EPasst=FALSE
istate=0
rang=1:nrow(fnames)
galb=NA
.GlobalEnv$moved=FALSE
.GlobalEnv$trimmed=FALSE

if(!rchecked)
  rang=which(!grepl('RPDNClips',fnames$fnx))

if(len(linerd))
  if(toupper(linerd)=='RPDNCLIPS'|all(dirname(as.character(fnames$fnx))=="D:/PNMTALL/RPDNClips")){
    .GlobalEnv$rchecked=TRUE
    rang=which(grepl('RPDNClips',fnames$fnx))
  }

batchFlag=FALSE
dftm=dfan[1,]
dftm$ofn=NA
dftm$ofmtime=NA
dftm$dfix=NA
Batch=dftm[0,c(names(dfan[c(1:4,6)]),'ofn','ofmtime','dfix')]
regexfilt=''
chula="Choose One or More Files or choose single file and Right Click to Edit Name/Comments\n"
oisu=FALSE
if (!tpexist) {
  renamed = FALSE
  ssv = NULL
  heit=min(100+(nrow(fnames)*30),650)
  linerd=liner
  if(ANDflag){
    linerd=gsub(' ','&',liner)
  }else{
    linerd=gsub(' ','|',liner)
  }
  w <- gwindow(paste(linerd,nrow(fnames[rang,]),'files',chula),
               width = 1366,height=heit,parent = c(0,0),visible=FALSE)
  
  getToolkitWidget(w)$maximize()
  getToolkitWidget(w)$resize(1366,heit)  
  getToolkitWidget(w)$move(-30,0)
  
  gp <- ggroup(horizontal = FALSE, container = w)
  .GlobalEnv$tpexist <- TRUE
  
  tab <- gtable(fnames[rang,], container = gp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  print('GTABLE HANDLER')
                  sctimer$stop_timer()
                  .GlobalEnv$SelectChanged=FALSE
                  if(isExtant(.GlobalEnv$eww))
                    dispose(.GlobalEnv$eww)
                  .GlobalEnv$ssv = getFnx()
                  if(len(ssv)==0){
                    return()
                  }
                  if(nchar(ssv)==0){
                    return() # file was deleted
                  }
                  .GlobalEnv$blockg=TRUE
                  .GlobalEnv$fnx=.GlobalEnv$ssv
                  keep_above(w,FALSE)
                  enabled(w)=FALSE
                  # if(any(grepl('.flv|.asf',ssv)))
                  # {
                  writeLines(gsub('/','\\\\',ssv),'fns.m3u') # Write playlist
                  shell('"C:\\Program Files\\VideoLAN\\VLC\\vlc.exe " fns.m3u')
                  # }else{
                  #   writeLines(ssv,'fns.m3u') # Write playlist
                  #   shell('fns.m3u')
                  # }
                  unlink('~/fns.m3u')
                  .GlobalEnv$playState=1 # waiting for player to end
                  .GlobalEnv$gdfopen=TRUE # block edit
                  #enabled(tbutton)=TRUE
                  enabled(mbutton)=TRUE
                  enabled(dbutton)=TRUE
                  enabled(ebutton)=TRUE
                  #enabled(xbutton)=TRUE
                  enabled(dbgbutton)=TRUE # DISMISS button
                  enabled(w)=TRUE
                  .GlobalEnv$blockg=FALSE
                }
  )
  
  gf = function(h,...) {
    tmpdf=NULL
    if ((length(.GlobalEnv$fnx) > 0) & !.GlobalEnv$gdfopen) {
      .GlobalEnv$gdfopen=TRUE # block edit
      .GlobalEnv$svt=getFnx()
      if(len(svt)>1){
        enabled(btns)=TRUE
        return() # cannot edit if multiple selections (allow PLAY)
      }
      idx=svalue(tab,index=TRUE) # selected row
      print(paste('gdfopen set',svt,'idx=',idx))
      .GlobalEnv$mtme=file.mtime(.GlobalEnv$fnames[idx,'fnx'])
      # supply select idx item in editing window 
      tmpdf=Batch[0,] #set nrow of tmpdf to zero
      if(len(idx)){
        if(batchFlag){
          tmpdf=Batch[grepl(toupper(trim(.GlobalEnv$fnames[idx,'fnx'])),toupper(Batch$filename),fixed=TRUE),]
          if(nrow(tmpdf))
            tmpdf$DMComment=NA
        }
        
        zz=harvestMeta(tab[idx,'fnx']) # get metadata for newly renamed file at selected tab idx
        dd=zz[[1]]
        tmpdf[1,]=NA
        tmpdf$filename=normalizePath(trim(dd[which(trim(dd$X1)=='Complete name'),'X2']),winslash = '/')
        ttl=dd[which(trim(dd$X1)=='Title'),'X2']
        if(len(ttl))
          tmpdf$Title=ttl
        sttl=dd[which(trim(dd$X1)=='Subtitle'),'X2']
        if(len(sttl))
          tmpdf$SubTitle=sttl
        dmc=dd[which(trim(dd$X1)=='DM Comment'),'X2']
        if(len(dmc))
          tmpdf$DMComment=dmc
        stdo=dd[which(trim(dd$X1)=='XMP Toolkit'),'X2']
        if(len(stdo)>0)
          tmpdf$studio=stdo
        tmpdf$Obn=NA
        Obx=dd[which(trim(dd$X1)=='GPS Latitude'),'X2']
        if(len(Obx)>0){
          Obn=strsplit(Obx,' ')[[1]][2]
          if(len(Obn)>0)
            tmpdf$Obn=Obn
        }
      }
      if(len(tmpdf$DMComment)){
        if(!is.na(tmpdf$DMComment))
          tmpdf$Comment=tmpdf$DMComment
      }
    }else{
      return()
    }
    tmpdf[is.na(tmpdf)]=''
    tmpx=trim(tmpdf[,c('filename','Title','Comment','SubTitle','studio','Obn')])
    enabled(dbutton)=FALSE # delete button
    #enabled(tbutton)=FALSE # TRIM button
    enabled(mbutton)=FALSE # metadata button
    enabled(ebutton)=FALSE # MOVE button
    #enabled(xbutton)=FALSE # explorer button
    enabled(tab)=FALSE
    enabled(dbgbutton)=FALSE # DISMISS button
    enabled(rbb)=FALSE # rebuild button
    
    a1$stop_timer()
    if(isExtant(w))
      keep_above(w,FALSE)
    .GlobalEnv$onTop=FALSE
    svalue(rbt)=.GlobalEnv$onTop
    if(isExtant(w))
      keep_above(w,FALSE)
    .GlobalEnv$ofxx=idx
    gdx=which(gdframe$fnx==tmpx$filename)
    fdx=which(fnames$fnx==tmpx$filename)
    
    erry=tryCatch.W.E({.GlobalEnv$fwind=gdfd(tmpx)}) ########## GDFD CALLED HERE ##########
    .GlobalEnv$gdfopen=FALSE
    if(!isExtant(tab)){
      gtkMainQuit()
    }
    a1$start_timer()
    fxx=svalue(tab,index=TRUE) # selected index
    enabled(tab)=TRUE # rebuild button
    enabled(dbgbutton)=TRUE # DISMISS button
    #enabled(rbb)=TRUE
    #enabled(tbutton)=TRUE
    enabled(mbutton)=TRUE
    enabled(dbutton)=TRUE
    enabled(ebutton)=TRUE
    #enabled(xbutton)=TRUE
    comps=c(TRUE,TRUE,TRUE,TRUE,TRUE)
    if(!identical(tmpx,fwind)){
      comps=sapply(1:5, function(x) identical(tmpx[x],fwind[x]))
      fxx=ofxx
      if(!comps[1]|!comps[2]|!comps[3]|!comps[4]|!comps[5]){
        S1=''
        S2=''
        S3=''
        S4=''
        fwind=trim(fwind)
        if(len(fwind$Title))
          S1=paste('Title:',fwind$Title)
        if(len(fwind$Comment))
          S2=paste('Comment:',fwind$Comment)
        if(len(fwind$SubTitle))
          S3=paste('SubTitle:',fwind$SubTitle)
        if(grepl('RPDNClips',fwind$filename)){
          if(len(fwind$studio)){
            S4=paste('studio:',fwind$studio)}
        }
        cmts=paste(S1,S2,S3,S4)
        tab[fxx,'comments']=cmts
        if(!mwrit){
          if(!tab[fxx,'sell']=='Batch'){
            tab[fxx,'sell']='Batch'
            nrb=nrow(.GlobalEnv$Batch)+1
            ofmtime=as.character(file.mtime(tmpx$filename))
            .GlobalEnv$Batch[nrb,'ofn']=.GlobalEnv$svt
            .GlobalEnv$Batch[nrb,'ofmtime']=ofmtime
            .GlobalEnv$Batch[nrb,'dfix']=which(svt==dfan$filename)
          }else{
            nrb=nrow(.GlobalEnv$Batch)
          }
          .GlobalEnv$Batch[nrb,1:5]=fwind[,1:5]
          .GlobalEnv$batchCnt=nrb
          svalue(stater)=paste(nrb,'recs batched')
          
          .GlobalEnv$batchFlag=TRUE
          enabled(ebutton)=FALSE # MOVE button
          enabled(clbp)=TRUE # PROGRAM button
          enabled(clbb)=TRUE # CLEAR BATCH button
          save(Batch,batchCnt,file='~/Batch.RData')
        }
        
        enabled(ebutton)=FALSE # MOVE button
        .GlobalEnv$fnames[idx,'fnx']=fwind$filename
        tab[fxx,'fnx']=fwind$filename
        .GlobalEnv$gdframe[gdx,'fnx']=fwind$filename
        .GlobalEnv$gdframe[gdx,'comments']=cmts
        .GlobalEnv$fnames[fdx,'fnx']=fwind$filename
        .GlobalEnv$fnames[fdx,'comments']=cmts          
        save(dfan,gdframe,idxs,file='~/gdframe.RData')
        
      }
    }else{
      if(!isExtant(tab)){
        gtkMainQuit()
      }else{
        return() # changes discarded in gdfd
      }
    }
    if(!.GlobalEnv$batchFlag &!mwrit){
      .GlobalEnv$Passt=TRUE
      gtkMainQuit()
    }
  }
  if(isExtant(.GlobalEnv$eww))
    dispose(.GlobalEnv$eww)
  
  addHandlerSelectionChanged(tab, handler = function(h,...) {
    if(!.GlobalEnv$SelectChanged){
      .GlobalEnv$gdfopen=FALSE # unblock edit
      .GlobalEnv$sctimer=gtimer(25,selchghnld,one.shot = TRUE)
      .GlobalEnv$SelectChanged=TRUE
    }else{
      .GlobalEnv$SelectChanged=FALSE
    }
  })
  
  moveHandler=function(h,...) {
    fxx=which(tab[,'fnx']==fnx)
    if(tab[fxx,'sell']=='Batch'){
      galert('Cannot Move, Already Batched')
    }else{
      if(isExtant(.GlobalEnv$eww))
        dispose(.GlobalEnv$eww)
      dircoach=strsplit(basename(fnx),'-')[[1]][[1]]
      xx=dir('D:/PNMTALL',full.names = TRUE)
      xx=paste(toupper(substr(xx,1,1)),substr(xx,2,nchar(xx)),sep='')
      xx=xx[order(basename(xx))]
      preselect=xx[which(grepl(toupper(dircoach),toupper(xx),fixed = TRUE))]
      if(len(preselect)!=1){
        preselect=NULL
      }
      drr=select.list(xx,graphics=TRUE,preselect = preselect)
      if(nchar(drr)){
        galert('MOVING FILE')
        Sys.sleep(.5)
        .GlobalEnv$svt=normalizePath(fnx,winslash = '/')
        idx=which(.GlobalEnv$fnames$fnx==.GlobalEnv$svt)
        .GlobalEnv$mtme=file.mtime(.GlobalEnv$fnames[idx,'fnx'])
        .GlobalEnv$Passt=TRUE
        tmpdf=dfan[grepl(toupper(trim(.GlobalEnv$fnames[idx,'fnx'])),
                         toupper(dfan[,'filename']),fixed=TRUE),]
        if(!is.na(tmpdf$DMComment))
          tmpdf$Comment=tmpdf$DMComment
        tmpx=tmpdf[,1:4]
        tmpx$filename=paste(drr,'/',basename(fnx),sep='')
        .GlobalEnv$fwind=tmpx
        ofmtime=as.character(file.mtime(svt))
        if(file.rename(svt,tmpx$filename)){
          fxx=which(tab[,'fnx']==svt)
          galert('File Move Successful')
          tab[fxx,'fnx']=tmpx$filename
          .GlobalEnv$fnames[idx,'fnx']=tmpx$filename
          print(paste('movedfnamesd idx=',idx))
          #### replace dfan$filename with newly moved filename (to allow batch)
          .GlobalEnv$dfanxT=which(grepl(normalizePath(svt,winslash='/'),dfan$filename,fixed=TRUE))
          .GlobalEnv$dfan[.GlobalEnv$dfanxT,'filename']=normalizePath(tmpx$filename,winslash ='/')
          print(dfan[.GlobalEnv$dfanxT,'filename'])
          .GlobalEnv$moved=TRUE
        }else{
          galert('File Rename Failed')
        }
      }
    }
  }
  
  trimHandler= function(h,...) {
    keep_above(w,FALSE)
    .GlobalEnv$onTop=FALSE
    if(!file.exists(fnx))
      return()
    .GlobalEnv$cancelFlag=FALSE
    .GlobalEnv$svt=normalizePath(fnx,winslash = '/')
    print(paste('Attempting trim of',.GlobalEnv$svt))
    idx=which(.GlobalEnv$fnames$fnx==.GlobalEnv$svt)
    .GlobalEnv$mtme=file.mtime(.GlobalEnv$fnames[idx,'fnx'])
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$changed=FALSE
    fxx=which(tab[,'fnx']==fnx)
    if(len(fxx)==0)
    {
      galert('fxx unavailable NULL')
      return()
    }
    print(paste('fxx=',fxx))
    if(tab[fxx,'sell']=='Batch'){
      galert('Cannot TRIM, Already Batched')
    }else{
      tmpdf=dfan[grepl(toupper(trim(.GlobalEnv$fnames[idx,'fnx'])),
                       toupper(dfan[,'filename']),fixed=TRUE),]
      if(!is.na(tmpdf$DMComment))
        tmpdf$Comment=tmpdf$DMComment
      tmpx=tmpdf[,1:4]
      tmpx$filename=fnx
      .GlobalEnv$fwind=tmpx
      ofmtime=as.character(file.mtime(svt))
      .GlobalEnv$Fdate=FALSE
      print('TRIMMING')
      if(isExtant(.GlobalEnv$eww))
        dispose(.GlobalEnv$eww)
      .GlobalEnv$svt=normalizePath(fnx,winslash = '/')
      if(h$action==1){
        StartMyGUI()
      }else{
        if(h$action==2){
          oldext=file_ext(svt)
          oldmt=file.mtime(svt)
          svtk=sub(oldext,'mkv',svt)
          file.rename(svt,svtk)
          shell(svtk) # call photos.exe
          restt=file.copy(svtk,svt)
          if(restt){
            print('File copied')
            WriteDate(svt,oldmt)
            sx=''
            while (!any(grepl("No matching handles found.",sx))) {
              Sys.sleep(2)
              sx=shell('handle -p Microsoft.photos.exe',intern=TRUE)
            }
            file.remove(svtk)
            print(paste(svtk,'REMOVED'))
            .GlobalEnv$cancelFlag=TRUE # do not indicate TRIM failed
            .GlobalEnv$ss=NULL
          }
        }else{
          #action is 3
          .GlobalEnv$Fdate=TRUE
          source('~/pllist.git/dfxprocess.R')
        }
      }
      if(!is.null(.GlobalEnv$ss)){
        fxx=which(tab[,'fnx']==svt)
        galert('File TRIM Successful')
        if(!exists('svtO'))
          svtO=svt # new filename equals old file name
        newfn=normalizePath(svtO,winslash = '/') # new filename
        .GlobalEnv$fwind=newfn
        tab[fxx,'fnx']=newfn
        tab[fxx,'Size']=ptn(file.size(newfn))
        tab[fxx,'Date']=as.character(file.mtime(newfn))
        .GlobalEnv$fnames[idx,'fnx']=newfn
        .GlobalEnv$Passt=TRUE
        .GlobalEnv$trimmed=TRUE
        .GlobalEnv$rebuild=TRUE
        .GlobalEnv$liner=NULL
        .GlobalEnv$gxy='' #prevent searching for popup
        #### replace dfan$filename with newly trimmed filename (to allow rebatch)
        .GlobalEnv$dfanxT=which(grepl(normalizePath(svt,winslash='/',mustWork=FALSE),dfan$filename,fixed=TRUE))
        .GlobalEnv$dfan[.GlobalEnv$dfanxT,'filename']=normalizePath(svtO,winslash ='/')
        print(dfan[.GlobalEnv$dfanxT,'filename'])
      }else{
        if(.GlobalEnv$cancelFlag){
          galert('File TRIM Cancelled')
        }else{
          galert('File TRIM Failed')
        }
      }
    }
  }
  
  deleteHandler = function(h,...) {  
    fxx=which(tab[,'fnx'] %in% .GlobalEnv$fnx)
    print(paste('delete handler global fnx=',.GlobalEnv$fnx))
    if(any(tab[fxx,'sell']=='Batch')){
      galert('Cannot delete, Batched')
    }else{
      if(gconfirm('DELETE',icon='question')){
        if(isExtant(.GlobalEnv$eww))
          dispose(.GlobalEnv$eww)
        .GlobalEnv$svt=normalizePath(fnx,winslash = '/')
        for(jxy in 1:len(fnx)){
          shell(sprintf('nircmd moverecyclebin "%s"',fnx[jxy]),translate=TRUE)
        }
        galert('File(s) DELETED')
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$Passt=TRUE
        .GlobalEnv$changed=FALSE
        tab[fxx,'fnx']=''
        tab[fxx,'Date']=''
        tab[fxx,'comments']=''
        tab[fxx,'Size']=''
        tab[fxx,'sell']='DELETED'
      }
    }
  }
  
  selchghnld= function(h,...){
    if(!.GlobalEnv$SelectChanged)
      return()
    
    .GlobalEnv$fnx=getFnx()
    .GlobalEnv$svt=fnx
    lenn=len(.GlobalEnv$fnx)
    if(lenn==1){
      
      enabled(btns)=FALSE
      idx=which(.GlobalEnv$fnames$fnx==.GlobalEnv$fnx)
      if(len(idx)==0)
        return()
      if(len(idx)>1){
        print('IDX length exceeded 1')
        idx=idx[1]
      }
      print(paste('Global fnx found:',.GlobalEnv$fnx))
      tmpdf=dfan[grepl(toupper(normalizePath(trim(.GlobalEnv$fnames[idx,'fnx']),winslash = '/')),toupper(dfan[,'filename']),fixed=TRUE),]
      dfx=tmpdf[,1:4]
      if(nrow(dfx)){ # in case of renamed file no match
        for(x in 1:ncol(dfx)){
          if(is.na(dfx[,x]))
            dfx[,x]=''
        }
      }
      if(exists('.GlobalEnv$eww')){
        if(isExtant(.GlobalEnv$eww))
          dispose(.GlobalEnv$eww)
      }
      shell('nircmd win close title "Action"')
      if(blockg){
        print('blocked by blockg')
        .GlobalEnv$blockg=FALSE
        return()
      }
      
      .GlobalEnv$eww=gwindow('Action',width=30,height=400,visible=FALSE,parent = c(0,0))
      ew=.GlobalEnv$eww
      getToolkitWidget(.GlobalEnv$eww)$move(400,100)
      .GlobalEnv$ggex=ggroup(cont=.GlobalEnv$eww,horizontal = FALSE)
      .GlobalEnv$gpp=ggroup(cont=.GlobalEnv$ggex)
      
      if(isExtant(.GlobalEnv$gpp))
        ewb1=gbutton("DELETE",cont=.GlobalEnv$gpp,handler=deleteHandler)
      isHEVC=isHevc(fnx)
      if(is.na(isHEVC))
        isHEVC=FALSE
      if(!isHEVC){
        if(isExtant(.GlobalEnv$gpp))
          ewb2=gbutton("PHOTO",cont=.GlobalEnv$gpp,handler=trimHandler,action=2)
        if(isExtant(.GlobalEnv$gpp))
          ewb2a=gbutton("TRIM",cont=.GlobalEnv$gpp,handler=trimHandler,action=1)
      }
      
      if(isExtant(.GlobalEnv$gpp))
        ewb2x=gbutton("Fdate",cont=.GlobalEnv$gpp,handler=trimHandler,action=3)
      if(isExtant(.GlobalEnv$gpp)) 
        ewb3=gbutton("PLAY",cont=.GlobalEnv$gpp,handler=function(h,...) {
          dispose(ew)
          shell(fnx)
          g1=galert('waiting for player terminate',120)
          Sys.sleep(2)
          pss='pid'
          while(any(grepl('pid',pss))){
            pss=shell('handle -p vlc',intern = TRUE)
            pss=c(pss,shell('handle -p wmplayer',intern = TRUE))
          }
          
          if(isExtant(g1))
            dispose(g1)
          enabled(tab)=TRUE
          #enabled(tbutton)=TRUE
          enabled(mbutton)=TRUE
          enabled(dbutton)=TRUE
          enabled(ebutton)=TRUE
          #enabled(xbutton)=TRUE
          enabled(dbgbutton)=TRUE # DISMISS button
        })
      
      if(isExtant(.GlobalEnv$gpp))
        ewb4=gbutton("EXPLORE",cont=.GlobalEnv$gpp,handler=function(h,...) {
          dispose(ew)
          shell(paste(normalizePath('~/',winslash = '/'),'/hexDump/bin/explorerselect.bat "',fnx,'" ',',' ,sep=''),translate = TRUE, 
                intern = TRUE)
        })
      if(isExtant(.GlobalEnv$gpp))
        mvb=gbutton("MOVE",cont=.GlobalEnv$gpp,handler=moveHandler)
      
      gf(h)
      .GlobalEnv$SelectChanged=FALSE
      
    }else{
      enabled(btns)=TRUE # playALL button
      enabled(ebutton)=FALSE
      #enabled(tbutton)=FALSE
      enabled(mbutton)=FALSE
      #enabled(xbutton)=FALSE
    }
  }
  
  
  addHandlerDestroy(w, handler = function(h,...) {
    print('handler destroy w')
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  })
  
  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)
  
  stater=glabel('STATUS',cont=bg)
  
  lbl=glabel('*',cont=bg)
  
  rbt=gcheckbox('Tp', checked=.GlobalEnv$onTop,cont=bg)
  IDR=addHandlerChanged(rbt,handler=function(h,...){
    .GlobalEnv$onTop=!.GlobalEnv$onTop
    keep_above(w,.GlobalEnv$onTop)
    
  })
  
  clbp=gbutton("PROGRAM",cont=bg,handler=function(h,...){
    .GlobalEnv$blockr=TRUE #prevent spurious AND REGEXPR handler trigger
    enabled(btns)=FALSE
    enabled(clbp)=FALSE
    enabled(clbb)=FALSE # CLEAR BATCH button
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww) ############## MOVE and make allbuttonhandler ############
    .GlobalEnv$ORflag = TRUE
    .GlobalEnv$ANDflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$rebuild=TRUE
    .GlobalEnv$liner='.'
    svalue(stater)='PROGRAMMING'
    gtkMainQuit()
  })
  
  clbb=gbutton('Clr Batch',cont=bg,handler=function(h,...){
    .GlobalEnv$changed=FALSE
    svalue(stater)='Batch Cleared'
    for(v in 1:nrow(tab)){
      if(tab[v,'sell']=='Batch'){
        z=which(Batch$filename==tab[v,'fnx'])
        zz=which(Batch[z,'filename']==.GlobalEnv$fnames$fnx)
        .GlobalEnv$fnames[zz,'fnx']=Batch[z,'ofn']
        tab[v,'fnx']=Batch[z,'ofn']
        tab[v,'sell']=''
        if(z==nrow(Batch))
          break
      }
    }
    
    enabled(clbb)=FALSE
    enabled(clbp)=FALSE # PROGRAM button
    .GlobalEnv$Batch=.GlobalEnv$Batch[0,]
    .GlobalEnv$batchFlag=FALSE
  })
  
  ggxHandler=function(){
    keep_above(w,FALSE)
    svalue(rbt)=FALSE
    if(svalue(ggr)){
      .GlobalEnv$rchecked=TRUE
      if(svalue(ggp)){ # only rpdn
        .GlobalEnv$pchecked=TRUE
        rng=which(grepl('rpdnclips',paste(.GlobalEnv$fnames$fnx,.GlobalEnv$fnames$comments),ignore.case = TRUE))
      }else{
        rng=which(grepl('.',paste(.GlobalEnv$fnames$fnx,.GlobalEnv$fnames$comments),ignore.case = TRUE))
      }
      .GlobalEnv$rang=rng
    }else{
      rng=which(!grepl('RPDNClips',paste(.GlobalEnv$fnames$fnx,.GlobalEnv$fnames$comments),ignore.case = TRUE))
      .GlobalEnv$rchecked=FALSE
      .GlobalEnv$rang=rng
    }
    enabled(ggp)=svalue(ggr)
    .GlobalEnv$rngr=rng[rng %in% rang] # if rexExp filter in use rang contains the subset
    visible(gp)=FALSE
    galert('Please wait.....')
    Sys.sleep(1)
    tryCatch.W.E({
      tab[,]=.GlobalEnv$fnames[.GlobalEnv$rngr,]
      visible(gp)=TRUE
      svalue(w)=paste(len(rng),'files',chula)
      if(len(rang)!=nrow(.GlobalEnv$fnames))
        svalue(w)=paste('REGEXP FILTER',.GlobalEnv$regexfilt,len(rng),'files',chula)
    })
  }
  
  ggp=gcheckbox('Only',cont=bg, checked=pchecked, handler=function(h,...){
    ggxHandler()
  })
  
  ggr=gcheckbox('RPDNC',cont=bg, checked=rchecked,handler = function(h,...) {
    ggxHandler()
  })
  
  rebuildHandler = function(){  
    .GlobalEnv$nxflag=TRUE
    .GlobalEnv$rebuild=TRUE
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$EPasst=TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$liner=NULL
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    dispose(w)
    .GlobalEnv$gdfopen=FALSE
    gtkMainQuit()
  }
  
  rbb=gbutton("REBUILD", container=bg, handler = function(h,...) {
    rebuildHandler()
  })
  
  MLButton=gbutton("MakeLast", container = bg, handler = function(h,...) {
    gxx=galert('MAKELAST - WORKING',delay=1000)
    enabled(rbb) = FALSE   # rebuild button
    enabled(MLButton) = FALSE
    keep_above(w,FALSE)
    shell('"c:/Program Files/R/R-3.5.0/bin/rscript.exe" c:/Users/Larry/Documents/Pllist.git/makelast.R testplots',translate = TRUE)
    shell('nircmd.exe win close class "CabinetWClass"')
    tryCatch.W.E(dispose(gxx))
    rebuildHandler()
  })
  
  BKButton=gbutton("Backk", container = bg, handler = function(h,...) {
    gxx=galert('Backing up - WORKING',delay=1000)
    enabled(rbb) = FALSE   # rebuild button
    enabled(MLButton) = FALSE
    enabled(BKButton) = TRUE
    shell('"c:/Program Files/R/R-4.0.2/bin/rscript.exe" c:/Users/Larry/Documents/Pllist.git/Backk.R',translate = TRUE)
    shell('nircmd.exe win close class "CabinetWClass"')
    if(exists('dbutton'))
      if(isExtant(dbutton)){
        enabled(rbb) = TRUE   # rebuild button
        enabled(BKButton) = TRUE
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
  
  ALLButton=gbutton("ALL", container = bg, handler = function(h,...) {
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    .GlobalEnv$ORflag = TRUE
    .GlobalEnv$ANDflag = FALSE
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$srchF=TRUE
    .GlobalEnv$liner='.'
    svalue(xw$ge)=''
    enabled(btns)=FALSE
    gtkMainQuit()
  })
  font(ALLButton) <- c(color="green", weight="bold") # initial 
  
  xe=gedit(container=bg, initial.msg='Enter Search Criteria', handler = function(h,...) {
    print('xe handler - enter search crit')
    .GlobalEnv$avail = TRUE
    .GlobalEnv$Passt=TRUE
    .GlobalEnv$EPasst=TRUE
    .GlobalEnv$srchF=TRUE
    .GlobalEnv$rchecked=FALSE
    .GlobalEnv$pchecked=FALSE
    gtkMainQuit()
  })
  
  addHandlerKeystroke(xe, handler = function(h,...) {
    enabled(ANDButton) = FALSE #
    enabled(ORButton) = FALSE #
    if(nchar(svalue(h$obj))){
      enabled(ANDButton) = TRUE #
      enabled(ORButton) = TRUE #
      enabled(ALLButton) = TRUE
      .GlobalEnv$liner=svalue(h$obj)
    }
  })
  
  geHandler = function(h,...) {
    print('geHandler')
    if(nchar(svalue(h$obj))==0){
      print('gehandler returned h$obj is zero')
      return()
    }
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    if(!testRegex(svalue(h$obj))){
      galert('BAD REGEXP')
      return()
    }
    keep_above(w,FALSE)
    svalue(ggp)=pchecked
    enabled(btn)=TRUE
    .GlobalEnv$andexp='.'
    if(!blockr){
      .GlobalEnv$blockr=TRUE # to block clear regexp button press
      if(TESTER==9999){
        load('~/aexp.RData')
        idxx=as.numeric(svalue(h$obj))
        .GlobalEnv$andexp[1]=as.character(aexp[idxx,1])
        .GlobalEnv$andexp[2]=as.character(aexp[idxx,2])
        .GlobalEnv$andexp[3]=as.character(aexp[idxx,3])
        .GlobalEnv$andexp[4]=as.character(aexp[idxx,4])
        svh=as.character(aexp[idxx,5])        
        .GlobalEnv$TESTER=0
      }else{
        print('Calling getAndExp()')
        .GlobalEnv$TESTER=0
        .GlobalEnv$andexp=unlist(getAndExp())
        svh=svalue(h$obj)
      }
      if(!testRegex(andexp[1]) | !testRegex(andexp[2]) | !testRegex(andexp[3])){
        galert('BAD REGEXP')
        return()
      }
    }else{
      .GlobalEnv$blockr=FALSE
      print('blocked by blockr')
      return()
    }
    expp=paste(.GlobalEnv$fnames$fnx,.GlobalEnv$fnames$comments)
    expp=sub('PNMTALL','',expp)
    if(svalue(ggr) & svalue(ggp)){ # RPDN Clips only
      rng=which(grepl(svh,expp,ignore.case = TRUE) & 
                  grepl('RPDNClips',expp,ignore.case = TRUE) & 
                  ifelse(rep(as.logical(.GlobalEnv$andexp[4]),len(expp)),  
                         grepl(andexp[1],expp,ignore.case = TRUE),
                         !grepl(andexp[1],expp,ignore.case = TRUE))  & 
                  grepl(andexp[2],expp,ignore.case = TRUE)   &
                  grepl(andexp[3],expp,ignore.case = TRUE))
    }else{
      if(!svalue(ggr)){
        rng=which(grepl(svh,expp,ignore.case = TRUE) & !grepl('RPDNClips',expp,ignore.case = TRUE) & 
                    ifelse(rep(as.logical(.GlobalEnv$andexp[4]),len(expp)),  
                           grepl(andexp[1],expp,ignore.case = TRUE),
                           !grepl(andexp[1],expp,ignore.case = TRUE))  & 
                    grepl(andexp[2],expp,ignore.case = TRUE)   &
                    grepl(andexp[3],expp,ignore.case = TRUE))
      }else{
        rng=which(grepl(svh,expp,ignore.case = TRUE) &
                    ifelse(rep(as.logical(.GlobalEnv$andexp[4]),len(expp)), 
                           grepl(andexp[1],expp,ignore.case = TRUE),
                           !grepl(andexp[1],expp,ignore.case = TRUE))  & 
                    grepl(andexp[2],expp,ignore.case = TRUE)   &
                    grepl(andexp[3],expp,ignore.case = TRUE))       
      }
    }
    
    svalue(ge)=''
    if(!len(rng)){
      galert('NONE FOUND')
    }else{
      aexpadd=data.frame(andexp[1],andexp[2],andexp[3],andexp[4],svh)      
      if(!file.exists('~/aexp.RData')){
        aexp=aexpadd
      }else{
        load('~/aexp.RData')
        aexp=unique(rbind(aexp,aexpadd))
      }
      save(aexp,file='~/aexp.RData')
      tryCatch.W.E({
        visible(gp)=FALSE
        tab[,]=.GlobalEnv$fnames[rng,]
        fxxt=which(fnames[rng,'fnx'] %in% Batch$filename)
        if(len(fxxt))
          tab[fxxt,'sell'] = 'Batch'
        .GlobalEnv$rang=rng
        enabled(btns)=TRUE
        anders=''
        if(len(andexp)>1)
          anders = capture.output(cat(as.character(andexp[1:(len(andexp)-1)]),sep=' & '))
        svalue(w)=gsub('& . ','',paste('REGEXP FILTER ',svh,' & ',
                                       ifelse(tail(andexp,1),'','!'),anders,' ',len(rng),' files ',chula,sep=''),fixed=TRUE)
        .GlobalEnv$regexfilt=svh
        visible(gp)=TRUE
        focus(ge)=TRUE
        .GlobalEnv$blockr=FALSE
      })
    }
    gtkMain()
    print('gtkmain first exited')
    gtkMainQuit()
  }
  print('Hello')
  initMain <- function() {
    .GlobalEnv$ge=gedit(container=bg, initial.msg='Enter Search RegExp Filter', handler = geHandler)
    
    addHandlerKeystroke(ge, handler = function(h, ...) {
      #print('Keystroke Handler ge')
      if(h$key=='\033'){
        load('~/aexp.RData')
        if(nrow(aexp)==0){
          galert('None Found')
          return()
        }
        .GlobalEnv$aexp=aexp
        choices=''
        for(i in 1:nrow(aexp)){
          andexp[1]=as.character(aexp$andexp.1.[i])
          andexp[2]=as.character(aexp$andexp.2.[i])
          andexp[3]=as.character(aexp$andexp.3.[i])
          andexp[4]=as.character(aexp$andexp.4.[i])          
          svh=as.character(aexp$svh[i])
          anders = capture.output(cat(as.character(andexp[1:(len(andexp)-1)]),sep=' & '))
          choices[i]=paste('REGEXP FILTER: ',svh,' & ', ifelse(as.logical(andexp[4]),'','!'),anders,' ',sep='')
        }
        keep_above(w,FALSE)
        rvv=NULL
        while(is.null(rvv)){
          rvv=selList(choices)
        }
        idxx=NULL
        if(len(rvv)>0){
          svvd=rvv$saved
        }else{
          svvd=0
        }
        if(svvd==2){ #save EDIT
          idxx=which(!rvv$gt %in% choices)
          aexp[idxx,]=rvv$aexpadd
          save(aexp,file='~/aexp.RData')
        }else{
          if(svvd==1){ # save SAVE
            aexp=.GlobalEnv$aexp
            save(aexp,file='~/aexp.RData')
          }else{ #no SAVE
            idxx=which(rvv$rv==choices)
          }
        }

        print(aexp[idxx,])
        if(len(idxx)>0){
          .GlobalEnv$TESTER=9999
          andexp[1]=as.character(aexp$andexp.1.[idxx])
          andexp[2]=as.character(aexp$andexp.2.[idxx])
          andexp[3]=as.character(aexp$andexp.3.[idxx])
          andexp[4]=as.logical(as.character(aexp$andexp.4.[idxx]))
          svalue(h$obj)=as.character(idxx)
        }
      }
    })
    
    .GlobalEnv$btn=gbutton("Clr RegEx",cont=bg)
    enabled(btn)=FALSE
    list(
      run = function(partner) {
        IDD=addHandlerChanged(btn, handler = function(h, ...) {
          blockHandler(btn,IDD)
          if(isExtant(.GlobalEnv$eww))
            dispose(.GlobalEnv$eww)
          svalue(partner$ge) <- ''
          .GlobalEnv$regexfilt=''
          .GlobalEnv$andexp='.'
          enabled(btn)=FALSE
          svalue(w)=paste(linerd,nrow(fnames[rang,]),'files',chula)
          enabled(btns)=FALSE
          enabled(btn)=FALSE
          tryCatch.W.E({
            visible(gp)=FALSE
            tab[,]=.GlobalEnv$fnames
            fxxt=which(fnames[,'fnx'] %in% Batch$filename)
            if(len(fxxt))
              tab[fxxt,'sell'] = 'Batch'
            .GlobalEnv$rang=1:nrow(fnames)
            enabled(btns)=TRUE
            svalue(w)=paste(nrow(fnames),'files',chula)
            .GlobalEnv$regexfilt=svalue(h$obj)
            visible(gp)=TRUE
            focus(ge)=TRUE
            .GlobalEnv$blockr=FALSE #Allow ge edit
            
          })
          unblockHandler(btn,IDD)
          .GlobalEnv$SelectChanged=FALSE
        } )
        #visible(w) <- TRUE
      },
      ge = ge
    )
  }
  xw=initMain()
  xw$run(xw)
  btns=gbutton("PlayALL",cont=bg,handler = function(h, ...) {
    .GlobalEnv$SelectChanged=FALSE
    if(isExtant(.GlobalEnv$eww))
      dispose(.GlobalEnv$eww)
    fn=svalue(tab)
    if(len(fn)==0){
      writeLines(gsub('/','\\\\',fnames[rang,2]),'fns.m3u') # Write playlist
      fn=fnames[rang,2]
    }else{
      fn=getFnx()
      keep_above(w,FALSE)
    }
    writeLines(gsub('/','\\\\',fn),'fns.m3u') # Write playlist
    shell('"C:\\Program Files\\VideoLAN\\VLC\\vlc.exe " fns.m3u')
  })
  
  ebutton=gbutton("MOVE", container = bg, handler = moveHandler)
  
  dbutton=gbutton("Delete", container = bg, handler = deleteHandler)
  
  #tbutton=gbutton('TRIM', container = bg, handler = trimHandler,action=1) # action 1 indicates TRIM not PHOTO
  
  mbutton=gbutton("Metadata", container = bg, handler = function(h,...) {
    editMeta()
  })
  
  dbgbutton=gbutton("dismiss", container = bg, handler = function(h,...) {
    if(.GlobalEnv$moved | .GlobalEnv$trimmed){
      rebuildHandler()
    }else{
      .GlobalEnv$tpexist <- FALSE
      if(isExtant(.GlobalEnv$eww))
        dispose(.GlobalEnv$eww)
      dispose(w)
      .GlobalEnv$gdfopen=FALSE
      gtkMainQuit()
    }
  })
  
  enabled(dbutton)=FALSE # delete button
  #enabled(tbutton)=FALSE # TRIM button
  enabled(mbutton)=FALSE # metadata button
  enabled(ebutton)=FALSE # edit button  
  #enabled(xbutton)=FALSE # explore button 
  enabled(MLButton) = FALSE # MakeLast
  enabled(rbb) = FALSE
  enabled(ANDButton) = FALSE #
  enabled(ORButton) = FALSE #
  #enabled(rbb) = TRUE
  enabled(btns)=FALSE
  enabled(clbb)=FALSE
  enabled(clbp)=FALSE
  
}else{
  visible(gp)=FALSE
  tab[,]=fnames[rang,]
  visible(gp)=TRUE
  linerd=liner
  if(!ANDflag)
    linerd=gsub(' ','|',liner)
  svalue(w)=paste(linerd,nrow(fnames[rang,]),'files',chula)
  svalue(stater)=''
}
getToolkitWidget(w)$move(0,0)
visible(w)=TRUE
if(exists('gxy'))
  if(isExtant(gxy))
    dispose(gxy)
srchF=FALSE
focus(xe)=TRUE
focus(ge)=TRUE
focus(tab)=TRUE # refresh initial message
keep_above(w,TRUE)
.GlobalEnv$onTop=TRUE
FUN1 <- function(data) {
  if(STOPPED)
    return()
  .GlobalEnv$STOPPED=TRUE
  a1$stop_timer()
  tryCatch.W.E(
    {
      if(tpexist){
        if(svalue(.GlobalEnv$lbl)=='*'){
          svalue(.GlobalEnv$lbl)=''
        }else{
          svalue(.GlobalEnv$lbl)='*'
        }
      }
      xx=(dir('d:/pnmtall',recursive = TRUE))
      writeLines(xx,'~/xx.txt')
      mdd=(md5sum('C:\\Users\\Larry\\Documents\\xx.txt'))
      if(mdd!=mddold & mddold!=0){
        print(paste(Sys.time(),"changes Detected in PNMTALL"))
        enabled(rbb)=TRUE
      }
      .GlobalEnv$mddold=mdd
      
      dd=shell('dir D:\\PNMTALL\\NewDownloads /S/B/A',intern = TRUE)
      dd=dd[grepl('.mp4|.mov',dd) & !grepl('orig',dd) & grepl('TRIMRATE_Trim',dd)]
      rrxx=file.size(dd)
      pss=shell('handle -p vlc',intern = TRUE)
      pss=c(pss,shell('handle -p wmplayer',intern = TRUE))
      
      if(!any(grepl('pid',pss)) & .GlobalEnv$playState==1){
        .GlobalEnv$playState=0
        .GlobalEnv$blockg=FALSE
      }
      
      if(exists('MLButton')){
        Sys.sleep(.5)
        enabled(ALLButton)=!batchFlag
        #enabled(ebutton)=!batchFlag # MOVE button (toolbar)
        if(isExtant(MLButton)){
          tryCatch.W.E({enabled(MLButton)=(sum(rrxx)>129)})
          if(sum(rrxx)>129){
            shell('Handle -p Microsoft.Photos.exe > hh.txt')
            hh=readLines('hh.txt')
            yy=subset(hh,grepl('PNMTALL|.exe',hh))
            pph=which(grepl("Microsoft.Photos.exe",yy,fixed=TRUE))
            if(len(pph)){
              llp=strsplit(yy[pph+1],' ')
              ix=which(file.exists(llp[[1]][1:9]))
              print('Found File:')
              .GlobalEnv$FoundFile=llp[[1]][ix]
              print(llp[[1]][ix])
              if(len(.GlobalEnv$FoundFile)==0){
                shell('taskkill /F /IM microsoft.photos.exe')
                return()
              }
              dirn=dirname(fnx)
              direxp=tail(unlist(strsplit(dirn,'/')),1)
              cmdd=sprintf("shell('nircmd win close title %s')",direxp)
              print(cmdd)
              eval(parse(text=cmdd))
              .GlobalEnv$newClip=dd[which(max(file.mtime(dd))==file.mtime(dd))][1]
              .GlobalEnv$newClipMtime=file.mtime(.GlobalEnv$FoundFile)
              if(file.mtime(.GlobalEnv$newClip) !=.GlobalEnv$newClipMtime)
                WriteDate(.GlobalEnv$newClip,as.character(.GlobalEnv$newClipMtime))
              load('~/ConvertLog.RData')
              nl=cnvLog[1,]
              nl$svt=.GlobalEnv$FoundFile
              nl$nfn=.GlobalEnv$newClip
              nl$md5s=md5sum(.GlobalEnv$newClip)
              cnvLog=rbind(cnvLog,nl)
              cnvLog=cnvLog[!duplicated(cnvLog$md5s),]
              save(cnvLog,file='~/ConvertLog.RData')
            }
          }
        }
        #enabled(BKButton)=len(dir('E:/'))
      }
    }
  )
  .GlobalEnv$STOPPED=FALSE
  a1$start_timer()
}
a1 <- gtimer(250, FUN1)
focus(w)=TRUE
gtkMain()
print('gtkmain END exited')
a1$stop_timer()
svalue(rbt)=.GlobalEnv$onTop
if(isExtant(w)){
  keep_above(w,FALSE)
  svalue(rbt)=FALSE
}
if(srchF){
  srchF=FALSE
  lnr=liner
  svalue(xe)=lnr
  if(ORflag){
    lnr=gsub(' ',' | ',lnr)
  }else{
    lnr=gsub(' ',' & ',lnr)
  }
  save(liner,ORflag,ANDflag,file='~/liner.RData')
  gxy=galert(paste('Searching for',lnr),delay=1000)
  Sys.sleep(1)
}








