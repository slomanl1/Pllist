idxs=NULL
for(x in 1:nrow(fnames)) 
  idxs=c(idxs,which(grepl(fnames[x,'fnx'],an[ttl],fixed=TRUE)))

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
                  .GlobalEnv$unsorted=is.unsorted(tab[,'cdts'])
                  print(paste('hdl isunsorted=',.GlobalEnv$unsorted))
                  .GlobalEnv$ssv = getFnx()
                  .GlobalEnv$avail = TRUE
                }
  )
  addHandlerRightclick(
    tab, handler = function(h,...) {
      if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
        .GlobalEnv$idx=svalue(h$obj,index = TRUE)
        print(paste('RC Handler idx=',idx))
        .GlobalEnv$ofnx=fnames[idx,]
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
      .GlobalEnv$avail = TRUE
      .GlobalEnv$tpexist <- FALSE
      print('tab destroyed handler')
      if(isExtant(fwind))
        dispose(fwind) # gdf window
      .GlobalEnv$gdfopen=FALSE
      if(isExtant(wm))
        visible(wm) <- FALSE
    }
  )

  bg <- ggroup(container = gp)
  .GlobalEnv$tab <- tab
  addSpring(bg)
  
  dbutton=gbutton("Delete", container = bg, handler = function(h,...) {
    answ=gconfirm('Are you Sure?')
    if(answ){
      print(paste('Deleting',getFnx()))
      .GlobalEnv$idx=svalue(tab,index=TRUE)
      print(paste('idx=',.GlobalEnv$idx))
      if(unlink(getFnx()))
        print('delete FAILED')
      else{
        .GlobalEnv$deleted=TRUE
        .GlobalEnv$avail=TRUE
        .GlobalEnv$Passt=TRUE
        visible(w) <- FALSE
        tpexist=FALSE
        writeLines(getFnx(),'file.tmp')
        file.append('deletelog.txt','file.tmp') # update delete log
        unlink('file.tmp')
      }
    }
  }
  )
  tbutton=gbutton("TRIM", container = bg, handler = function(h,...) {
    svt=normalizePath(getFnx(),winslash = '/')
    startt=NULL
    print(svt)
    startt=EnterStartStop()
    endtt=EnterStartStop("Enter End Time (secs) or (mm:ss)\n")
    print(len(startt))
    if(len(startt)>0){
      unlink('~/temppt.mp4')
      file.rename(svt,'~/temppt.mp4')
      svtt=gsub(' ','',svt) # remove spaces for ffmpeg (does not accept " in filename's)
      cmdd=paste('shell("ffmpeg.exe -ss',startt,' -i c:/users/LarrySloman/Documents/temppt.mp4 -t',endtt,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
      print(cmdd)
      eval(parse(text=cmdd))
      file.rename(svtt,svt)
    }
  }
  )
  
  wm <- gwindow("Metadata",width=400)
  visible(wm) <- FALSE
  gpm<- ggroup(horizontal=FALSE, container=wm)
  tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE,
                 handler = NULL)
  bgm <- ggroup(container=gpm)
  addSpring(bgm)
  addHandlerDestroy(
    tabm, handler = function(h,...) {
      print('destroyed tabm');
      if(exists('w')) 
        if(isExtant(w)) 
          dispose(w)
      .GlobalEnv$avail = TRUE})
  gbutton("dismiss", container=bgm, handler = function(h,...) {visible(wm) <- FALSE;    if(exists('w')) if(isExtant(w)) enabled(w) <- TRUE})
  
  mbutton=gbutton("Metadata", container = bg, handler = function(h,...) {
    enabled(w) <- FALSE
    svt=normalizePath(getFnx(),winslash = '/')
    print(svt)
    cmdd=paste('shell("exiftool.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    .GlobalEnv$meta=readLines('meta.txt')
    visible(wm) <- TRUE
    tabm[,]=meta
    
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

EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n"){
  while(TRUE){
    startt <- dlgInput(x)$res;
    if(len(startt)>0){
      if(!is.na(as.integer(startt))){
        break # good integer
      }else{
        cpos=regexpr(':',startt)
        if(cpos>0){
          f1=as.integer(substr(startt,1,cpos-1))
          f2=as.integer(substr(startt,cpos+1,nchar(startt)))
          if (f1>=0 & f1<60 & f2>=0 & f2<60){
            break # good mm:ss
          }
        }
        
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}

getFnx = function() return(fnames[svalue(tab,index=TRUE),'fnx'])
