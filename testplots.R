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
                  #print(paste('hdl isunsorted=',.GlobalEnv$unsorted))
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
      #print('tab destroyed handler')
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
    .GlobalEnv$svt=normalizePath(getFnx(),winslash = '/')
    startt=NULL
    print(paste('svt=',.GlobalEnv$svt))
    StartMyGUI()
#     startt=EnterStartStop()
#     
#     print(len(startt))
#     if(len(startt)>0){
#       endtt=EnterStartStop("Enter End Time (secs) or (mm:ss)\n")
#       unlink('~/temppt.mp4')
#       file.rename(svt,'~/temppt.mp4')
#       svtt1=gsub(' ','',svt) # remove spaces for ffmpeg (does not accept " in filename's)
#       svtt='c:/RealPlayerDownloads/trimmed.mp4'
#       entf=FALSE
#       if(len(endtt)==0){
#         entf=TRUE
#         endtt=10000
#       }
#       cmdd=paste('shell("ffmpeg.exe -ss',startt,' -i c:/users/Larry/Documents/temppt.mp4 -t',endtt,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
#       print(cmdd)
#       eval(parse(text=cmdd))
#       if(entf){
#         file.rename(svtt,svt) # replace svt has trimmed with start to end
#       }else{
#         file.rename('~/temppt.mp4',svt)  # put back original, svtt has trimmed
#       }
#     }
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
    cmdd=paste('shell("mediainfo.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    .GlobalEnv$meta=readLines('meta.txt')
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
    .GlobalEnv$avail = TRUE
    .GlobalEnv$ofnx=NULL
    dispose(w)
    dispose(fwind)
    .GlobalEnv$gdfopen=FALSE
  }
  )
}else
  visible(w)=TRUE



getFnx = function() return(tab[svalue(tab,index=TRUE),'fnx'])
