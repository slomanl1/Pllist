dircoach='RealPlayerDownloads'
xxd=c('C:/RealPlayerDownloads',dir('d:/pnmtall',full.names = TRUE))
xxd=xxd[order(basename(xxd))]
preselect=xxd[which(grepl(dircoach,xxd,ignore.case = TRUE))]
if(len(preselect)!=1){
  preselect=NULL
}
source('~/pllist.git/GDF.R')

harvestMeta=function(svtin) {
  cd('~/')
  svt=normalizePath(svtin,winslash = '/')
  print(svt)
  cmdd=paste('shell("mediainfo.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
  print(cmdd)
  eval(parse(text=cmdd))
  cmdd=paste('shell("exiftool',svt,' >>meta.txt",mustWork=NA,translate=TRUE)')
  print(cmdd)
  eval(parse(text=cmdd))
  .GlobalEnv$meta=readLines('meta.txt')
  unlink('meta.txt')
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
  gg=rbind(cmts,mg)
  gg=gg[!endsWith(gg$X2,'min'),]
  flds=c('Complete name','Title','Subtitle','Image Size','Movie name','File Modification Date/Time','Compressor ID','Duration','Comment','DM Comment','XMP Toolkit','GPS Latitude')
  hh=gg[trim(gg$X1) %in% flds,]
  jj=hh[0,]
  for(x in flds) {
    pp=which(trim(hh$X1) %in% x)
    if(len(pp)>0)
      jj=rbind(jj,hh[pp,][1,])
  }
  zm=list(jj,mg)
  return(zm)  
}

editMeta=function() {
  zm=harvestMeta(svt)
  jj=zm[[1]]
  mg=zm[[2]]
  jj$X2=gsub("'","",jj$X2) # remove single quote marks
  mg$X2=gsub("'","",mg$X2) # remove single quote marksSS
  jj$X1=sub('GPS Latitude','Obn',jj$X1)
  jj$X2=sub('deg 0 0.00" N','',jj$X2)
  if(substr(jj$X2[2],nchar(jj$X2[2]),nchar(jj$X2[2])+1)==','){
    jj$X2[2]=substr(jj$X2[2],1,nchar(jj$X2[2])-1) # remove trailing comma
  }

  .GlobalEnv$eww=gwindow('Action',width=30,height=400,visible=FALSE,parent = c(0,0))
  ew=.GlobalEnv$eww
  getToolkitWidget(.GlobalEnv$eww)$move(400,100)
  .GlobalEnv$ggex=ggroup(cont=.GlobalEnv$eww,horizontal = FALSE)
  .GlobalEnv$gpp=ggroup(cont=.GlobalEnv$ggex)
  
  wm <- gwindow(paste("Metadata-",svt),width=700,visible = FALSE,parent=c(100,200))
  gpm<- ggroup(horizontal=FALSE, container=wm)
  tabm <- gtable('', chosencol = 2, container=gpm, expand=TRUE, handler = function(h,...){
    print('tabm handler')
  })
  tabm[,]= jj # remove duration with no seconds
  .GlobalEnv$metadata = mg
  visible(wm) <- TRUE
  bgm <- ggroup(container=gpm)
  addSpring(bgm)
  
  glabel('Search',cont=bgm)
  
  rgx=gedit(' ',cont=bgm,handler=function(h,...){
    mdd=trim(paste(.GlobalEnv$metadata[,1],.GlobalEnv$metadata[,2]))
    rng=which(grepl(svalue(h$obj),mdd,ignore.case = TRUE))
    tabm[,]=.GlobalEnv$metadata[rng,]
  })
  
  pbtn=gbutton('Edit', container=bgm, handler = function(h,...) {
    enabled(pbtn)=FALSE
    filename=svt                                                                    # Filename
    Title=jj[which(trim(jj[,1])=='Title'),2];if(len(Title)==0) Title=NA             # Title
    Comment=jj[which(trim(jj[,1])=='DM Comment'),2];if(len(Comment)==0) Comment=NA  # Comment
    SubTitle=jj[which(trim(jj[,1])=='Subtitle'),2];if(len(SubTitle)==0) SubTitle=NA # Sub Title
    studio= jj[which(trim(jj[,1])=='XMP Toolkit'),2];if(len(studio)==0) studio=NA   # studio
    Obn   = jj[which(trim(jj[,1])=='Obn'),2];if(len(Obn)==0)   Obn= NA     # Obn
    tmpx=trim(data.frame(filename=filename,Title=Title,Comment=Comment,SubTitle=SubTitle,
                         studio=studio,Obn=Obn,stringsAsFactors = FALSE ))
    dfyy=gdfd(tmpx)
    print('gdfd returned:')
    print(str(dfyy))
    if(doneflag){
      ############## PROGRAM HERE ########## use wrStud ###### then pgm subtitle...
      wrStud(dfyy$filename,dfyy$studio,dfyy$Comment,dfyy$Title,dfyy$SubTitle)
      dispose(wm)
    }
    gtkMainQuit()
  })
  
  gbutton('dismiss', container=bgm, handler = function(h,...) {
    if(isExtant(eww))
      # dispose(eww)
    dispose(wm)
    gtkMainQuit()
  })  
  
  focus(rgx)=TRUE
  addHandlerDestroy(wm,function(h,...) {
    gtkMainQuit()
  })
  gtkMain()
  print('editmeta exited')
}

ALTGinput = function(x="Enter Start Time (secs) or (mm:ss)",allowEnter){
  .GlobalEnv$bOK=TRUE
  .GlobalEnv$ss=-1
  .GlobalEnv$ToEnd=FALSE
  ww=gwindow(height=30,width=1000,title=x,parent=c(100,200))
  ggp=ggroup(cont=ww)
  getToolkitWidget(ww)$move(0,100)
  obj <- gedit(container=ggp, handler=function(h,...) 
  { .GlobalEnv$ss=svalue(h$obj)
  dispose(ww)
  gtkMainQuit()
  })
  focus(obj)=TRUE
  
  if(allowEnter)
    .GlobalEnv$odir="C:\\RealPlayerDownloads"
  else
    .GlobalEnv$odir=dirname(svt)
  
  .GlobalEnv$xxd[which(grepl(odir,.GlobalEnv$xxd,ignore.case=TRUE))]=NA
  .GlobalEnv$xxd[1]=odir
  xxy=data.frame(outdir=odir)
  names(xxy)='Output Directory'
  
  .GlobalEnv$chosen=FALSE
  if(!.GlobalEnv$Fdate){
    gx=gtable(xxy, cont=ggp,chosen.col=1, handler=function(h,...){
      if(!.GlobalEnv$chosen){
        .GlobalEnv$chosen=TRUE
        odirnew=choose.dir()
        if(!is.na(odirnew)){
          svalue(gx)=odirnew
        }else{
          .GlobalEnv$chosen=FALSE
        }
      }
    })
  }
  idxx=which(grepl(odir,xxd,ignore.case=TRUE))
  if (exists('gx'))
    svalue(gx,index=TRUE)=idxx
  addHandlerKeystroke(obj, handler = function(h,...){
    if(nchar(svalue(h$obj))==10000){ ########################## TESTING to prevent premature exit on backspace or left arrow
      .GlobalEnv$ss=NULL
      dispose(ww)
      gtkMainQuit()
    }else{
      #print(svalue(h$obj))
      .GlobalEnv$ToEnd=FALSE
      .GlobalEnv$ss=svalue(h$obj)}
  })
  addHandlerDestroy(ww, handler = function(h,...) {
    if(!.GlobalEnv$bOK)
      .GlobalEnv$ss=NULL
    gtkMainQuit()})
  
  olabel='TO'
  if(.GlobalEnv$Fdate)
    olabel='OK'
  
  obutton=gbutton(olabel, container=ggp,handler=function(h,...)
  {
    .GlobalEnv$bOK=FALSE
    if(ss >= 0){
      .GlobalEnv$bOK=TRUE
      dispose(ww)
      gtkMainQuit()
    }else{
      focus(obj)=TRUE
    }
  })
  if(!.GlobalEnv$Fdate){
    tbutton=gbutton('ToEnd', container=ggp,handler=function(h,...)
    {
      .GlobalEnv$ToEnd=TRUE
      if(ss > 0 | allowEnter){
        dispose(ww)
        gtkMainQuit()
      }else{
        focus(obj)=TRUE
      }
    })
    
    if(!allowEnter)
      fbutton=gbutton('FDATE', container=ggp,handler=function(h,...)
      {
        .GlobalEnv$Fdate=TRUE
        dispose(ww)
        gtkMainQuit()
      })
    
    bhandler=function(h,...){
      .GlobalEnv$gg=c('H264','H265','CANCEL','F720P')[h$action]
      print(paste('gg=',gg))
      dispose(.GlobalEnv$wzz)
      dispose(ww)
      gtkMainQuit()
    }
    
    if(!allowEnter)
      Cbutton=gbutton('Convert', container=ggp,handler=function(h,...){
        .GlobalEnv$convert=TRUE
        wz=gwindow(height=40,width=120,parent=c(700,200))
        .GlobalEnv$wzz=wz
        ggpb=ggroup(cont=wz)
        b1=gbutton('Cancel',cont=ggpb,action=3,handler=bhandler)
        b2=gbutton('H265',  cont=ggpb,action=2,handler=bhandler)
        b3=gbutton('H264',  cont=ggpb,action=1,handler=bhandler)
        b4=gbutton('H264/720P',  cont=ggpb,action=4,handler=bhandler)
        
      })
  }
  if(!allowEnter)
    mdbutton=gbutton('Metadata', container=ggp,handler=function(h,...) 
    {
      editMeta()
      .GlobalEnv$Fmeta=TRUE # exit immediately after ww close
      dispose(ww)
      gtkMainQuit()
    })
  
  xbutton=gbutton('Cancel', container=ggp,handler=function(h,...) 
  {
    .GlobalEnv$ss=NULL
    dispose(ww)
    gtkMainQuit()
  })
  focus(ww)=TRUE
  gtkMain()
}

EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n",allowEnter=FALSE){
  .GlobalEnv$ss=NULL
  while(TRUE){
    ALTGinput(x,allowEnter)
    startt= .GlobalEnv$ss  
    if(len(startt)>0){
      xx=tryCatch.W.E(as.POSIXlt(startt))
      if(any(class(xx$value)=="POSIXlt"))
        .GlobalEnv$Fdate=TRUE
      if(.GlobalEnv$Fdate){
        xx=tryCatch.W.E(as.POSIXlt(startt))
        if(!grepl('error',xx$value,ignore.case = TRUE)){
          .GlobalEnv$BypassE=TRUE
          .GlobalEnv$ss=as.character(xx$value)
          break # good date
        }
        print('BAD DATE')
        .GlobalEnv$Fdate=FALSE
      }
      if(!is.na(as.integer(startt))){
        break # good integer
      }else{
        cpos=regexpr(':',startt)
        if(cpos>0){
          f1=as.integer(substr(startt,1,cpos-1))
          f2=as.integer(substr(startt,cpos+1,nchar(startt)))
          if(!(is.na(f1)| is.na(f2))){
            if (f1>=0 & f1<60 & f2>=0 & f2<60){
              break # good mm:ss
            }
          }
        }else{
          if(allowEnter){
            startt=10000
            break;
          }
          
        }
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}

galert=function(msg='',delay=3,x=0,y=50,onTop=TRUE)
{
  if(len(msg)==0)
    msg=''
  vvv=gwindow(height = 50)
  if(onTop)
    keep_above(vvv,TRUE)
  getToolkitWidget(vvv)$move(x,y)
  addHandlerDestroy(vvv,handler=function(h,...) {aga$stop_timer()})
  g <- gvbox(cont=vvv)
  if(nchar(msg)>25){
    gtext(msg,cont=g,font.attr = list(size=21))
    size(vvv)[1]=size(vvv)[1]*(as.integer(nchar(msg))/25)
  }else{
    gtext(msg,cont=g,font.attr = list(size=21))
  }
  FUNz=function(data) {
    if(isExtant(vvv))
      dispose(vvv)
  }
  aga <- gtimer(delay*1000,one.shot=TRUE,FUNz)
  return(vvv)
}

metadata = function(filename) {
  xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                 filename,'" ' ,sep=''),translate = TRUE, intern = TRUE)
  fmt=strsplit(subset(xx,grepl('Format  ',xx))[2],';')[[1]][2]
  durX=strsplit(subset(xx,grepl('Duration  ',xx))[1],':')[[1]][2]
  meta=list(ddd=xx,dur=durX,format=fmt,hevcFlag=any(grepl('HEVC',xx)))
  return(meta)
}
