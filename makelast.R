source("~/Local.R")
source('~/.RProfile') # required for standalone version
len=function(x) length(x)
library(tools)
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/rmmovname.R')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
################################
debugg=FALSE
##############################

args <- commandArgs(TRUE)
save(args,file='~/args.RData')
print(paste('ARGS',args))

if(debugg){
  cd('D:/PNMTALL/NewDownloads')
  args=file.choose()
}
if(len(args)>0){
  if(args=='testplots'){
    args=dir('d:\\pnmtall\\newdownloads','*TRIMRATE_Trim',full.names = TRUE)
    print(paste('testplot ARGS',args))    
  }
  if(!is.na(args) & file.exists(args) & (grepl('TRIMRATE_Trim',args,fixed = TRUE) |
                                         grepl('RATE_cutTrim.mp4',args,fixed = TRUE))){
    cd('D:/PNMTALL/NewDownloads')
    svt=normalizePath(args[1])
    fn=basename(sub('TRIMRATE_Trim.mp4','',svt))
    fn=basename(sub('RATE_cutTrim.mp4','',fn))
    fn=basename(sub('Gay69biz','',fn))
    ttl=strsplit(fn,'-|1')[[1]][2]
    pos=c(unlist(gregexpr('[A-Z]',ttl)),1000)
    ww=''
    for(z in 1:(len(pos)-1)){
      ww[z]=substr(ttl,pos[z],pos[z+1]-1)
      print(ww[z])
    }
    title=capture.output(cat(ww))
    zz=getMetadata(svt,FALSE)
    pfx=''
    if(nchar(zz$title[1])==0){
      psa=gregexpr('blah|ah',ttl)
      psah=tail(unlist(psa),1)
      pfx=substr(ttl,psah,1000)
      title=sub(pfx,'',title)
      vv=gsub('[A-z]','',fn)
      yoms=beaner(sub('-','',sub('\\+','',vv)))
    }else{
      lastw=tail(unlist(strsplit(zz$title[1],' ')),1) # last word of title
      title=sub(lastw,paste(lastw,''),zz$title[1])
      vv=gsub('[A-z]','',fn)
      yoms=beaner(sub('-','',sub('\\+','',vv)))
      pfx=''
      if(grepl(lastw,fn)){
        fn=sub(lastw,paste(lastw,''),fn) # space insert before nnYoms
        pfx=unlist(strsplit(fn,' |1'))[2]
      }
    }
    
    fn=sub(pfx,'',fn)    
    fx=unlist(strsplit(fn,paste(yoms,'|Yoms',sep='')))
    if(is.na(fx[3])){
      galert('MALFORMED FILENAME OR NO YOMS, ABORTING')
      return()
    }
    if(substr(fx[3],1,1)=='+'){
      fn=paste(fx[1],yoms,'Yoms+',substr(fx[3],2,nchar(fx[3])))
    }else{
      fn=paste(fx[1],yoms,'Yoms',fx[3])
    }
    if(substr(fx[3],1,1)=='-'){
      fn=paste(fx[1],yoms,'Yoms-',substr(fx[3],2,nchar(fx[3])))
    }else{
      if(!grepl('+',fn))
        fn=paste(fx[1],yoms,'Yoms',fx[3])
    }    
    fn=sub('CUHB','CUHB ',fn)
    fn=sub('CUHB YB','CUHBYB ',fn)
    gww=gwindow(height=30,width=600)
    zz=gedit(fn,cont=gww,handler=function(h,...){
      .GlobalEnv$fn=strsplit(svalue(zz),' ')[[1]][1]
      if(file.exists(paste('c:/RealPlayerDownloads/',fn,pfx,'.mp4',sep=''))){
        print('file already exists in c:/RealPlayerDownloads')
      }else{
        cmm=unlist(strsplit(svalue(zz),' '))
        cmx=cmm[2:len(cmm)]
        cmt=capture.output(cat(cmx))
        print(paste('cmx',cmx,'len of cmx',len(cmx),'svt=',svt))
        if(len(cmx)==2 & !any(grepl('Yoms',cmx)))
          cmt='Added to RPDNClips by makelast'
        cmt=sub('Top',' Top ',cmt)
        cmt=sub('Bot',' Bot ',cmt)
        bts=unlist(gregexpr('Bot|Top',cmt))
        if(len(bts)>0){
          poss=unlist(gregexpr('[A-Z]',cmt))
          cmt=paste(substr(cmt,1,poss[3]-1),substr(cmt,poss[3],nchar(cmt)))
        }
        if(len(bts)>1){
          poss=unlist(gregexpr('[A-Z]',cmt))        
          cmt=paste(substr(cmt,1,poss[len(poss)-2]-1),substr(cmt,poss[len(poss)-2],1000))
        }
        
        xm=strsplit(fn,'-')
        zm=xm[[1]][2]
        pos=gregexpr(zm,title)[[1]][1]
        ttll=title
        if(pos>1){
          ttll=substr(title,1,pos-1)
        }
        cmt=sub('C UHB','CUHB',cmt)
        wrStud(svt,Title=ttll,studio = NA,dmComment = cmt)
        file.copy(svt,paste('c:/RealPlayerDownloads/',fn,pfx,'.mp4',sep=''))
        cd('c:/RealPlayerDownloads/')
        file.remove(svt)
        gtkMainQuit()
      }
      dispose(zz)
    })
    addHandlerDestroy(gww,handler=function(h,...){
      gtkMainQuit()
    })
    if(file.exists(fn))
      gtkMainQuit()
    
    gtkMain()
  }
}
getTitle = function(filename){
  cc=NULL
  ss='shell("mediainfo %s | findstr /I name > mediainfo.txt")'
  xx=filename
  cmds=sprintf(ss,normalizePath(xx,winslash = '/'))
  eval(parse(text=cmds))
  tt=readLines('mediainfo.txt')
  tx=data.frame(tt,ff=as.numeric(as.factor(substr(tt,1,15))))
  if(nrow(tx)>0){
    tx$mn=''
    mvnm=tx[which(grepl('Movie name',tt)),'ff'][1]
    if(!is.na(mvnm)){
      for(i in 1:nrow(tx)){if(tx[i,'ff']==mvnm) tx[i-1,'mn']=as.character(tx[i,'tt'])}
      cc=subset(tx,nchar(mn)>0)[,c('tt','mn')]
      cc$tt=sub('Complete name                            : ','',cc$tt)
      cc$mn=sub('Movie name                               : ','',cc$mn)
      movienames=cc
      names(cc)=c('filename','MovieName')
    }
  }
  unlink('mediainfo.txt')
  return(cc)
}


orggs=dir('D:/PNMTALL/RPDNClips',pattern='*_original',full.names = TRUE)
orggs=c(orggs,dir('c:/RealPlayerDownloads',pattern='*_original',full.names = TRUE))
if(len(orggs))
  file.remove(orggs)

setwd('~/')

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
  source(scriptStatsRemoveAll) #clear bones
  source("~/Local.R")
  load('~/dirtbl.rdata')
  dirtbl=subset(dirtbl,nchar(as.character(dirtbl$Var1))>1)
  dirtbl=subset(dirtbl,!grepl(' ',as.character(dirtbl$Var1)))
  fn=NULL
  shell('dir "D:\\PNMTALL\\RPDNClips" /od/b > ddd.txt')
  lns1=readLines('ddd.txt')
  unlink('ddd.txt')
  lns=lns1[file_ext(lns1)=='wmv'|file_ext(lns1)=='asf']
  bn=beaner(lns)
  bx=sort(bn)
  last=tail(bx[which(bx<=99999)],1)
  
  setwd('C:\\RealPlayerDownloads')
  fns1=dir(pattern='*.mp4|*.mov|*.MP4')
  fns1=fns1[!(grepl('.mp4',fns1,ignore.case = TRUE)&(grepl('.mov',fns1)))]
  exts1=c('ah.mp4','blah.mp4','bfa.mp4','ussf.mp4','dd.mp4','bl.mp4','cs.mp4',
          'ussfd.mp4','pn.mp4','blfd.mp4','utpfd.mp4','ussfa.mp4','uwa.mp4',
          'vva.mp4','utp.mp4','sbd.mp4','su.mp4','so.mp4','dg.mp4')
  exts=gsub('.mp4','',exts1,fixed=TRUE)
  odr=sample(last:last+length(fns1),length(fns1),replace=T) 
  if(length(odr)!=0){
    load('~/mfnfo.RData')
    setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
    ex=file.exists(as.character(mfnfo$lsst))
    mfnfo=mfnfo[ex,] #remove non existent files from fnfo
    fns=fns1[order(odr)]
    fn=fns
    setwd('C:\\RealPlayerDownloads')
    last=last+1
    dfx=data.frame(lsst='',studio='',fn='',Title='',stringsAsFactors = FALSE)
    dfx=dfx[0,]
    for (i in 1:length(fns)){
      cc=capture.output(cat(sort(exts[unlist(sapply(exts,function(x) gregexpr(x,fns[i])))>0]),sep=''))
      for(x in 1:len(exts))
        cc=sub(paste(exts[x],exts[x],sep=''),exts[x],cc)
      exist=TRUE
      ren=ifelse(any(grepl('_REN',mfnfo$lsst)),'_REN','')
      while (exist){
        fn[i]=paste(as.character(last),cc,ren,'.wmv',sep='')
        print(paste('Processing',fns[i]))
        fs=file.info(fns[i])
        if(isLocked(fns[i]))
        {
          print('FIle LOCKED, SKIPPED')
          exist=FALSE
          next
        }
        if(fs$size %in% mfnfo$size &
           identical(md5sum(fns[i]), md5sum(fns[fs$size %in% mfnfo$size]))){
          exist=FALSE
          print(c(fns[i],'Dup file found'))
          next
        }else{
          last=last+1
          std=which(sapply(paste('\\<',dirtbl$Var1,sep=''),function(x)grepl(x,fns[i],ignore.case = TRUE)))
          studio=''
          if(len(std)>0){
            studio=dirtbl[std,'Var1']
            fn[i]=sub('.wmv',paste('_',studio,'.wmv',sep=''),fn[i])
          }else{
            studio=''
            if(interactive())
              studio=select.list(sort(as.character(dirtbl$Var1)),graphics =TRUE,title=fn[i])
            if(nchar(studio)<2)
              studio=''
            fn[i]=sub('.wmv',paste('_',studio,'.wmv',sep=''),fn[i])
          }
          fntt=cleanwplsfn(fn[i])
          fn[i]=fntt$fname
          if(file.exists(paste(drive,"PNMTALL\\RPDNClips\\",fn[i],sep=''))){
            print(paste(fn[i],'exists'))
            next
          }else{
            title=getTitle(fns[i])$MovieName
            if(is.null(title))
              title=NA
            title=gsub("'",'',title)
            dfxa=data.frame(lsst=fns[i],studio=studio,fn=fn[i],Title=title,Obn=beaner(fn[i]))
            dfx=rbind(dfx,dfxa)
            print(paste(fn[i], 'added to RPDNClips write queue'))
            exist=FALSE
          }
        }
      }
      
    }
    dmc='Added to RPDNClips by makelast'
    dfx$DMcomment=NA
    wrStud(dfx$lsst,dfx$studio,Title=dfx$Title,dfx$DMcomment,Obn=dfx$Obn)
    for(x in 1:nrow(dfx)) {
      file.copy(as.character(dfx$lsst)[x],
                normalizePath(as.character(paste(drive,"PNMTALL/RPDNClips/",dfx$fn,sep='')),'/',FALSE)[x],
                copy.date=TRUE,overwrite = TRUE)
      catt(paste(dfx[x,'lsst'], 'Copied to',dfx[x,'fn']))
    }
    
    #file.copy(as.character(dfx$lsst),normalizePath(as.character(paste(drive,"PNMTALL/RPDNClips/",dfx$fn,sep='')),'/',FALSE),copy.date=TRUE)
    unlink(as.character(dfx$lsst))
    setwd(paste(pldrive,'My Playlists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
    addfnfo=file.info(fn)
    addfnfo$lsst=fn
    addfnfo$xx=2^(length(wpls)-1) # wa1.wpl
    addfnfo$md5s=md5sum(fn)
    addfnfo$bn=as.integer(gsub('[a-z|A-Z|_|-]','',addfnfo$lsst))
    addfnfo$studio=''
    addfnfo$cmt=''
    addfnfo$Obn=addfnfo$bn
    mfnfo=rbind(mfnfo,addfnfo)
    tod=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    lastr=nrow(mfnfo)
    mfnfo[(lastr-len(fn)+1):lastr,c("mtime","ctime","atime")]=tod
    save(mfnfo,wpls,file='~/mfnfo.RData')
    load('c:/Users/Larry/Google Drive/cc.RData')
    vv=cc[0,] # append place holder
    vv[1:nrow(addfnfo),]=NA
    vv$fn=addfnfo$lsst
    vv$mtime=file.mtime(vv$fn)
    vv$bn=beaner(vv$fn)
    vv$Obn=vv$bn
    cc=rbind(cc,vv)
    save(cc,ss,file='c:/Users/Larry/Google Drive/cc.RData')
    setwd(paste(pldrive,'My Playlists',sep=""))
    fn1=paste(drive,'PNMTALL/RPDNClips/',fn,sep='')
    writeLines(gsub('/','\\\\',fn1),'fns.m3u') # Write playlist
    shell('"C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe " fns.m3u')
    
  }else print('No new files found')
} else print('CANNOT OPEN FLK')

load('~/mfnfo.RData')
setwd(paste(pldrive,'My Playlists',sep=""))
extx=gsub('.wpl','',wpls,fixed=TRUE)
for(fnx in fn){
  exx=exts[unlist(sapply(exts,function(x) gregexpr(x,fnx)))>0]
  exx=exx[!is.na(exx) & exx!='blfd']
  print(paste('fnx=',fnx,'exx=',exx))
  bb=which(extx %in% exx)
  bits=sum(2^(bb-1))
  mfnfo[mfnfo$lsst==fnx,'xx']=bitwOr(mfnfo[mfnfo$lsst==fnx,'xx'],bits)
}
if(len(fn)>0){
  save(mfnfo,wpls,file='~/mfnfo.RData')
  source('~/pllist.git/Buildmfnfo.R')
}
load('~/dirtbl.rdata')
dirtbl=subset(dirtbl,nchar(as.character(dirtbl$Var1))>1)
dirtbl=subset(dirtbl,!grepl(' ',as.character(dirtbl$Var1)))
orggs=dir('D:/PNMTALL/RPDNClips',pattern='*_original',full.names = TRUE)
orggs=c(orggs,dir('c:/RealPlayerDownloads',pattern='*_original',full.names = TRUE))
file.remove(orggs)



