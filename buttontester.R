library(gWidgets2)
options(guiToolkit = "RGtk2") 

if(exists('w')){
  if(isExtant(w))
    dispose(w)
}
scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))

orggs=dir('D:/PNMTALL/RPDNClips',pattern='*_original',full.names = TRUE)
orggs=c(orggs,dir('c:/RealPlayerDownloads',pattern='*_original',full.names = TRUE))
if(len(orggs)>0)
  file.remove(orggs)

load('~/mfnfo.RData')
dd=dir()
dd=subset(dd,!grepl('.ini',dd,fixed=TRUE))
dd=subset(dd,!grepl('original',dd,fixed=TRUE))
if(any(!file.exists(mfnfo$lsst)) | len(dd[which(!dd %in% mfnfo$lsst)])>0){
  print('MFNFO not in sync with files, REBUILDING')
  catt(dd[which(!dd %in% mfnfo$lsst)])
  galert('MFNFO not in sync with files, REBUILDING')
  source('~/pllist.git/Buildmfnfo.R')
}

source('~/pllist.git/WriteDate.R')
source('~/pllist.git/cleanwplsfn.R')
source('~/pllist.git/editClipName.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source

#3108256301 Singer Pager No-08435, Dr. Avidan
gotSt=FALSE
breaker=FALSE
quitter=FALSE
newfnn=NA
nibble=NA
oSt=''
nibbled=FALSE
randd=FALSE # random flag (test for rmnss > 0) case ################
sveddx=''

selFiles = function(){
  selectL = select.list(c('ONLY','AND','OR','NOT'),graphics=TRUE)
  print(selectL)
  if(nchar(selectL)> 0) { 
    print(nchar(selectL))
    selectorf = select.list(wpls,multiple=(selectL!='ONLY'),graphics=TRUE)
    if (length(selectorf) > 0) {
      print(selectorf)
      bits=0
      a=1:length(wpls)
      for (i in 1:length(selectorf)){
        b=which(selectorf[i]==wpls)
        if(len(b)>0)
          if (!is.na(b)) 
            bits = bitwOr(bits,2^(b-1))
      }
      if (selectL == 'AND')
        flist1 = mfnfo[bitwAnd(mfnfo$xx,bits) == bits,'lsst']
      else
        if (selectL == 'OR')
          flist1 = mfnfo[bitwAnd(mfnfo$xx,bits) > 0,'lsst']
      else #NOT
        flist1 = mfnfo[bitwAnd(mfnfo$xx,bits) == 0,'lsst']
      
      flist2 = flist1[flist1!='']
      if(selectL=='ONLY'){
        lsst=mfnfo$lsst
        flist2=subset(lsst,grepl(substr(selectorf,1,nchar(selectorf)-4),lsst))
      }
      
      enabled(flb)=TRUE
      return(flist2)
    }
  }else{
    #    .GlobalEnv$filterx=''
    enabled(xdl)=TRUE
    enabled(flb)=TRUE
    enabled(xaa)=TRUE
    enabled(xR)=TRUE
    enabled(xL)=TRUE
    enabled(xe)=TRUE
    font(flb) <- list(weight="normal",color='black')
    if(exists('gal')){
      if(isExtant(gal))
        dispose(gal)
    }
    
  }
}

setButtonColors= function(clr='black', wgt='normal',btnn=1:nrow(dirtbl)){
  if(len(btnn))
    for(i in 1:len(btnn)){
      eval(parse(text=paste('.GlobalEnv$bx=btnL',btnn[i],sep='')))
      font(.GlobalEnv$bx) <- c(color=clr, weight=wgt) # initial
    }
}

reorgpkd = function(topix){
  pkdi=rbind(pkdf,pkdf[1:(topix-1),])
  pkdi=pkdi[topix:nrow(pkdi),]
  return(pkdi)
}

checkIdent = function(){
  load('~/pkdfsave.RData')
  npkdf=pkdf[order(pkdf$fname),]
  nmfnfo=mfnfo[order(mfnfo$md5s),]
  nmfnfo$bn=as.integer(nmfnfo$bn)
  load('~/PKDWorkSave.RData')
  mfnfo=mfnfo[order(mfnfo$md5s),]
  mfnfo$bn=as.integer(mfnfo$bn)
  pkdf=pkdf[order(pkdf$fname),]
  rownames(pkdf)=NULL
  rownames(npkdf)=NULL
  return(identical(pkdf,npkdf) &identical(mfnfo,nmfnfo))
}

gtbP=function() paste(.GlobalEnv$pkdf[,'fname'],.GlobalEnv$pkdf[,'cmt'])

finderw=function(sll){
  if(len(sll)>0 & !any(is.na(sll))){
    .GlobalEnv$blockg=TRUE # prevent another search
    svalue(gedd)='FindNext' 
    .GlobalEnv$pkdf=reorgpkd(sll[1])
    gtb[,]=gtbP()
    .GlobalEnv$k=1
    .GlobalEnv$sveddx=svalue(geddx)
    visible(wdd)=FALSE
    .GlobalEnv$quitter=TRUE
  }else{
    galert('NONE FOUND')
    svalue(gedd)='Search'
  }
}

load('~/mfnfo.RData')
load('~/dirtbl.rdata')
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))

dd=data.frame(table(mfnfo$studio),stringsAsFactors = FALSE)
ee=dd[order(dd$Freq),]
mgg=merge(dirtbl,ee,by='Var1',all = TRUE)
mgg[is.na(mgg$Freq.y),'Freq.y']=1
mgg=mgg[mgg$Freq.y>0,]
dirtbl=mgg[order(mgg$Freq.y,decreasing = TRUE),]
names(dirtbl)=c('Var1','removv','Freq')
dirtbl$Var1=as.character(dirtbl$Var1)
dirtbl=dirtbl[nchar(dirtbl$Var1)>0 & dirtbl$Var1!='NA' & dirtbl$Var1 %in% mfnfo$studio,c('Var1','Freq')]
save(dirtbl,file='~/dirtbl.Rdata')

mfnfo$bn=gsub('[a-z|A-Z|_|-]','',mfnfo$lsst)
mfnfo$bn=as.integer(trim(sub('.','',mfnfo$bn,fixed=TRUE)))
mfnfo[which(is.na(mfnfo$studio)|grepl('Image',mfnfo$studio)),'studio']=''

mgg=mfnfo[order(mfnfo$bn),]
pkdf=data.frame(fname=mgg$lsst,studio=as.character(mgg$studio),cmt=as.character(mgg$cmt), stringsAsFactors = FALSE)
pkdf[is.na(pkdf$studio),'studio']=''
pkdf=pkdf[!duplicated(pkdf$fname),]
pkdf=pkdf[file.exists(pkdf$fname),]
pkdf[pkdf$studio==pkdf$cmt |is.na(pkdf$cmt),'cmt']=''
save(mfnfo,pkdf,file='~/pkdfSave.RData')
ss=0
for(b in 1:len(wpls)){
  ss[b]=sum(bitwAnd(mfnfo$xx ,2^(b-1))>0)
}
sx=data.frame(ss,wpls)
sy=sx[order(sx$ss,decreasing = TRUE),]

wplsx=gsub('.wpl','',sy$wpls,fixed=TRUE)

looped=FALSE
k=sample(nrow(pkdf),1)
.GlobalEnv$fnx=mfnfo[1,'lsst']
tpexist=FALSE
wddexists=FALSE
gconfer=FALSE
filterx=''
DblClkd=FALSE
FILT=FALSE
ggx=FALSE
blockg=FALSE

while(TRUE){
  print('MAIN LOOP')
  rmns=sum(nchar(pkdf$studio)<2,na.rm = TRUE)
  rmn=paste(rmns,'Files Remaining')
  if(rmns>0){
    if(!DblClkd){
      while(is.na(pkdf[k,'fname'])|is.na(pkdf[k,'studio'])){
        k=k+1
        if(k>nrow(pkdf))
          k=1 # wrap
      }
      while(!nchar(filterx) & nchar(trim(pkdf[k,'studio']))>1){
        k=k+1
        if(k>nrow(pkdf))
          k=1 # wrap
      }
      tryCatch.W.E(while(nchar(filterx) & pkdf[k,'studio']!=filterx){
        k=k+1
        if(k>nrow(pkdf))
          k=1 # wrap
      })
    }
  }
  if(randd)
    k=sample(nrow(pkdf),1)
  if(k>6){
    pkdf=reorgpkd(k)
    if(tpexist){
      gtb[,]=gtbP()
      svalue(gtb)=1
    }
    k=1
  }
  
  DblClkd=FALSE
  if(!file.exists(pkdf[k,'fname'])){
    bn=as.integer(gsub('[a-z|A-Z|_|-]','',pkdf[k,'fname']))
    dd=dir()
    dd=subset(dd,!grepl('orig',dd))
    bns=as.integer(gsub('[a-z|A-Z]|_|-','',dd))
    ix=which(bn==bns)
    if(len(ix)){
      pkdf[k,'fname']=dd[ix]
      gtb[,]=gtbP()
    }else{
      galert('File Not Found, May Need to run buildALT.R')
      k=k+1
      next
    }
  }
  
  shell(pkdf[k,'fname'],wait=FALSE)
  if(file_ext(pkdf[k,'fname'])=='asf'){
    shell('nircmd win close title "Windows Media Player"')
  }else{
    shell('nircmd win close process "vlc.exe"')
  }
  
  ofn=pkdf[k,] # save filename,studio for rename
  cmdd=sprintf("durr=shell('exiftool -DMComment -Duration %s',intern = TRUE)",ofn$fname)
  durr=eval(parse(text=cmdd))
  xx=strsplit(durr,' ')
  dmComment=''
  if(any(sapply(durr,function(x) grepl('DM Comment',x)))){
    ll=strsplit(xx[[2]][len(xx[[2]])],':')
    dmComment=capture.output(cat(xx[[1]][(which(xx[[1]]==':')+1):len(xx[[1]])]))
    if(dmComment=='NA :')
      dmComment=''
    if(tail(unlist(ll),1)=='s'){
      ll=strsplit(xx[[2]][len(xx[[2]])-1],':')
      ll=c(0,0,ll)
    }
  }else{
    ll=strsplit(xx[[1]][len(xx[[1]])],':')
    
  }
  
  lg=as.integer(unlist(ll))
  durx=lg[1]*3600+lg[2]*60+lg[3]
  
  print(paste('############# Playing',ofn$fname,'duration:',durx,'seconds, Comment',dmComment))
  blocker=FALSE
  if(tpexist){
    blocker=TRUE
    svalue(w)=paste(.GlobalEnv$pkdf[k,'fname'],.GlobalEnv$pkdf[k,'studio'],rmn)
    svalue(gtb)=k # select the line
    if(pkdf[k,'studio']!=dmComment & dmComment!="NA :"){
      svalue(cmtw)=dmComment
    }else{
      svalue(cmtw)=''
    } 
    focus(cmtw)=TRUE
  }
  Sys.sleep(1)
  
  ss=shell("cmdow.exe /T",intern = TRUE)
  sx=fi('wmpl',ss)
  if(len(sx)>0){
    hand=strsplit(sx,' ')[[1]][1]
    cmdd=sprintf('shell("cmdow %s /SIZ 1080 720")',hand)
    eval(parse(text=cmdd))
  }
  shell("nircmd win setsize process vlc.exe 0 0 1080 720")
  if(tpexist){
    enabled(xL)=TRUE
    enabled(xR)=TRUE
    enabled(flb)=TRUE
    focus(cmtw)=FALSE
    focus(cmtw)=TRUE
  }else{
    pbb=winProgressBar(min=1,max = nrow(dirtbl))
    w <- gwindow(parent = c(1190,0),height = 820,width=400,visible=FALSE)
    addHandlerDestroy(w,handler=function(h,...) {
      a$stop_timer()
      .GlobalEnv$breaker=TRUE
      gtkMainQuit()
    })
    
    g2=gvbox(cont=w,use.scrollwindow = TRUE)
    
    gtb=gtable(paste(pkdf$fname,pkdf$cmt),cont=g2,handler=function(h,...){
      if(!enabled(xdl)){
        gcxx=gconfirm('OK to Discard Changes')
        if(!gcxx){
          svalue(gtb)=k
          enabled(xdl)=TRUE
          enabled(flb)=TRUE
          enabled(xaa)=TRUE
          enabled(xR)=TRUE
          enabled(xL)=TRUE
          return()
        }
      }
      gtb[k,1]=paste(.GlobalEnv$pkdf[k,'fname'],.GlobalEnv$pkdf[k,'cmt']) # revert
      .GlobalEnv$k=svalue(gtb,index=TRUE)
      .GlobalEnv$DblClkd=TRUE
      gtkMainQuit()
    })
    
    g1=ggroup(cont=g2,horiz=TRUE)
    gxx=ggroup(cont=g2,horiz=TRUE)
    
    cmtw=gedit(dmComment,cont=gxx,handler=function(h,...) {
      if(blocker){
        .GlobalEnv$blocker=FALSE
        return()
      }
      if(svalue(cmtw)!=dmComment){
        .GlobalEnv$pkdf[k,'cmt']=svalue(cmtw) 
        .GlobalEnv$gotSt=TRUE
        #.GlobalEnv$nibbled=TRUE
        svalue(xaa)='OK'
        .GlobalEnv$k=.GlobalEnv$k+1
        #print('nibbled set TRUE, xaa to OK')
      }
    })
    gglbl=glabel('Edit Comment',cont=gxx)
    
    gckb=gcheckbox('Random',cont=gxx)
    
    gedd=gbutton('Search',cont=gxx,handler=function(h,...){
      if(blockg){ # blocked by changed in svalue of gedd
        .GlobalEnv$blockg=FALSE
        print('gedd blocked by blockg')
        return()
      }
      if(svalue(gedd)=='Search'){ 
        visible(wdd)=TRUE # not findnext case
      }else{
        .GlobalEnv$sll=which(grepl(sveddx,gtb[2:nrow(gtb),],ignore.case = TRUE)) # findNext case
        finderw(sll[2]+1)
        .GlobalEnv$blockg=FALSE # allow another search
      }
    })
    
    xb=gbutton('EXIT',cont=g1,handler=function(h,...) {
      dispose(w)
    })
    
    xL=gbutton('<-',cont=g1, handler=function(h,...){
      enabled(xL)=FALSE
      .GlobalEnv$breaker=FALSE
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)==0){
        svalue(gtb)=2
        print('xL returned with ixd==0')
        return()
      }
      print(paste('Left Arrow xL ixd=',ixd,'k=',k))
      ixd=ixd-1
      if(ixd<1){
        if(rmns==0){
          ixd=which(strsplit(svalue(gtb),' ')[[1]][1]==pkdf$fname)
          if(ixd==1)
            ixd=nrow(pkdf)
        }else{
          if(ixd==0)
            ixd=1
        }
        .GlobalEnv$pkdf=reorgpkd(ixd)
        gtb[,]=gtbP()
        .GlobalEnv$k=1
      }
      
      if(k==1 | k==2){
        .GlobalEnv$k=0
      }else{
        .GlobalEnv$k=.GlobalEnv$k-2
      }
      print(paste('xL quitting k=',k))
      .GlobalEnv$quitter=TRUE
    })
    
    xR=gbutton('->',cont=g1, handler=function(h,...){
      .GlobalEnv$quitter=FALSE
      ixd=svalue(gtb,index = TRUE)
      print(paste('xR ixd=',ixd))
      if(len(ixd)==0){
        return()
      }
      enabled(xR)=FALSE
      print(paste('right arrow','newfnn=',newfnn,'ixd=',ixd))
      if(ixd>5){
        if(rmns==0){
          ixd=which(strsplit(svalue(gtb),' ')[[1]][1]==pkdf$fname)+7 
        }
        .GlobalEnv$pkdf=reorgpkd(ixd-6)
        gtb[,]=gtbP()
        .GlobalEnv$k=1
      }
      if(svalue(gedd)!='Search'){
        .GlobalEnv$blockg=TRUE
        svalue(gedd)='Search'
      }
      print(paste('rA quitting k=',k))
      .GlobalEnv$quitter=TRUE
    })
    
    xdl=gbutton('DELETE',cont=g1,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        fnd=pkdf[ixd,'fname']
        yn=gconfirm(paste('delete',pkdf[ixd,'fname']))
        if(yn){
          shell(sprintf('nircmd moverecyclebin "%s"',fnd),translate=TRUE)
          .GlobalEnv$pkdf[ixd,'studio']=NA
          .GlobalEnv$pkdf[ixd,'fname']=NA
          .GlobalEnv$pkdf[ixd,'cmt']=NA
          .GlobalEnv$breaker=FALSE
          gtb[,]=gtbP()
          gtkMainQuit()
        }
      }
    })
    
    xe=gbutton('Backspace',cont=g1,handler=function(h,...){
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)){
        enabled(xdl)=FALSE
        enabled(flb)=FALSE
        #enabled(xaa)=FALSE
        #enabled(xd)=FALSE
        enabled(xR)=FALSE
        enabled(xL)=FALSE
        if(nchar(pkdf[ixd,'studio'])>0){  # remove studio
          .GlobalEnv$pkdf[ixd,'fname']=sub(paste('_',pkdf[ixd,'studio'],sep=''),'',pkdf[ixd,'fname'])
          .GlobalEnv$pkdf[ixd,'studio']=''
        }else{
          print('editing ah pn cd... here using cleanwplsfn.R')
          field=cleanwplsfn(pkdf[ixd,'fname'])
          xxx=strsplit(field$fy,' ')[[1]]
          print(paste('xxx=',xxx))
          if(len(xxx))
            xxx=xxx[1:len(xxx)-1]
          .GlobalEnv$pkdf[ixd,'fname']=paste(field$bn,capture.output(cat(xxx,sep='')),'.',file_ext(field$fname),sep='')
          .GlobalEnv$nibble=gsub(' ','',paste(field$bn,capture.output(cat(xxx,sep=''))))
          if(len(.GlobalEnv$nibble)==0)
            .GlobalEnv$nibble=''
          print(paste('backspace nibble',nibble))
        }
        svalue(w)=paste(.GlobalEnv$pkdf[ixd,'fname'],.GlobalEnv$pkdf[ixd,'studio'],rmn)
        gtb[k,1]=paste(ofn$fname,'<---',cleanwplsfn(.GlobalEnv$pkdf[ixd,'fname'])$fname,.GlobalEnv$pkdf[ixd,'studio'],.GlobalEnv$pkdf[ixd,'cmt'])
        svalue(gtb)=ixd # select line
      }
    })
    
    xaa=gbutton('ADD',cont=g1,handler=function(h,...){ # ADD/OK button
      print((paste('ADD/OK HANDLER-sval of xaa=',svalue(xaa),'nibbled=',nibbled)))
      if(!enabled(xaa))
        return()
      if(!nibbled & nchar(trim(svalue(xd)))>0){
        ccx=gconfirm('Are you sure you want to add to DIRTBL?')
        if(!ccx)
          return()
        dad=dirtbl[1,]
        dad$Var1=trim(svalue(xd))
        dad$Freq=1
        .GlobalEnv$dirtbl=rbind(.GlobalEnv$dirtbl,dad)
        save(dirtbl,file='~/dirtbl.Rdata')
        addRow(dad$Var1,'NA',nrow(dirtbl))
      }else{
        if(svalue(xaa)=='OK' & !nibbled){
          print(paste('exit HERE','svalue(xaa)=',svalue(xaa)))
          enabled(xaa)=FALSE
          enabled(xdl)=FALSE
          enabled(xe)=FALSE # backspace
          .GlobalEnv$gotSt=TRUE
          .GlobalEnv$pkdf[k,'studio']=ofn$studio
          .GlobalEnv$nibble=NA
          print('nibbled Cleared2')
          .GlobalEnv$nibbled=FALSE
          gtkMainQuit()
        }
      }
      .GlobalEnv$nibbled=FALSE
    })
    
    flb=gbutton('Filt',cont=g1,handler=function(h,...){
      a$stop_timer()
      print('FILT handler')
      if(nchar(filterx)|ggx){
        .GlobalEnv$ggx=FALSE
        .GlobalEnv$filterx=''
        font(flb) <- list(weight="normal",color='black')
        enabled(xdl)=TRUE
        enabled(xaa)=TRUE
        enabled(xd)=TRUE
        enabled(xR)=TRUE
        enabled(xL)=TRUE
        enabled(xe)=TRUE
        if(exists('gal')){
          if(isExtant(gal))
            dispose(gal)
        }
        gtkMainQuit()
      }else{
        enabled(xdl)=FALSE
        #enabled(flb)=FALSE
        enabled(xaa)=FALSE
        #enabled(xd)=FALSE
        enabled(xR)=FALSE
        enabled(xL)=FALSE
        enabled(xe)=FALSE
        font(flb) <- list(weight="normal",color='green')
        .GlobalEnv$ggx=TRUE
        enabled(cmtw)=FALSE
        svalue(gglbl)='CHOOSE STUDIO FILTER, Enter=="ALL"'
        focus(xd)=TRUE
      }
    })
    
    xd=gedit(cont=g1,width = 4,handler=function(h,...){
      if(nchar(trim(svalue(xd)))==0){
        if(ggx)
          svalue(xd)='ALL'
        return()
      }
      bn=as.integer(gsub('[a-z|A-Z|_|-]','',pkdf$fname))
      ixd=NA
      if(!grepl('[A-z]',svalue(xd))) # test for non numeric to prevent warning
        ixd=which(as.integer(svalue(xd))==bn)[1]
      if(!is.na(ixd) & !ggx){ # test for filter input
        svalue(xd)=''
        if(ixd>5){
          .GlobalEnv$pkdf=reorgpkd(ixd)
          gtb[,]=gtbP()
          ixd=1
        }
        .GlobalEnv$k=ixd
        svalue(gtb)=1
        .GlobalEnv$breaker=FALSE
        .GlobalEnv$DblClkd=TRUE # no NOT na search
        gtkMainQuit()
      }else{
        svalue(gglbl)=''
        .GlobalEnv$btnn=which(grepl(trim(svalue(xd)),dirtbl$Var1,ignore.case = TRUE))
        if((len(btnn)==0) & ggx & toupper(svalue(xd))=='ALL')
          buttonHandler('ALL')
        if(toupper(trim(svalue(xd))) %in% toupper(dirtbl$Var1)&len(btnn)==1){
          galert(paste('EXACT match DETECTED-',dirtbl$Var1[btnn[1]]))
          buttonHandler(btnn[1])
        }else{
          if(len(btnn)){
            if(len(btnn)==1){
              galert(paste('PARTIAL match DETECTED-',dirtbl$Var1[btnn[1]]))
              buttonHandler(btnn[1])
            }else{
              if(exists('ggw')){
                if(isExtant(ggw))
                  dispose(ggw)
              }
              .GlobalEnv$ggw=gwindow("Choose Group",parent=c(600,100))
              .GlobalEnv$ggpw=gvbox(cont=ggw,use.scrollwindow = TRUE)
              for(i in 1:len(btnn)){
                bz=gbutton(dirtbl$Var1[btnn[i]],action=i,cont=ggpw,handler=function(h,...){
                  cmdd=paste('btn_',i,'=bz',sep='')
                  eval(parse(text=cmdd))
                  if(exists('ggw')){
                    if(isExtant(ggw))
                      dispose(ggw)
                  }
                  buttonHandler(btnn[h$action])
                  shell("nircmd setcursor 1250 200")
                })
              }
              # }
              addHandlerKeystroke(ggw,handler=function(h,...){
                if(h$key=="\r"){
                  buttonHandler(dirtbl$Var1[btnn[1]])
                }
              })
              addHandlerDestroy(ggw,handler=function(h,...){
                enabled(xb)=TRUE # EXIT button
              })
              shell("nircmd setcursor 800 150")
            }
          }
        }
        .GlobalEnv$breaker=FALSE
        setButtonColors('red','bold',btnn)
        if(exists('gal')){
          if(isExtant(gal))
            dispose(gal)
        }
        svalue(xd)=''
      }
    })
    
    ggp=ggroup(cont=g2,horiz=FALSE)
    
    nibbler=function(action) {
      if(ggx)
        return()
      
      print(paste('nibbler action=',action,'nibbled=',nibbled))
      if(nchar(trim(pkdf[k,'studio']))>0 & svalue(xaa)=='ADD'){
        .GlobalEnv$nibbled=TRUE
        svalue(xaa)='OK'
        print('nibbled set TRUE xaa to OK')
      }
      if(!grepl(action,.GlobalEnv$nibble)){
        enabled(xdl)=FALSE
        enabled(flb)=FALSE
        enabled(xR)=FALSE
        enabled(xL)=FALSE
        if(is.na(.GlobalEnv$nibble)){
          .GlobalEnv$nibble=sub(paste('_',pkdf[k,'studio'],sep=''),'',file_path_sans_ext(.GlobalEnv$pkdf[k,'fname']))}
        .GlobalEnv$nibble=paste(nibble,action,sep='')
        .GlobalEnv$newfnn=paste(nibble,'_',pkdf[k,'studio'],'.',file_ext(pkdf[k,'fname']),sep='')
        svalue(w)=paste(cleanwplsfn(.GlobalEnv$newfnn)$fname,.GlobalEnv$pkdf[k,'studio'],rmn)
        gtb[k,1]=paste(ofn$fname,'<---',cleanwplsfn(.GlobalEnv$newfnn)$fname,.GlobalEnv$pkdf[k,'cmt'])
        .GlobalEnv$pkdf[k,'fname'] = cleanwplsfn(.GlobalEnv$newfnn)$fname
      }
      print(paste('nibble,newfnn,pkdf[k]',nibble,.GlobalEnv$newfnn,.GlobalEnv$pkdf[k,'fname']))
      
    }
    
    buttonHandler=function(action) {
      print('button Handler enter')
      enabled(xb)=FALSE
      if(ggx){
        if(action=='ALL'){
          .GlobalEnv$filterx='ALL'
        }else{
          if(dirtbl$Freq[action]>1){
            .GlobalEnv$filterx=as.character(dirtbl$Var1[action])
            font(flb) <- list(weight="bold",color='red')
            if(exists('gal')){
              if(isExtant(gal))
                dispose(gal)
            }
          }
        }
        galert(paste('filterx chosen',filterx))
        shell("nircmd setcursor 1250 200")
        .GlobalEnv$ggx=FALSE
        enabled(flb)=FALSE
        .GlobalEnv$selector =selFiles()
        gtkMainQuit()
      }else{
        if(!is.na(newfnn))
          .GlobalEnv$pkdf[k,'fname']=cleanwplsfn(newfnn)$fname
        if(is.na(pkdf[k,'studio']))
          browser()
        
        .GlobalEnv$oSt=paste('_',pkdf[k,'studio'],sep='') # old studio with underscore
        
        if(nchar(pkdf[k,'studio'])>0) {
          .GlobalEnv$gotSt=TRUE
          if(nchar(filterx)){
            .GlobalEnv$pkdf[k,'studio']=as.character(dirtbl$Var1[action])
            print(paste('replaced',filterx,'with',dirtbl$Var1[action]))
          }else{
            .GlobalEnv$pkdf[k,'studio']=as.character(dirtbl$Var1[action])
            .GlobalEnv$pkdf[k,'cmt']=svalue(cmtw)
            enabled(xaa)=FALSE
            svalue(xaa)='OK'
          }
        }else{
          .GlobalEnv$pkdf[k,'studio']=as.character(dirtbl$Var1[action])
          .GlobalEnv$pkdf[k,'cmt']=svalue(cmtw)
          .GlobalEnv$gotSt=TRUE
        }
        enabled(xdl)=TRUE
        enabled(flb)=TRUE
        enabled(xaa)=TRUE
        enabled(xR)=TRUE
        enabled(xL)=TRUE
        .GlobalEnv$nibble=NA
        .GlobalEnv$newfnn=NA
        print('BH Exit Q')
        .GlobalEnv$quitter=TRUE
      }
    }
    addRow= function(ttlL,ttlR,rowno){
      cmd1=sprintf('.GlobalEnv$pg%s=gpanedgroup(cont=ggp)',rowno)
      cmd2=sprintf('.GlobalEnv$btnL%s=gbutton("%s",cont=pg%s,action=%s,handler=function(h,...) {
        buttonHandler(h$action)})',rowno,ttlL,rowno,rowno)
      cmd3=sprintf('.GlobalEnv$btnR%s=gbutton("%s",cont=pg%s,action=%s,handler=function(h,...) {
        nibbler(wplsx[h$action])})',rowno,ttlR,rowno,rowno)
      
      eval(parse(text=cmd1))
      eval(parse(text=cmd2))
      eval(parse(text=cmd3))
      if(trim(ttlR)=='NA'){
        cmdx=sprintf('enabled(.GlobalEnv$btnR%s)=FALSE',rowno)
        eval(parse(text=cmdx))
      }
    }
    
    for(j in 1:nrow(dirtbl)){
      setWinProgressBar(pbb,j)
      addRow(substr(paste(dirtbl[j,'Var1'],'         '),1,10),substr(paste(wplsx[j],'      '),1,8),j)
    }
    
    visible(w) <- TRUE
    keep_above(w)
    size(gtb) <- c(200, 180)
    addSpring(g2)
    
    for(j in 1:nrow(dirtbl)){ # Set svalue of all buttons to .5 to force button size equal
      cmd4=sprintf('svalue(pg%s)=.5',j)
      eval(parse(text=cmd4))
    }
    tpexist=TRUE
    svalue(gtb)=k # select the line (first time)
    svalue(w)=paste(.GlobalEnv$pkdf[k,'fname'],.GlobalEnv$pkdf[k,'studio'],rmn)
    close(pbb)
  }
  if(!wddexists){
    wdd=gwindow('Enter RegExp',height=30,parent=c(800,100),visible=FALSE)
    addHandlerDestroy(wdd,handler=function(h,...){
      .GlobalEnv$wddexists=FALSE
      svalue(gedd)='Search'
    })
    
    geddx=gedit(sveddx, cont=wdd,handler=function(h,...){
      .GlobalEnv$sveddx=svalue(geddx)
      .GlobalEnv$sll=which(grepl(sveddx,gtb[,],ignore.case = TRUE))
      finderw(sll)
    })
    .GlobalEnv$wddexists=TRUE
  }
  #################################
  if(!exists('ww')){
    bb=c('WOWOW+','Ultimate','The Ultimate','The Ultimate+','The Ultimate++')
    ww=gwindow(width=1080,height=50,parent=c(20,700))
    w=ggroup(cont=ww)
    for (i in 1:len(bb)){
      gbutton(bb[i],cont=w,action=i,handler=function(h,...){
        load('~/ults.rdata')
        ults[nrow(ults)+1,'fn']=pkdf[k,'fname']
        ults[nrow(ults),'ult']=bb[h$action]
        save(ults,file='~/ults.RData')
        print(paste(bb[h$action],'Written'))
        if(file_ext(pkdf[k,'fname'])=='asf'){
          shell('nircmd win close title "Windows Media Player"')
        }else{
          shell('nircmd win close process "vlc.exe"')
        }
      })
    }
  }
  ############################
  ptm=proc.time()[3]
  focus(cmtw)=TRUE
  FUN <- function(data) {
    elapsed=proc.time()[3]-.GlobalEnv$ptm
    .GlobalEnv$ptm=proc.time()[3]
    #cat('.',durx)
    proc=shell('Process.exe -t | findstr "wmplayer vlc"',intern=TRUE)
    if(len(proc)>0 &!breaker &!quitter){
      lnn=len(strsplit(proc,' ')[[1]])
      cput=strsplit(proc,' ')[[1]][lnn-4]
      .GlobalEnv$durx=.GlobalEnv$durx-elapsed
      if(cput=='0' & durx <=0){
        cat('\n')
        print(paste(proc,'cpu is zero'))
        .GlobalEnv$k=.GlobalEnv$k+1
        .GlobalEnv$blockg=TRUE
        svalue(gedd)='Search'
        gtkMainQuit()
      }
    }else{
      if(breaker | quitter)
        gtkMainQuit()
      
      ixd=svalue(gtb,index = TRUE)
      if(len(ixd)==0){
        svalue(gtb)=2
        return()
      }
      if(ixd>6){
        if(rmns==0){
          ixd=which(strsplit(svalue(gtb),' ')[[1]][1]==pkdf$fname)+7 
        }
        .GlobalEnv$pkdf=reorgpkd(ixd-6)
        gtb[,]=gtbP()
        .GlobalEnv$k=0
      }
      .GlobalEnv$k=.GlobalEnv$k+1
      print(paste('FUN blip new',k))
      .GlobalEnv$blockg=TRUE
      svalue(gedd)='Search'
      gtkMainQuit() 
    }
  }
  Sys.sleep(2)
  
  a <- gtimer(250, FUN)
  gtkMain()  ###################################***************main loop********#################
  a$stop_timer()
  quitter=FALSE
  blockg=FALSE
  if(k==0)
    .GlobalEnv$k=1
  print(paste('GTK MAIN EXITED k=',k))
  enabled(cmtw)=TRUE
  if(isExtant(wdd)){
    visible(wdd)=FALSE
  }
  
  blocker=TRUE
  setButtonColors()
  nibbled=FALSE
  randd=svalue(gckb)
  rmn=paste(sum(!nchar(pkdf$studio),na.rm = TRUE),'Files Remaining')
  svalue(gglbl)='Edit Comment'
  if(gotSt){
    if(svalue(gedd)!='Search'){
      .GlobalEnv$blockg=TRUE
      svalue(gedd)='Search'
    }
    shell('nircmd win close process "vlc.exe"') # close to prevent sharing violation of file rename ofn--> new
    #shell('nircmd win close process "wmplayer.exe"')
    gtb[,]=gtbP()
    cmtww=svalue(cmtw)
    if(is.null(cmtww))
      cmtww=''
    k=k-1
    if(k==0)
      k=1
    
    bn1=beaner(ofn$fname)
    ixk=which(beaner(pkdf$fname)==bn1)
    print(paste('len(ofn),nrow(pkdf[ixk,]) =',len(ofn$fname),nrow(pkdf[ixk,]),'ixk=',ixk))
    if(len(ixk)!=1){
      print('Double ixk detected')
      browser()
    }
    mx=data.frame(lsst=ofn$fname,studio=.GlobalEnv$pkdf[ixk,'studio'],dmComment=cmtww, Title=NA, SubTitle=NA,stringsAsFactors = FALSE)
    .GlobalEnv$pkdf[ixk,'fname']=gsub(oSt,'',.GlobalEnv$pkdf[ixk,'fname']) # clear old studio
    .GlobalEnv$pkdf[ixk,'fname']=gsub(pkdf[ixk,'studio'],'',.GlobalEnv$pkdf[ixk,'fname']) # clear new studio if already there studio
    .GlobalEnv$pkdf[ixk,'fname']=paste(file_path_sans_ext(.GlobalEnv$pkdf[ixk,'fname']),'_',
                                       .GlobalEnv$pkdf[ixk,'studio'],'.',file_ext(pkdf[ixk,'fname']),sep='')
    .GlobalEnv$pkdf[ixk,'fname']=sub('__','_',.GlobalEnv$pkdf[ixk,'fname'])
    svalue(w)=paste(.GlobalEnv$pkdf[ixk,'fname'],rmn,'ofn',ofn$fname)
    writeStudio(mx,FALSE)
    
    bn2=beaner(pkdf[ixk,'fname'])
    if(len(bn1)!=1){
      print('Double ixk detected')
      browser()
    }
    if(bn1!=bn2){
      galert('Attempt to write non-identical bn')
      browser()
    }else{
      if(!file.rename(ofn$fname,pkdf[ixk[1],'fname'])){ # safety valve to prevent double ixk's (unknown error not resolved)
        galert('RENAME of clip failed')
      }else{
        gtb[,]=gtbP()
        msgga=paste('file rename success from',ofn$fname, 'to',pkdf[ixk,'fname'])
        galert(msgga)
        print(msgga)
      }
    }
    #.GlobalEnv$breaker=FALSE
    visible(g2)=FALSE
    Sys.sleep(.3)
    visible(g2)=TRUE
    k=k+1
    print(paste('In gotSt k=',k))
    svalue(gtb)=k # select the line
    .GlobalEnv$gotSt=FALSE
  }
  bn=as.integer(gsub('[a-z|A-Z|_|-]','',dir(pattern='*.wmv|*.asf')));
  ddpp=dups(bn)
  if(len(ddpp)>0){
    galert('Duplicate BNs Found')
    print(ddpp)
  }
  
  if((nchar(filterx)) & !FILT){
    FILT=TRUE
    if(len(selector)==0){
      fpkdf=subset(pkdf,studio==filterx)
    }else{
      fpkdf=subset(pkdf,studio==filterx & fname %in% selector)
    }
    if(filterx=='ALL'){
      if(len(selector)==0){
        fpkdf=pkdf # ALL everything
      }else{
        fpkdf=subset(pkdf,fname %in% selector)
      }
    }
    if(nrow(fpkdf)){
      save(pkdf,fpkdf,file='~/pkdfSave.RData')
      pkdf=fpkdf
    }else{
      galert('INVALID FILTER, NO FILES FOUND')
      FILT=FALSE
    }
    gtb[,]=gtbP()
  }else{
    if(FILT & nchar(filterx)==0){
      FILT=FALSE
      load('~/pkdfSave.RData')
      gtb[,]=gtbP()
    }
  }
  if(breaker)
    break
  svalue(gtb)=k # select the line
  enabled(xb)=TRUE # EXIT button
  svalue(xaa)='ADD'
  enabled(xaa)=TRUE # ADD/OK Button
  enabled(xdl)=TRUE # DELETE Button
  enabled(xe)=TRUE # backspace
  save(mfnfo,pkdf,file='~/PKDWorkSave.RData') # save for accidental loss of pkdf
}

pkdf=subset(pkdf,!is.na(fname)) # remove NA for deleted clips
save(mfnfo,pkdf,file='~/PKDWorkSave.RData') # clips w/studio not yet in mfnfo
shell('nircmd win close title "Windows Media Player"')
shell('nircmd win close process "vlc.exe"')
if(!checkIdent()){
  mfnfo=subset(mfnfo,xx>0)
  nms=names(mfnfo)
  mfnfo=subset(mfnfo,file.exists(mfnfo$lsst)&!is.na(mfnfo$bn))
  mgx=merge(pkdf,mfnfo,by.x='fname',by.y='lsst')
  mss=mgx[which(mgx$studio.x != mgx$studio.y | mgx$cmt.x != mgx$cmt.y),c('fname','studio.x','cmt.x')]
  studios=data.frame(fname=pkdf$fname,bn=as.integer(gsub('[a-z|A-Z|_|-]','',pkdf$fname)),
                     studio=as.character(pkdf$studio), cmt=as.character(pkdf$cmt),stringsAsFactors = FALSE)
  mgg=merge(mfnfo[,1:11],studios,by='bn') [,nms]
  zzm=pkdf[which(!pkdf$fname %in% mgg$lsst),]
  names(mss)=names(zzm)
  zzk1=unique(rbind(zzm,mss))
  zzk=zzk1[nchar(zzk1$studio)>0 &zzk1$studio!='NA',] # remove incomplete records (occurs on EXIT w/o write of studio field)
  if(nrow(zzk)){
    zzn=cbind(zzk,file.info(zzk$fname))
    print(paste('Calclating md5sum of zzk',nrow(zzk),'recs'))
    zzn$md5s=md5sum(zzk$fname)
    zzn$xx=0
    zzn$bn=as.integer(gsub('[a-z|A-Z|_|-]','',zzn$fname))
    names(zzn)[1]='lsst'
    zzn=zzn[,names(mfnfo)]
    for(i in 1:nrow(zzn)){
      newxx=mfnfo[which(mfnfo$md5s==zzn[i,'md5s']),'xx']
      if(len(newxx)==0)
        next
      zzn[i,'xx']=newxx[1] ######################## CHECK IT ***********************
      print(zzn[i,c('lsst','studio')])
    }
    zzx=getWplsXX(zzn$lsst,zzn)
    mfnfo=mfnfo[!mfnfo$bn %in% zzx$bn,]
    mfnfo=rbind(mfnfo,zzx)
    print(paste(nrow(zzx),'Records added to mfnfo'))
  }
  
  mfnfo=mfnfo[file.exists(mfnfo$lsst),]
  save(mfnfo,wpls,file='~/mfnfo.RData')
  source('~/pllist.git/Buildmfnfo.R')
  
}else{
  galert('NO CHANGES MADE')
}

setwd(paste('D:/PNMTALL/RPDNClips',sep=""))
bn=as.integer(gsub('[a-z|A-Z|_|-]','',dir()));
ddpp=dups(bn)
if(len(ddpp)>0){
  galert('Duplicate BNs Found')
  print(ddpp)
}

dispose(ww)

