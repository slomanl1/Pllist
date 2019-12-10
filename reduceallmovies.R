source('~/pllist.git/EnterStartStop.R') # galert in here
source('~/pllist.git/FFMPEGProgressBar.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/WriteDate.R')
source('~/pllist.git/getMetadata.R')
source('~/pllist.git/rmmovname.R')

library(gWidgets2)
options(guiToolkit = "RGtk2")

scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
tpexists=FALSE
xx=NA

svv=function(filename,errorCode,printF=TRUE) {
  if(len(filename)==0)
    return()
  if(file.exists('~/bads.RData'))
    load('~/bads.RData')
  whh=which(toupper(filename)==toupper(bads$fname))
  if(!len(whh)){
    badsa=data.frame(fname=filename,errorC=errorCode,md5s=md5sum(filename))
    bads=rbind(bads,badsa)
    if(printF)
      print(paste(badsa$fname,badsa$errorC))
  }else{
    bads[whh,'errorC']=errorCode
    if(printF)
      print(paste(filename,errorCode))
  }
  bads=bads[!duplicated(bads$fname),]
  #print(paste('nrow bads=',nrow(bads)))
  save(bads,file='~/bads.RData')
}

getDur = function(svtDur) {
  svt=svtDur$fname
  durF=svtDur$durF
  dur=NA
  for(i in 1:len(svt)){
    if(!is.na(durF[i])){
      durx=durF[i]
    }else{
      .GlobalEnv$xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                                svt[i],'" ' ,sep=''),translate = TRUE, intern = TRUE)
      
      # if(any(grepl('Movie name',xx)))
      #   rmmovname(as.character(svt[i]),FALSE)
      xxm=fi('XMP Toolkit',.GlobalEnv$xx)
      studio=''
      if(len(xxm)>0){
        studio=trim(strsplit(xxm,':')[[1]][2])
      }
      zxx=subset(.GlobalEnv$xx,grepl('Image Size  ',.GlobalEnv$xx))
      isize=''
      if(len(zxx)>0)
        isize=strsplit(zxx,':')[[1]][2]
      durx=paste(subset(.GlobalEnv$xx,grepl('Format  ',.GlobalEnv$xx))[2],'  ',isize,
                 '- ',studio,subset(.GlobalEnv$xx,grepl('Duration  ',.GlobalEnv$xx))[1])
    }
    dur[i]=gsub('  |Image','',durx)
  }
  return(dur)
}

file.remove(dir(pattern = 'file')[!isLocked(dir(pattern = 'file'))&&!grepl('Facebook_files',dir(pattern = 'file'))])
cd('~/')
fname=NULL
dwnlds=NULL
of=''
choices=c('D:/PNMTALL','C:/PNMTALL','D:/PNMTALL/RPDNclips','C:/RealPlayerDownloads',
          '*_mp4',' D:/PNMTALL/NewDownloads', 'Include 1080p')
source('~/pllist.git/ChooseDIRS.R')
if(len(sll)>0){
  slx=which(choices %in% sll)
  if(1 %in% slx){
    fname=dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE)
    fname=fname[!grepl('D:/PNMTALL/RPDNClips',fname)]}  
  if(2 %in% slx){
    #fname=c(fname,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
    fname=fname[!grepl('D:/PNMTALL/NewDownloads',fname)]}
  if(3 %in% slx)
    fname=c(fname,dir('D:/PNMTALL/rpdnclips',recursive = TRUE,full.names = TRUE))
  if(4 %in% slx)
    fname=c(fname,dir('C:/RealPlayerDownloads',recursive = TRUE,full.names = TRUE))
  if(5 %in% slx){
    dwnlds=c(dwnlds,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
    dwnlds=subset(dwnlds,grepl('_mp4',dwnlds))
  }
  if(6 %in% slx)
    dwnlds=dir('D:/PNMTALL/NewDownloads',recursive = TRUE,full.names = TRUE)
  fname=c(fname,dwnlds)
  f1920=FALSE
  if(7 %in% slx)
    f1920=TRUE
  
  fname=fname[which(!grepl('crdownload|.ini|_REN',fname,fixed=TRUE))]
  fname=subset(fname,file_ext(fname) %in% c('asf','avi','flv','mov','mp4','MP4','wmv' ))
  fna=fname # all files (including _New's)
  fname=fname[which(!grepl('_New|.asf',fname))] # do not convert asf's to protect
  if(len(fname)>0){
    nfns=paste(file_path_sans_ext(fname),'_New.',file_ext(fname),sep='')
    dfa1=data.frame(fname,sz=file.size(fname),nfns,durF=NA,fdate=file.mtime(fname))
    dfa1$fsize=ptn(dfa1$sz)
    
    dfa2=subset(dfa1,!file.exists(as.character(dfa1$nfns))) # remove already converted to _New
    dfa3=dfa2[order(dfa2$sz,decreasing=decreasing),]
    nfns=paste(file_path_sans_ext(fname),'_New.',file_ext(fname),sep='')
    dfa4=data.frame(fname,sz=file.size(fname),nfns,durF=NA,fdate=file.mtime(fname))
    dfa4$fsize=ptn(dfa4$sz)
    dfa5=subset(dfa4,!file.exists(as.character(dfa4$nfns))) # remove already converted to _New
    dfa6=rbind(dfa3,dfa5)
    
    if(file.exists('~/bads.RData')){
      load('~/bads.RData')
    }else{
      bads=data.frame(fname=NA,errorC=NA,md5s=NA)
    }
    bads$fname=as.character(bads$fname)
    bdsa=subset(bads,errorC=='AVC')
    dfa7=dfa6[!duplicated(toupper(dfa6$fname)),]
    dfa8=dfa7[!toupper(dfa7$fname) %in% toupper(bads$fname),]
    dfa9=dfa7[toupper(dfa7$fname) %in% toupper(bdsa$fname),]
    dfa10=rbind(dfa8,dfa9)
    mm=getMetadata(dfa10$fname)
    bds11=data.frame(fname=as.character(mm$fns),errorC=ifelse(!grepl('1920',mm$ImageSize) & mm$format=='hev1','HEVC',''),md5s=NA,stringsAsFactors = FALSE)
    bads=rbind(bads,bds11[nchar(bds11$errorC)>0,])
    dfa10g=merge(dfa10,mm,by.x='fname',by.y='fns')
    dfa10h=subset(dfa10g,!grepl('TRIM|crdownload',dfa10g$fname))
    dfa10i=subset(dfa10h,(f1920 & grepl('1920',ImageSize)) | !grepl('hev1',format))
    dfa11=dfa10i[order(dfa10i$sz,decreasing=decreasing),]
    dfa=rbind(subset(dfa11,grepl('RPDNC',fname,ignore.case = TRUE)),rbind(subset(dfa11,!grepl('RPDNC',fname,ignore.case = TRUE))))
    dfa=dfa[!duplicated(dfa$fname),]
    ttl=paste(nrow(dfa),'Items',ptn(sum(dfa$sz)/1000),'KBytes')
    for(fn in dfa$fname)
    { 
      print('------------------------------------------------------------------------------')
      print(fn)
      txl=(paste(len(dfa$fname)-which(fn==dfa$fname),'Files Remaining',
                 ptn(sum(file.size(as.character(dfa$fname)),na.rm = TRUE)/1000),'Kbytes Remaining',Sys.time()))
      svalue(ww)=txl
      print(txl)
      durt1=getDur(dfa[which(fn==dfa$fname),c('fname','durF')])
      if(any(grepl('Movie name',xx))){
        rmmovname(as.character(fn),FALSE)
      }
      rng=which(fn==dfa$fname):len(dfa$fname) #range pre-calc
      if(grepl('HEVC',durt1) & !grepl('1920',durt1)){
        print('HEVC FOUND')
        svv(as.character(dfa[rng[which(grepl('HEVC',durt1))],'fname']),"Already HEVC")
        svv(as.character(dfa[rng[which(grepl('VC-1',durt1))],'fname']),"Bad Size")
        next
      }
      
      durt=getDur(dfa[rng[1]:rng[min(len(rng),17)],c('fname','durF')])
      dfa[rng[1:min(len(rng),17)],'durF']=durt[1:min(len(rng),17)]
      nfn1=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
      nfn=sub('REDUCE','',nfn1)
      clflag=FALSE
      if(grepl('rpdnclips',nfn,ignore.case = TRUE)){
        clflag=TRUE
        nfn=sub('_New','',nfn)
      }
      done=FALSE
      print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
      of=convH265(fn,ttl=fn,nfn,F720P=grepl('1920|3840',dfa$durF[which(fn %in% dfa$fname)])) # Conversion routine
      if(aborted | done)
        break # aborted
    }
    
    if(isExtant(ww))
      dispose(ww)
  }
  if(exists('bads')){
    bads=bads[!grepl('_mp4',bads$fname),]
    save(bads,file='~/bads.RData')
  }
  source('~/pllist.git/Buildmfnfo.R')  
}else{
  print('User Aborted')
  galert('User Aborted')
}


