source('~/pllist.git/EnterStartStop.R') # galert in here
source('~/pllist.git/FFMPEGProgressBar.R')
source('~/pllist.git/ProtoConvertH265Func.R')
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
tpexists=FALSE

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
      xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                     svt[i],'" ' ,sep=''),translate = TRUE, intern = TRUE)
      durx=paste(subset(xx,grepl('Format  ',xx))[2],subset(xx,grepl('Duration  ',xx))[1])
    }
    dur[i]=gsub('  ','',durx)
  }
  return(dur)
}

file.remove(dir(pattern = 'file'))
cd('~/')
fname=NULL
dwnlds=NULL
of=''
choices=c('D:/PNMTALL','C:/PNMTALL','C:/PNMTALL/RPDNclips','C:/RealPlayerDownloads',
          'c:/PNMTALL/NewDownloads', 'REDUCE only')
source('~/pllist.git/ChooseDIRS.R')
if(len(sll)>0){
  slx=which(choices %in% sll)
  if(1 %in% slx)
    fname=c(fname,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
  if(2 %in% slx){
    fname=c(fname,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
    fname=fname[!grepl('c:/PNMTALL/NewDownloads',fname)]}
  if(3 %in% slx)
    fname=c(fname,dir('c:/PNMTALL/rpdnclips',recursive = TRUE,full.names = TRUE))
  if(4 %in% slx)
    fname=c(fname,dir('C:/RealPlayerDownloads',recursive = TRUE,full.names = TRUE))
  if(5 %in% slx)
    dwnlds=dir('c:/PNMTALL/NewDownloads',recursive = TRUE,full.names = TRUE)
  fname=c(fname,dwnlds)
  if(6 %in% slx)
    fname=fname[grepl('REDUCE',fname) |grepl('RPDN',fname)]
  
  fname=fname[which(!grepl('crdownload|.ini|_REN',fname,fixed=TRUE))]
  fna=fname # all files (including _New's)
  fname=fname[which(!grepl('_New',fname))]
  if(len(fname)>0){
    cla=fna[which(grepl('_New',fna))]
    dfn=data.frame(cla)
    dfn$fname=sub('_New','',dfn$cla)
    dfn$sz=file.size(dfn$fname)
    dfn=dfn[order(dfn$sz),]
    dfn=dfn[!is.na(dfn$sz),]
    
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
    
    bdsa=subset(bads,errorC=='AVC')
    dfa7=dfa6[!duplicated(toupper(dfa6$fname)),]
    dfa8=dfa7[!toupper(dfa7$fname) %in% toupper(bads$fname),]
    dfa9=dfa7[toupper(dfa7$fname) %in% toupper(bdsa$fname),]
    dfa8=rbind(dfa8,dfa9)
    dfa=dfa8[order(dfa8$sz,decreasing=decreasing),]

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
      rng=which(fn==dfa$fname):len(dfa$fname) #range pre-calc
      if(grepl('HEVC|VC-1',durt1)){
        print('HEVC/VC-1 FOUND')
        svv(as.character(dfa[rng[which(grepl('HEVC',durt1))],'fname']),"Already HEVC")
        svv(as.character(dfa[rng[which(grepl('VC-1',durt1))],'fname']),"Bad Size")
        next
      }

      durt=getDur(dfa[rng[1]:rng[min(len(rng),13)],c('fname','durF')])
      dfa[rng[1:min(len(rng),13)],'durF']=durt[1:min(len(rng),13)]
      nfn1=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
      nfn=sub('REDUCE','',nfn1)
      clflag=FALSE
      if(grepl('rpdnclips',nfn,ignore.case = TRUE)){
        clflag=TRUE
        nfn=sub('_New','',nfn)
      }
      done=FALSE
      print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
      of=convH265(fn,ttl=fn,nfn) # Conversion routine
      if(aborted | done)
        break # aborted
  }
  
  if(isExtant(ww))
    dispose(ww)
  }
}
