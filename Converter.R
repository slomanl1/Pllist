print('Converter hello')
source('~/.RProfile') # required for standalone version
len=function(x) length(x)
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/FFMPEGProgressBar.R')
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
source('~/pllist.git/WriteDate.R')
tpexists=FALSE
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
      xxm=fi('XMP Toolkit',xx)
      studio=''
      if(len(xxm)>0){
        studio=trim(strsplit(xxm,':')[[1]][2])
        
      }
      zxx=subset(xx,grepl('Image Size  ',xx))
      isize=''
      if(len(zxx)>0)
        isize=strsplit(zxx,':')[[1]][2]      
      durx=paste(subset(xx,grepl('Format  ',xx))[2],isize,'-',studio,subset(xx,grepl('Duration  ',xx))[1])
    }
    dur[i]=gsub('  ','',durx)
  }
  return(dur)
}

shell('nircmd win min process rscript.exe')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
fname=NULL
args <- commandArgs(TRUE)
if(len(args)>0){
  fname=strsplit(args,' ')[[1]]
  save(args,fname,file='~/args.RData')
  catt(fname)
  # if(is.na(fname)){
  #   fname=normalizePath(choose.files(),winslash = '/')
  # }
}
if(is.null(fname)){
  fname=normalizePath(choose.files(),winslash = '/')
}
if(len(fname)>0){
  nfns=paste(file_path_sans_ext(fname),'_New.',file_ext(fname),sep='')
  dfa=data.frame(fname,sz=file.size(fname),nfns,durF=NA,fdate=file.mtime(fname))
  ttl=paste(nrow(dfa),'Items',ptn(sum(dfa$sz)/1000),'KBytes')
  dfa$fsize=ptn(dfa$sz)
  for(fn in dfa$fname)
  { 
    print('------------------------------------------------------------------------------')
    print(fn)
    txl=(paste(len(dfa$fname)-which(fn==dfa$fname),'Files Remaining',
               ptn(sum(file.size(as.character(dfa$fname)),na.rm = TRUE)/1000),'Kbytes Remaining',Sys.time()))
    #svalue(ww)=txl
    print(txl)
    durt1=getDur(dfa[which(fn==dfa$fname),c('fname','durF')])
    rng=which(fn==dfa$fname):len(dfa$fname) #range pre-calc
    if(grepl('HEVC',durt1)){
      print('HEVC FOUND')
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
}else{
  print('NONE FOUND')
}

