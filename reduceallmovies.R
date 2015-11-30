cd('~/')
cls=NULL
ptn=function(x) prettyNum(x,big.mark = ",")
#cls=c(cls,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
#cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/my videos/rpdnclips',recursive = TRUE,full.names = TRUE))
#cls=c(cls,dir('c:/Users/Larry/Downloads',recursive = TRUE,full.names = TRUE))
fna=cls # all files (including _New's)
cls=cls[which(!grepl('_New',cls))]
cla=fna[which(grepl('_New',fna))]
dfn=data.frame(cla)
dfn$cls=sub('_New','',dfn$cla)
dfn$sz=file.size(dfn$cls)
dfn=dfn[order(dfn$sz),]
dfn=dfn[!is.na(dfn$sz),]

nfns=paste(file_path_sans_ext(cls),'_New.',file_ext(cls),sep='')
dfa=data.frame(cls,sz=file.size(cls),nfns)
dfa=subset(dfa,!file.exists(as.character(dfa$nfns))) # remove already converted to _New
dfa=dfa[order(dfa$sz,decreasing=TRUE),]
#dfa=dfa[order(dfa$sz,decreasing=FALSE),] # for clips #***************

bads=data.frame(fname=NA,errorC=NA)
if(file.exists('~/bads.RData'))
  load('~/bads.RData')

dfa=dfa[!dfa$cls %in% bads$fname,]
if(!file.exists('~/msgis.txt')){
  writeLines('','~/msgis.txt')
}
for(fn in dfa$cls)
{ 
  print(paste(len(dfa$cls)-which(fn==dfa$cls),'Files Remaining',
              ptn(sum(file.size(as.character(dfa$cls)),na.rm = TRUE)),'bytes Remaining',Sys.time()))
  nfn=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
  print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
  if(file.exists(fn) & !file.exists(nfn) & file.size(fn)==dfa[which(dfa$cls==fn),'sz']){
    if(file.exists('~/out.mp4')){
      shell('tasklist > tl.txt')
      tl=readLines('tl.txt')
      if(any(grepl('ffmpeg.exe',tl,fixed=TRUE)))
        shell('taskkill /F /IM  ffmpeg.exe')
      if(!file.remove('~/out.mp4')){
        stop('Cannot Remove out.mp4, kill ffmpeg.exe')}
    }
    
    msgi=shell(paste('c:/Users/Larry/Documents/hexDump/bin/converth265.bat "',
                     fn,'" c:/Users/Larry/Documents/out.mp4',sep=''),translate = TRUE,intern=TRUE)
    print(tail(msgi))
    if(file.exists('~/out.mp4')){
    if(file.size('~/out.mp4')>1000){
      shell("c:/Users/Larry/Documents/HexDump/bin/exiftool c:/Users/Larry/Documents/out.mp4 > exifdata.txt")
      exif=readLines('~/exifdata.txt')
      if(any(grepl('Lavf56|Lavf57',exif))){
        file.copy('~/out.mp4',nfn)
        print(paste(fn,ptn(file.size(fn))))
        print(paste(nfn,ptn(file.size(nfn))))
        file.remove(fn)
      }else{
        bads[badx,]$fname=fn
        bads[badx,]$errorC='Bad Metadata'
        badx=badx+1
        save(bads,badx,file='~/bads.RData')
        print('bad metadata')
      }
    }else{
      bads[badx,]$fname=fn
      bads[badx,'errorC']='Bad Size'
      badx=badx+1
      save(bads,badx,file='~/bads.RData')
      print('Bad size')
    }
    }else{
      bads[badx,]$fname=fn
      bads[badx,'errorC']='Bad Moov'
      badx=badx+1
      save(bads,badx,file='~/bads.RData')
      print('Bad Moov')
    }
    writeLines(msgi,'~/temp.txt')
    msgi=readLines('~/temp.txt') # convert CR to CRLF
    writeLines(msgi,'~/temp.txt')
    file.append('~/msgis.txt','~/temp.txt')
  }
}

