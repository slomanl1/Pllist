cd('~/')
cls=NULL
dwnlds=NULL
cls=c(cls,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/my videos/rpdnclips',recursive = TRUE,full.names = TRUE))
dwnlds1=dir('c:/Users/Larry/Downloads',recursive = TRUE,full.names = TRUE)
dwnlds=dwnlds1[grepl('REDUCE',dwnlds1)]
cls=c(cls,dwnlds)
cls=cls[which(!grepl('crdownload|ini|_REN',cls))]
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
dfa=dfa[order(dfa$sz,decreasing=FALSE),]

bads=data.frame(fname=NA,errorC=NA)
if(file.exists('~/bads.RData'))
  load('~/bads.RData')

dfa=dfa[!toupper(dfa$cls) %in% toupper(bads$fname),]
if(!file.exists('~/msgis.txt')){
  writeLines('','~/msgis.txt')
}
for(fn in dfa$cls)
{ 
  print('')
  print(paste(len(dfa$cls)-which(fn==dfa$cls),'Files Remaining',
              ptn(sum(file.size(as.character(dfa$cls)),na.rm = TRUE)/1000),'Kbytes Remaining',Sys.time()))
  nfn1=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
  nfn=sub('REDUCE','',nfn1)
  print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
  ddd=shell(paste('mediainfo "',fn,'"',sep=''),intern = TRUE)
  hevcFlag=any(grepl('HEVC',ddd))
  if(file.exists(fn) & !file.exists(nfn) & file.size(fn)==dfa[which(dfa$cls==fn),'sz']){
    if(!hevcFlag){
      print(subset(ddd,grepl('Duration',ddd))[1])
      of=paste(basename(tempfile()),'.mp4',sep='')
      print(of)
      msgi=shell(paste('c:/Users/Larry/Documents/hexDump/bin/converth265.bat "',
                       fn,'" ',of,',' ,sep=''),translate = TRUE, intern = TRUE)
      print(tail(msgi))
      if(file.exists(of)){
        if(file.size(of)>1000){
          if(any(grepl('hevc',msgi))){
            if(file.copy(of,nfn))
              unlink(of)
            print(paste(fn,ptn(file.size(fn))))
            print(paste(nfn,ptn(file.size(nfn))))
            file.remove(fn)
          }else{
            bads[badx,]$fname=fn
            bads[badx,]$errorC='Bad Metadata'
            badx=badx+1
            save(bads,badx,file='~/bads.RData')
            print('bad metadata')
            unlink(of)
          }
        }else{
          bads[badx,]$fname=fn
          bads[badx,'errorC']='Bad Size'
          badx=badx+1
          save(bads,badx,file='~/bads.RData')
          print('Bad size')
          unlink(of)
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
    }else{
      bads[badx,]$fname=fn
      bads[badx,'errorC']='Already HEVC'
      badx=badx+1
      save(bads,badx,file='~/bads.RData')
      print('Already HEVC')
    }
  }
}
