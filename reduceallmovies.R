cd('~/')
cls=NULL
dwnlds=NULL
choices=c('D:/PNMTALL','C:/PNMTALL','C:/My Videos/RPDNclips','c:/Users/Larry/Downloads','REDUCE only')
sll=select.list(choices,multiple=TRUE,graphics = TRUE)
if(len(sll)>0){
  slx=which(choices %in% sll)
  if(1 %in% slx)
    cls=c(cls,dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
  if(2 %in% slx)
    cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
  if(3 %in% slx)
    cls=c(cls,dir('c:/my videos/rpdnclips',recursive = TRUE,full.names = TRUE))
  if(4 %in% slx)
    dwnlds=dir('c:/Users/Larry/Downloads',recursive = TRUE,full.names = TRUE)
  cls=c(cls,dwnlds)
  if(5 %in% slx)
    cls=cls[grepl('REDUCE',cls)]
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
  nfns=paste(file_path_sans_ext(cls),'_New.',file_ext(cls),sep='')
  dfa1=data.frame(cls,sz=file.size(cls),nfns)
  dfa1=subset(dfa1,!file.exists(as.character(dfa1$nfns))) # remove already converted to _New
  dfa=rbind(dfa1,dfa)
  
  bads=data.frame(fname=NA,errorC=NA)
  if(file.exists('~/bads.RData'))
    load('~/bads.RData')
  
  dfa=dfa[!toupper(dfa$cls) %in% toupper(bads$fname),]
  if(!file.exists('~/msgis.txt')){
    writeLines('','~/msgis.txt')
  }
  dfa=dfa[!duplicated(dfa$cls),]
  dfa=dfa[order(dfa$sz,decreasing=TRUE),]
  for(fn in dfa$cls)
  { 
    print('')
    print(paste(len(dfa$cls)-which(fn==dfa$cls),'Files Remaining',
                ptn(sum(file.size(as.character(dfa$cls)),na.rm = TRUE)/1000),'Kbytes Remaining',Sys.time()))
    
    nfn1=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
    nfn=sub('REDUCE','',nfn1)
    clflag=FALSE
    if(grepl('rpdnclips',nfn)){
      clflag=TRUE
      nfn=sub('_New','',nfn)
    }
    print(paste(fn,ptn(file.size(fn)),'nfn-',nfn))
    ddd=shell(paste('mediainfo "',fn,'"',sep=''),intern = TRUE)
    hevcFlag=any(grepl('HEVC',ddd))
    
    if(clflag & hevcFlag){
      bads[badx, ]$fname = fn
      bads[badx, "errorC"] = "Already HEVC"
      badx = badx + 1
      save(bads, badx, file = "~/bads.RData")
      print("Already HEVC")
    }
    if((clflag & !hevcFlag) | file.exists(fn) & !file.exists(nfn) & file.size(fn)==dfa[which(dfa$cls==fn),'sz']){
      if(!hevcFlag){
        mtime=file.mtime(fn)
        msize=file.size(fn)
        print(paste('Mtime=',mtime))
        print(subset(ddd,grepl('Duration',ddd))[1])
        of=paste(basename(tempfile()),'.mp4',sep='')
        print(of)
        msgi=shell(paste('c:/Users/Larry/Documents/hexDump/bin/converth265.bat "',
                         fn,'" ',of,',' ,sep=''),translate = TRUE, intern = TRUE)
        print(tail(msgi))
        if(file.exists(of)){
          if(file.size(of)>1000){
            if(any(grepl('hevc',msgi))){
              unlink(nfn)
              if(file.copy(of,nfn)){
                unlink(of)
                dx=data.frame(dtn=NA,fn=NA,times=NA)
                dx$dtn=mtime
                dx$fn=normalizePath(as.character(fn),winslash = '/')
                dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                               ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
                shell(paste('c:/Users/Larry/Documents/fdd.bat "',
                            nfn,'" ',dx$times,'' ,sep=''),translate = TRUE)
                
                print(paste('file mtime back to orig',file.mtime(nfn)))
                print(paste(fn,ptn(msize)))
                print(paste(nfn,ptn(file.size(nfn))))
                if(nfn!=fn)
                  file.remove(fn)
              }
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
}
