source('~/.RProfile') # required for standalone version
cd('~/')
shell('curl -X POST http://192.168.0.77:8123/api/webhook/3312541')
scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
source('~/pllist.git/Buildmfnfo.R')
source('~/pllist.git/findDels.R')
source('~/pllist.git/EnterStartStop.R')
mx=NULL
ffg=FALSE
while(TRUE) {
  eere=tryCatch.W.E(shell('E:',intern = TRUE))
  if(!ffg)
  {
    zzz=galert('Waiting for E Power',100,x=600,y=500)
    ffg=TRUE
  }
  if(len(eere$warning)==0){
    dispose(zzz)
    break
  }
}
zzz=galert('drive E Connected',10,x=600,y=500)
cd('~/')
shell('dir D:\\ /S/B > zz.txt')
shell('dir c:\\PNMTALL /S/B >> zz.txt')
shell('dir E:\\PNMTALL /S/B > yy.txt')
zz = readLines('zz.txt')
zz=zz[!grepl('zip|txt',zz)]
yy = readLines('yy.txt')
cc=yy
save(cc,file='~/EPNMTALLyy.RData')
if(len(yy)){
  if(file.exists('~/Efound.RData')){
    load('~/Efound.RData')
    dfound=sub('e:','d:',efound,ignore.case = TRUE)
    file.copy(efound,dfound)
    file.remove('~/Efound.RData')
  }
  unlink('zz.txt')
  unlink('yy.txt')
  copyl1 = zz[!(substr(zz,2,100) %in% substr(yy,2,100))]
  copyl=copyl1[!grepl('RECYCLE|crdownload|RPDNClips|OneDrive|dsvsasa',copyl1) 
               & !file_ext(copyl1) %in% c('exe','msi',"rar","bin","lnk","part","")] # remove recycle bin entried
  reml = yy[!(substr(yy,2,100) %in% substr(zz,2,100))]
  unlink(reml,recursive = TRUE)
  ccdirs = sub('D:','E:',na.omit(copyl[file.info(copyl)[,'isdir']]),ignore.case = TRUE)
  ccdirs = sub('C:','E:',na.omit(copyl[file.info(copyl)[,'isdir']]),ignore.case = TRUE)
  ccdirs=ccdirs[!file.exists(ccdirs)]
  if (len(ccdirs) > 0)
    for (i in 1:len(ccdirs)) {
      dir.create(ccdirs[i])
      print(paste(ccdirs[i],'created'))
    }
  lnc = len(copyl)
  copyll=sub('D:','E:',copyl,ignore.case = TRUE)
  copyll=sub('C:','E:',copyll,ignore.case = TRUE)
  
  if (lnc > 0)
    for (i in 1:lnc) {
      if(!file.exists(copyll[i])){
        print(paste('Copying',copyll[i],lnc - i))
        file.copy(copyl[i],copyll[i])
      }
    }
  
  load('E:/mfnfo.RData')
  emfnfo=mfnfo
  load('~/mfnfo.RData')
  mgg=merge(mfnfo[,c('lsst','md5s')],emfnfo[,c('lsst','md5s')],by='md5s',all.x=TRUE)
  cd('E:/PNMTALL/RPDNClips')
  print(paste('Getting file sizes', nrow(mgg),'Files'))
  ee=file.size(mgg$lsst.y)
  print(paste('Done getting file sizes', len(ee),'Files'))
  cd('D:/PNMTALL/RPDNClips')
  cc=file.size(mgg$lsst.x)
  mgg$ee=ee
  mgg$cc=cc
  fils1=mgg[is.na(mgg$lsst.y),'lsst.x']
  fils=unique(c(fils1,mgg[which(!ee %in% cc),'lsst.x']))
  
  if(len(fils)){
    print(sprintf('Backing up %d RPDN Clips',len(fils)))
    nss=len(fils)
    k=0
    pb=winProgressBar(min=1,max=nss,label = sprintf('Backing up %d RPDN Clips',len(fils)))
    FUN <- function(data) {
      setWinProgressBar(pb, k, title =as.character(nss-k) )
    }
    backk <- gtimer(250, FUN)

    sapply(fils,function(x) {
      .GlobalEnv$k=.GlobalEnv$k+1
      if(!file.copy(x,'e:/PNMTALL/RPDNClips',overwrite=TRUE,copy.date=TRUE)){
        print(paste(x,'COPY FAILED'))
      }else{
        print(paste(x,'COPIED OK'))
      }
      })
    backk$stop_timer()
    print(" ")
    close(pb)
  }
  
  mm=mgg[!mgg$lsst.x==mgg$lsst.y & !is.na(mgg$lsst.y),]
  if(nrow(mm)){
    nfn=paste('E:/PNMTALL/RPDNClips/',mm$lsst.x,'.temp',sep='')
    ofn=paste('E:/PNMTALL/RPDNClips/',mm$lsst.y,sep='')
    rn1=file.rename(ofn,nfn)
    rn=paste(ofn,nfn)[rn1]
    ofn=nfn
    nfn=gsub('.temp','',nfn,fixed=TRUE)
    rn2=file.rename(ofn,nfn)
    rn=c(rn,rn=paste(ofn,nfn)[rn2])
    print(paste(rn, "renamed"))
  }
  file.copy('~/mfnfo.RData','e:/mfnfo.RData',overwrite=TRUE)
  file.copy('C:\\Users\\Larry\\Documents\\PNMTALL.RDATA','E:\\PNMTALL.RData',overwrite=TRUE)
  cd('~/')
  shell('bkk.bat')
  print('Done')
  ee=dir('E:/PNMTALL/RPDNClips')
  cc=dir('D:/PNMTALL/RPDNClips')
  mm=ee[which(!ee%in% cc)]
  cd('E:/PNMTALL/RPDNClips/')
  mx=mm[file.remove(mm)]
  if(length(mx))
    catt(paste(mx,'removed'))
  if(length(reml))
    catt(paste(reml,'removed'))
  print('Backk DONE')
  cd('~/')
  tryCatch.W.E(shell('mountvol E: /p'))
  shell('curl -X POST http://192.168.0.77:8123/api/webhook/3312542') # disk backup power OFF
}else{
  print('E: Drive not present Backup Aborted')
}

