cd('~/')
shell('dir D:\\ /S/B > zz.txt')
shell('dir c:\\PNMTALL /S/B >> zz.txt')
shell('dir E:\\PNMTALL /S/B > yy.txt')
zz = readLines('zz.txt')
yy = readLines('yy.txt')
unlink('zz.txt')
unlink('yy.txt')
copyl1 = zz[!(substr(zz,2,100) %in% substr(yy,2,100))]
copyl=copyl1[!grepl('RECYCLE|crdownload',copyl1)] # remove recycle bin entried
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
    print(paste('Copying',copyll[i],lnc - i))
    file.copy(copyl[i],copyll[i])
  }
shell('xcopy D: E: /S/D/Y/J',intern = TRUE)
print('Backing up RPDN Clips')
load('E:/mfnfo.RData')
emfnfo=mfnfo
load('~/mfnfo.RData')
mgg=merge(mfnfo[,c('lsst','md5s')],emfnfo[,c('lsst','md5s')],by='md5s',all.x=TRUE)
cd('C:/MyVideos/RPDNClips')
file.copy(mgg[is.na(mgg$lsst.y),'lsst.x'],'e:/RPDN',overwrite=TRUE)
mm=mgg[!mgg$lsst.x==mgg$lsst.y & !is.na(mgg$lsst.y),]
if(nrow(mm)){
  nfn=paste('E:/rpdn/',mm$lsst.x,'.temp',sep='')
  ofn=paste('E:/rpdn/',mm$lsst.y,sep='')
  rn1=file.rename(ofn,nfn)
  rn=paste(ofn,nfn)[rn1]
  ofn=nfn
  nfn=gsub('.temp','',nfn,fixed=TRUE)
  rn2=file.rename(ofn,nfn)
  rn=c(rn,rn=paste(ofn,nfn)[rn2])
  print(paste(rn, "renamed"))
}
file.copy('~/mfnfo.RData','e:/mfnfo.RData',overwrite=TRUE)
print('Done')
