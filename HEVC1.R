source('~/pllist.git/BuildxxALT.R')
cd('c:/myvideos/rpdnclips')
load('~/mfnfo.RData')
load('~/bads.RData')
mg1=merge(mfnfo[,c('lsst','md5s')],bads,by='md5s')
bds=data.frame(fname=normalizePath(mg1$lsst,winslash = '/'),errorC=mg1$errorC,md5s=mg1$md5s)
mssng=mfnfo[which(!mfnfo$lsst %in% basename(as.character(bds$fname))),'lsst']
unlink('~/meta.txt')
for(i in 1:len(mssng)){
  svt=normalizePath(mssng[i],winslash = '/')
  print(svt)
  cmdd=paste('shell("mediainfo.exe',svt,' >> c:/Users/Larry/Documents/meta.txt",mustWork=NA,translate=TRUE)')
  print(svt)
  eval(parse(text=cmdd))
}
shell('findstr "VC Complete" c:\\Users\\Larry\\Documents\\meta.txt > c:\\Users\\Larry\\Documents\\hevc.txt')
hvc=readLines('~/hevc.txt')
hvc=data.frame(hvc,fc=NA)
hvc$fc=as.numeric(as.factor(substr(hvc$hvc,1,10)))
hx=c(diff(hvc$fc),NA)
hvc$df=hx
fns1=hvc[grepl('Complete',hvc$hvc),'hvc']
fns2=sub('Complete name                            : ','',fns1)
fns=subset(fns2,file.exists(fns2))
hevcs=hvc[grepl('Complete',hvc$hvc) & (hvc$df!=0),'hvc']
hevcs=sub('Complete name                            : ','',hevcs)
hevcs=normalizePath(hevcs,winslash = '/')
bds=data.frame(fname=normalizePath(fns,winslash = '/'),errorC=NA,md5s=md5sum(fns))
save(hvc,hevcs,bds,fns,bads,file='~/hvc.rdata')
unlink('~/meta.txt')
vc1s=(hvc[which(grepl('VC-1',hvc$hvc,fixed=TRUE))-1,'hvc'])
hevcss=(hvc[which(grepl('HEVC',hvc$hvc,fixed=TRUE))-1,'hvc'])
avcs=(hvc[which(grepl('AVC',hvc$hvc,fixed=TRUE))-1,'hvc'])
vc1s=sub('Complete name                            : ','',vc1s)
hevcss=sub('Complete name                            : ','',hevcss)
avcs=sub('Complete name                            : ','',avcs)
vc1s=data.frame(fname=normalizePath(vc1s,winslash = '/'))
hevcss=data.frame(fname=normalizePath(hevcss,winslash = '/'))
avcs=data.frame(fname=normalizePath(avcs,winslash = '/'))
bdx1=merge(bds,hevcss,by='fname')
bdx1$errorC='Already HEVC'
bdx2=merge(bds,vc1s,by='fname')
bdx2$errorC='VC-1'
bdx3=merge(bds,avcs,by='fname')
bdx3$errorC='AVC'
bads=rbind(bdx1,bdx2,bdx3,bads)
bads=bads[!duplicated(bads$fname),]
save(bads, file = "~/bads.RData")
