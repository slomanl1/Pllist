source('~/pllist.git/WriteDate.R')
load('~/dfan.rdata')
vc=dir('d:\\pnmtall\\vc1s')
vc1=sub('_mp4','',vc)
dfan=dfan[!grepl('RPDNClips',dfan$filename),]
zz=sapply(vc1,function(x) which(grepl(x,dfan$filename,fixed=TRUE))[1])
xx=as.data.frame(zz)
xx$fn=rownames(xx)
rownames(xx)=1:nrow(xx)
xx$fullfn=dfan[xx$zz,'filename']
xx$convname=vc
xx$fdt=file.mtime(xx$fullfn)
xx$dirnm=dirname(xx$fullfn)
save(dfan,xx,file='~/xxdfan.RData')
cd('D:/PNMTALL/VC1s')
dfx=subset(xx,grepl('_mp4',convname))
file.copy(dfx$convname,dfx$fullfn,overwrite = TRUE)
file.remove(dfx$convname)
if(nrow(dfx)>0){
  for (x in 1:nrow(dfx)){
    WriteDate(dfx[x,'fullfn'],xx[x,'fdt'])
  }
}
