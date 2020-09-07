load('~/mfnfo.RData')
cd('D:/PNMTALL/RPDNClips')
cc=mfnfo[,c('lsst','mtime','bn')]
cc$mt=substr(cc$mtime,1,10)
mtt=unique(cc$mt)
for(i in 1:length(mtt)){
  dd=subset(cc,grepl(mtt[i],cc$mtime))
 if(is.unsorted(dd$mtime)){
   print(paste(mtt[i],nrow(dd),'unsorted'))
   dd$mts=sort(dd$mtime)-(8*3600)
   sapply(1:nrow(dd), function(x) WriteDate(dd$lsst[x],dd$mts[x]))
 }
}
