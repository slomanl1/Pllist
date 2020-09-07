source('~/pllist.git/Buildmfnfo.R')
load('~/mfnfo.Rdata')
dd=mfnfo[,c('lsst','mtime')]
dd$bn=beaner(dd$lsst)
xx=dd[order(dd$bn,decreasing = TRUE),]
xx$otime=xx$mtime
#dts=unique(strptime(substr(xx$mtime,1,10),"%Y-%m-%d",tz='MST'))
xx$dts=strptime(xx$mtime,"%Y-%m-%d %H:%M:%S",tz='MST')
xx$dff=c(0,diff(xx$dts))
row.names(xx)=1:nrow(xx)

for(i in 1:nrow(xx)){
  print(i)
  if(abs(xx$dff[i+1])<100000)
    while(xx$dts[i+1]>xx$dts[i]){
      xx$dts[i+1]=xx$dts[i+1]-3600
    }
  print(xx$dts[i+1])
}





ww=which(xx$dts!=xx$otime)
for(i in 1:len(ww)){
  print(xx[ww[i],])
  WriteDate(xx[ww[i],'lsst'],xx[ww[i],'dts'])
}
