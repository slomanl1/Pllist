source('~/pllist.git/writeDate.R')
load('~/gdframeOld.RData')
gfdo=gdframe
load('~/gdframe.RData')
gdn=subset(gdframe,grepl('RPDNC',fnx))
gdo=subset(gfdo,grepl('RPDNC',fnx))
gdn$bn=beaner(gdn$fnx)
gdo$bn=beaner(gdo$fnx)
mgg=merge(gdn,gdo,by='bn')
mm=mgg[which(as.Date(mgg$Date.x) - as.Date(mgg$Date.y)>10),c('fnx.x','fnx.y','Date.x','Date.y')]
# if(nrow(mm)>0){
#   source('~/Pllist.Git/writeDate.R')
#   for( i in 1:nrow(mm)) WriteDate(mm$fnx.x[i],mm$Date.y[i])
# }

gdf1=gfdo
gdframe$fnn=sub('_New','',gdframe$fnx)
mgg=merge(gdf1,gdframe,by.y='fnn',by.x='fnx')
xx=mgg[which(as.Date(mgg$Date.x)-as.Date(mgg$Date.y) < -10),]
if(nrow(xx)>0){
  for(i in 1:nrow(xx)) WriteDate(xx$fnx.y[i],xx$Date.x[i])
}else{
  file.copy('~/gdframe.Rdata','~/gdframeOld.RData',overwrite = TRUE )
}
