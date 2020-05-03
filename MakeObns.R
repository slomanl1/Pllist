cd("D:/PNMTALL/rpdnclips")
bb=shell('exiftool * -GPSLatitude',intern = TRUE)
writeLines(bb,'~/Obnsbb.txt')
bb=readLines('~/Obnsbb.txt')
lsst=fi('wmv',bb)
Obn=fi('GPS',bb)
obns=data.frame(lsst,Obn,stringsAsFactors = FALSE)
obns$lsst=substr(obns$lsst,10,nchar(obns$lsst))
ol=strsplit(obns$Obn,' ')
obns$Obn=as.integer(sapply(1:len(ol),function(x) ol[[x]][23]))
obns$mtime=file.mtime(obns$lsst)
dps=dups(obns$Obn)
obns$dupp=obns$Obn %in% dps
save(obns,file='~/obns.RData')

# Merge with cc.Rdata and exclude duplicates, fix obn dates and add Obn to excluded records
# *************** FIX Obn in makelast.R to force a unique Obn using last Obn in cc.Rdata, 
# Make sure to update cc.rdata
load('c:/Users/Larry/Google Drive/cc.RData')
mgg=merge(obns,cc,by.x='lsst',by.y='fn')

mgx=mgg[!mgg$dupp,]
which(abs(mgx$mtime.x-mgx$mtime.y)>100)
mbb=mgx[which(abs(mgx$mtime.x-mgx$mtime.y)>100),]
if(nrow(mbb)==0){
  galert('NONE FOUND')
}else{
  for(x in 1:nrow(mbb)){ 
    WriteDate(mbb$lsst[x],mbb$mtime.y[x])
  }
}

mgy=mgg[mgg$dupp,]
strsplit(mgy$lsst,'_|.wmv')
mgy$studio=unlist(strsplit(mgy$lsst,'_|.wmv'))[(1:nrow(mgy))*2]

wrStud(lsst=mgy$lsst,Obn=beaner(mgy$lsst),studio=mgy$studio )
