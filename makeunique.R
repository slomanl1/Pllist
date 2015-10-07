rm(list=ls())
load('~/xxxx.RData')

source('~/Local.R') #get drive

flist=lsst
#rm(xx,lsst,wpls)
k=0
setwd(paste(drive,'My Videos/RPDNClips',sep=""))
removers = ""
for (i in 1:length(flist)) {
  if (!file.exists(flist[i])) {
    k=k+1
    removers[k] = flist[i]
    print(c('removers',flist[i]))
  }
}

setwd(paste(pldrive,'My Playlists',sep=""))
fns=dir(pattern='*.wpl')
for (i in 1:length(fns)) {
  lss=readLines(fns[i])
  lns=lss
  print(fns[i])
  dups=lns[duplicated(lns[nchar(lns)>0])]
  if(length(dups)>0)
     print(dups)
  lnsu=unique(lns)
  for (j in 1:length(removers)){
    if(nchar(removers[j])>0){
      lnsu[grep(removers[j],lnsu,fixed=TRUE)] <- NA
      lnsu=lnsu[!is.na(lnsu)]
      lsst[grep(removers[j],lsst,fixed=TRUE)] <- NA
      lsst=lsst[!is.na(lsst)]
    }
  }
 
  writeLines(lnsu,fns[i])
}

setwd("~/")
save(xx,lsst,wpls,removers,file='xxxx.RData')
removers
