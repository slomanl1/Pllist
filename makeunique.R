scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/mfnfo.RData')

source('~/Local.R') #get drive

lsst=c(mfnfo[is.na(mfnfo$md5sn),'lsst'],mfnfo[!is.na(mfnfo$md5sn),'nfn'])
k=0
setwd(paste(drive,'My Videos/RPDNClips',sep=""))
removers = ""
for (i in 1:length(lsst)) {
  if (!file.exists(lsst[i])) {
    k=k+1
    removers[k] = lsst[i]
    print(c('removers',lsst[i]))
  }
}

setwd(paste(pldrive,'My Playlists',sep=""))

for (i in 1:length(wpls)) {
  lss=readLines(wpls[i])
  lns=lss
  print(wpls[i])
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
  
  writeLines(lnsu,wpls[i])
}

setwd("~/")
mfnfo=rbind(mfnfo[mfnfo$lsst %in% lsst,],mfnfo[mfnfo$nfn %in% lsst,])
save(mfnfo,wpls,file='~/mfnfo.RData')
print(paste('Removed -',removers))

