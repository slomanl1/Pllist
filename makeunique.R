scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/mfnfo.RData')

source('~/Local.R') #get drive

lsst=as.character(mfnfo$lsst)
k=0
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
removers = NULL
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
  lns=sub('c:','C:',lss,fixed=TRUE)
  print(wpls[i])
  dupp=dups(toupper(lns)[nchar(lns)>0])
  if(length(dupp)>0){
    print(paste(length(dupp),'dups found in',wpls[i]))
  }
  lnsu=unique(lns)
  if(length(removers)>0)
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
mfnfo=mfnfo[mfnfo$lsst %in% lsst,]
save(mfnfo,wpls,file='~/mfnfo.RData')


