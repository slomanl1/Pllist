scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/cleanwplsfn.R')
load('~/mfnfo.RData') # load lsst, wpls and xx
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))

sapply(1:nrow(mfnfo), function (x) {
  fy=cleanwplsfn(mfnfo[x,'lsst'])
  print(paste(mfnfo[x,'lsst'],trim(fy$fy)))
})

mfnfo$wpl=''
for(b in 1:len(wpls)){
  x1=bitwAnd(mfnfo$xx ,2^(b-1))
  mfnfo$wpl=paste(mfnfo$wpl,ifelse(x1,file_path_sans_ext(wpls)[b],''),sep='')
}

for( wp in mfnfo$wpl){
   mfnfo[which(wp==mfnfo$wpl),'wpl']=cleanwplsfn(wp)
 } 
