load('~/mfnfo.RData')
cd('D:/PNMTALL/RPDNClips')

source('~/pllist.git/addStudioToDmfnfo.R')
mfnfo=mfnfo[10250:nrow(mfnfo),]  
wrStud(mfnfo$lsst,mfnfo$studio,Obn=beaner(mfnfo$lsst))
