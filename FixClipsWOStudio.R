source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
cd('~/')
load('dirtbl.rdata')
load('mfnfo.RData')
mm=subset(mfnfo,(nchar(studio)==0 |is.na(studio)|grepl('Image',studio)))
if(nrow(mm)>0){
  cd('D:/pnmtall/RPDNClips')
  mx=mm[,c('lsst','studio')]
  ofn=mx$lsst
  poss=unlist(gregexpr('_',mx$lsst))
  stud=sapply(1:len(poss), function(x){substr(mx[x,'lsst'],poss[x]+1,-4+nchar(mx[x,'lsst']))})
  wrStud(mx$lsst,stud) 
  source('~/Pllist.git/Buildmfnfo.R')
}else{
  galert('NO FILES FOUND')
}
