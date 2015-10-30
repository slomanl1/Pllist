scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/fnfo.RData')
source('~/Local.R') #get drive
setwd(paste(drive,'My Videos/RPDNClips',sep=""))
rn=rownames(fnfo)
lsst=fnfo$lsst
addfnfo=file.info(lsst[(!lsst %in% rn)])
if(nrow(addfnfo)>0){
  addfnfo$lsst=NA
  addfnfo$xx=NA
  fnfo=rbind(fnfo,addfnfo)
}
save(fnfo,file='~/fnfo.RData')
#flist=lsst
setwd(paste(pldrive,'My Playlists',sep=""))
fns=dir(pattern='*.wpl')
for (j in 1:length(fns)) {
  print(fns[j])
  fnn=readLines(fns[j])
  strt=grep('media',fnn)[1]
  lss=fnn[strt:(length(fnn)-3)]
  lssx=''
  for(i in 1:length(lss)){
    lssx[i]=substr(lss[i],regexpr('Clips',lss[i])[1]+6,regexpr('mpg|mp4|flv|asf|wmv',lss[i])[1]+2)
  }
  setwd(paste(drive,'My Videos/RPDNClips',sep=""))
  lssy=lssx[!duplicated(lssx) & file.exists(lssx)]
  lss1=lss[!duplicated(lssx) & file.exists(lssx)]
  fnnh=fnn[1:(strt-1)] #wpl header
  fnnt=fnn[(length(fnn)-2):length(fnn)] #wpl footer
  fnfox=fnfo[rownames(fnfo) %in% lssy,]
  fnfoy=file.info(lssy[!(lssy %in% rownames(fnfo))])
  fnfoz=rbind(fnfox,fnfoy)
  fnfoz$fname=rownames(fnfoz)
  lssg=data.frame(fname=lssy,lss1)
  lssj=merge(lssg,fnfoz,by='fname')
  lssz=as.character(lssj[order(lssj$mtime),'lss1'])
  fnno=c(fnnh,sub('..\\My','C:\\My',lssz,fixed = TRUE),fnnt)
  setwd(paste(pldrive,'My Playlists',sep=""))
  writeLines(fnno,fns[j])
}

