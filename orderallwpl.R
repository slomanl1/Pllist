scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/mfnfo.RData')
source('~/Local.R') #get drive
setwd(paste(pldrive,'My Playlists',sep=""))
for (j in 1:length(wpls)) {
  print(wpls[j])
  fnn=readLines(wpls[j])
  strt=grep('media',fnn)[1]
  lss=fnn[strt:(length(fnn)-3)]
  lssx=''
  for(i in 1:length(lss)){
    lssx[i]=substr(lss[i],regexpr('Clips',lss[i])[1]+6,regexpr('mpg|mp4|flv|asf|wmv',lss[i])[1]+2)
  }
  setwd(paste(drive,'My Videos/RPDNClips',sep=""))
  lssy=sub('_','',lssx)
  dxx=data.frame(lsst=lssx,lss)
  lssj=merge(dxx,mfnfo[,c('lsst','mtime')])
  lssz=as.character(lssj[order(lssj$mtime),'lss'])
  fnnh=fnn[1:(strt-1)] #wpl header
  fnnt=fnn[(length(fnn)-2):length(fnn)] #wpl footer
  fnno=c(fnnh,sub('..\\My','C:\\My',lssz,fixed = TRUE),fnnt)
  setwd(paste(pldrive,'My Playlists',sep=""))
  writeLines(fnno,wpls[j])
}

save(mfnfo,wpls,file='~/mfnfo.RData') # update file mtime for chooser time test if mfnfo to date
