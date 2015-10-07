rm(list=ls())
load('~/xxxx.RData')
load('~/fnfo.RData')
source('~/Local.R') #get drive
setwd(paste(drive,'My Videos/RPDNClips',sep=""))
rn=rownames(fnfo)
addfnfo=file.info(lsst[(!lsst %in% rn)])
fnfo=rbind(fnfo,addfnfo)
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
    if (length(grep('mpg',lss[i])>0)) {
      lssx[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('mpg',lss[i])[1]+2)
      }
    if (length(grep('wmv',lss[i])>0)){
      lssx[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('wmv',lss[i])[1]+2)
    }
    if (length(grep('asf',lss[i])>0)){
      lssx[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('asf',lss[i])[1]+2)
    }
    if (length(grep('flv',lss[i])>0)){
      lssx[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('flv',lss[i])[1]+2)
    }
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
  fnno1=c(fnnh,lssz,fnnt)
  fnno=sub('Y:','C:',fnno1)
  setwd(paste(pldrive,'My Playlists',sep=""))
  writeLines(fnno,fns[j])
}

