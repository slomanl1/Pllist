source("~/Local.R")
library(tools)
setwd('~/')
if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
rm(list=ls())
source("~/Local.R")
shell('dir "C:\\My Videos\\RealPlayer Downloads" /od/b > ddd.txt')
lns=readLines('ddd.txt')
unlink('ddd.txt')
last=as.numeric(substr(lns[length(lns)],5,9))+1
if(is.na(last))
  last=as.numeric(substr(lns[length(lns)],5,8))+1

setwd('C:\\RealPlayerDownloads')
fns1=dir(pattern='*.mp4')
fns1=fns1[!grepl('.mp4.',fns1)]
exts=c('blah.mp4','bfa.mp4','ussf.mp4','dd.mp4','bl.mp4','ussfd.mp4','pna.mp4','blfd.mp4','utpfd.mp4','ussfa.mp4')
odr=sample(last:last+length(fns1),length(fns1),replace=T) 
if(length(odr)!=0){
load('~/fnfo.RData')
setwd(paste(drive,'My Videos/RealPlayer Downloads',sep=""))
ex=file.exists(rownames(fnfo))
fnfo=fnfo[ex,] #remove non existent files from fnfo
fns=fns1[order(odr)]
fn=fns
setwd('C:\\RealPlayerDownloads')
for (i in 1:length(fns)){
  cc=''
  for (j in 1:length(exts)){
    if (length(grep(exts[j],fns[i]))>0)
      cc=substr(exts[j],1,nchar(exts[j])-4)
  }
  exist=TRUE
  while (exist){
  fn[i]=paste('NEW_',as.character(last),cc,'.wmv',sep='')
  fs=file.info(fns[i])
  if(fs$size %in% fnfo$size &
    identical(md5sum(fns[i]), md5sum(fns[fs$size %in% fnfo$size]))){
      exist=FALSE
      print(c(fns[i],'Dup file found'))
      next
  }else{

      last=last+1
      if(file.exists(paste(drive,"My Videos\\RealPlayer Downloads\\",fn[i],sep=''))){
        print(paste(fn[i],'exists'))
        next
      }else{
          print(paste(fn[i], 'added'))
          file.copy(fns[i],paste(drive,"My Videos\\RealPlayer Downloads\\",fn[i],sep=''))
          exist=FALSE
          }
      }
  }
  unlink(fns[i])
}
source("~/Local.R")
setwd(paste(drive,'My Videos/RealPlayer Downloads',sep=""))
fnfo=rbind(fnfo,file.info(fn))
save(fnfo,file='~/fnfo.RData')
setwd(paste(drive,'My Videos/RealPlayer Downloads',sep=""))

setwd(paste(pldrive,'My Playlists',sep=""))
lss = unique(readLines('wa.wpl'))
js="            <media src=\"Y:\\My Videos\\RealPlayer Downloads\\%s\"/>"
adds=sprintf(js,fn)
lsx1=c(lss[1:(length(lss)-3)],adds,tail(lss,n=3))
m3uname <- paste(pldrive,'My Playlists/',sep='')
write(lsx1,paste(m3uname,'wa.wpl',sep=''))

}else print('No new files found')
} else print('CANNOT OPEN FLK')




