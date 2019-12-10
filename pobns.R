source('~/pllist.git/addStudioToDmfnfo.R')
cd("D:/PNMTALL/RPDNClips")
www=gwindow(height=30)
keep_above(www,TRUE)
shell('start /HIGH /B c:/users/Larry/Documents/getobns.bat')
lndir=len(dir())
mm=c(0,0,0)
while(TRUE){
  mm=shell('grep -c \\.wmv d://obns.txt',intern = TRUE)
  vv=100*as.integer(mm[3])/lndir
  svalue(www)=paste(round(vv,2),'%')
  gprogressbar(vv,www)
  if(as.integer(mm[3])==lndir)
    break
  Sys.sleep(2)
}
dispose(www)
dd=readLines('D:/obns.txt')
dd=dd[!grepl('GPSR',dd)]
cc=data.frame(fn=NA,gps1=NA,mtime=NA)
nf=sum(grepl('========',dd,fixed = TRUE))
load('~/mfnfo.RData')
i=1
j=1
while(j < nf+1){
  cc[j,'fn']=dd[i]
  if(grepl('GPS',dd[i+1])) {
    cc[j,'gps1']=dd[i+1]
    i=i+2
  }else{
    print(paste('Data Error',i))
    i=i+1
  }
  j=j+1
  print(j)
}
cd('D:/PNMTALL/RPDNClips')

cc$bn=beaner(cc$fn)
cc$fn=substr(cc$fn,10,100)
cc$mtime=file.mtime(cc$fn)
pos=unlist(gregexpr('deg',cc$gps1))
cc$Obn=as.integer(substr(cc$gps1,35,pos-2))
nobn=subset(cc,is.na(gps1)&!is.na(fn))
mgg=merge(nobn,mfnfo[,c('bn','studio')],by='bn')
ss=subset(mfnfo,nchar(Obn)==0)
ss$Obn=as.integer(ss$bn)
save(cc,ss,file='c:/Users/Larry/Google Drive/cc.RData')
if(nrow(mgg)>0)
  wrStud(mgg$fn,mgg$studio,Obn=mgg$bn)
if(nrow(ss)>0)
  wrStud(ss$lsst,ss$studio,Obn=ss$Obn)
source('~/pllist.git/Buildmfnfo.R')
load('c:/Users/Larry/Google Drive/cc.RData')
mfnfo=merge(mfnfo,cc[,c('bn','Obn')],by='bn')
mfnfo=mfnfo[,1:14]
names(mfnfo)[14]='Obn'
mfnfo$Obn=as.integer(mfnfo$Obn)
save(mfnfo,wpls,file='~/mfnfo.RData')
source('~/pllist.git/Buildmfnfo.R')

