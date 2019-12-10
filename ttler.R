library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/rmmovname.R') # hasMovieName()

cd('D:/PNMTALL/RPDNClips')
tt=system('exiftool *.* -Title',intern = TRUE)
save(tt,file='~/ttl.rdata')
load('~/ttl.rdata')
load('~/dfan.Rdata')

fn=NULL
ttl=NULL
i=1
while(i<len(tt)){
  if(grepl('===',tt[i])){
    fn[i]=substr(tt[i],10,1000)
  }else{
    ttl[i-1]=substr(tt[i],35,100)
  }
  i=i+1  
}
xx=data.frame(fn,ttl,stringsAsFactors = FALSE)
xx=subset(xx,!is.na(xx$fn))
cmm=xx[which(substr(xx$ttl,nchar(xx$ttl),nchar(xx$ttl)+1)==','),]
cmm$ncm=substr(cmm$ttl,1,nchar(cmm$ttl)-1)
dfan$base=basename(dfan$filename)
mgg=merge(cmm,dfan,by.x='fn',by.y='base')
poss=unlist(gregexpr('_',mgg$fn))
possp=unlist(gregexpr('.',mgg$fn,fixed = TRUE))
stds=substr(mgg$fn,poss+1,possp-1)
mgg$studio=stds
if(nrow(mgg)>0){
mgg[nchar(mgg$ncm)==0,'ncm']='None'
wrStud(mgg$fn,mgg$studio,NA,Title = mgg$ncm)
}else{
  galert('None Found')
}


