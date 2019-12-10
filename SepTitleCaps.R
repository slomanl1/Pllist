source('~/.RProfile') # required for standalone version
len=function(x) length(x)
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
load('~/dfan.rdata')
ddn=subset(dfan,is.na(Title) &!grepl('RPDNClips',filename))
dd=ddn$filename
ee=strsplit(dd,'-|RATE')
tt=sapply(1:len(dd),function(x) ee[[x]][2])
zz=unlist(strsplit(tt[grepl('and[A-Z]',tt)],'and'))
ax=zz[seq(1,16,2)] # ands 1st word
ay=zz[seq(2,16,2)] # ands 2nd word
poss=gregexpr('[a-z][A-Z]',tt)
ttl=NA
for( i in 1:len(dd)){
  poss[[i]]=c(0,poss[[i]],1000)
  ttl[i]=''
  for(pp in 1:(len(poss[[i]])-1)){
    ttl[i]=paste(ttl[i],(substr(tt[i],1+poss[[i]][pp],poss[[i]][pp+1])))
  }
}

plss=gregexpr('+',ttl,fixed=TRUE)
lastplus=sapply(1:len(ttl),function(x) tail(plss[[x]],1))

fvss=gregexpr('fav',ttl)
lastfav=sapply(1:len(ttl),function(x) tail(fvss[[x]],1))
dx=data.frame(dd,lastfav,lastplus,ttl)
dx$pmx=pmax(dx$lastfav,dx$lastplus)
dx$txl=substr(dx$ttl,dx$pmx,1000)
dx$txl=gsub('fav','',dx$txl)
dx$txl=gsub('+','',dx$txl,fixed=TRUE)
dx$txl=gsub('TRIM','',dx$txl,fixed=TRUE)
spp=gregexpr(' ',trim(dx$txl))
px=sapply(1:len(spp), function(x) len(spp[[x]]))
dx$px=px
dx$pxx=spp
dx$py=as.character(dx$pxx)!='-1'
dx=dx[dx$py & dx$txl!=' NA NA',]
dx$fdate=file.mtime(as.character(dx$dd))
errss=NULL
if(file.exists("~/writeErrorLog.txt")){
  errss=readLines("~/writeErrorLog.txt")
}

dx=dx[which(!file_path_sans_ext(dx$dd) %in% file_path_sans_ext(errss)),]
if(nrow(dx)==0){
  print('None Found')
  galert('None Found')
}else{
  wrStud(dx$dd,studio=NA, Title=dx$txl)
}


