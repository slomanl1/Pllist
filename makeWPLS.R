scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd('~/')
ppp=galert('BUILDING PLAYLISTS',delay=60,x=500)
source('~/pllist.git/cleanwplsfn.R')
source('~/pllist.git/EnterStartStop.R')
load('~/mfnfo.RData') # load lsst, wpls and xx

makeWpl=function(flist,slctor){
  setwd(paste(pldrive,'My Playlists',sep=""))
  load('~/headFoot.RData')
  header=sub('fns',file_path_sans_ext(slctor),header)
  header=sub('3611',len(flist),header)
  js="            <media src=\"D:\\PNMTALL\\RPDNClips\\%s\"/>"
  adds=unique(sprintf(js,basename(flist)))
  lsx1=c(header,adds,footer)
  m3uname <- paste(pldrive,'My Playlists/',sep='')
  write(lsx1,paste(m3uname,slctor,sep=''))
}

flist1=NULL
for(selector in  wpls){
  print(selector)
  bits=0
  b=which(selector==wpls)
  bits = bitwOr(bits,2^(b-1))
  print(bits)
  flisto=flist1
  flist1 = mfnfo[bitwAnd(mfnfo$xx,bits) == bits,]$lsst
  if(selector=='wa1.wpl'){
    flist1=flist1[!flist1 %in% flisto]
  }
  flist = paste(drive,'PNMTALL/RPDNClips/',flist1,sep='')
  m3uname <- paste(pldrive,'My Playlists/',sep='')
  fname=file_path_sans_ext(selector)
  makeWpl(flist, selector)
  dx=data.frame(fn=basename(flist),selector=substr(selector,1,nchar(selector)-4),stringsAsFactors=FALSE)
  if(!exists('tmdf')){
    tmdf=dx
  }else{
    tmdf=rbind(tmdf,dx)
  }
}

tdf=tmdf[order(tmdf$fn),]
tbl=data.frame(table(tdf$fn))$Freq
sls=unique(tdf$fn)

tt=tbl[1]
for(i in 2:len(tbl)) # build index table
  tt[i]=tt[i-1]+tbl[i]

slss=capture.output(cat(tdf[1:(tt[1]),'selector']))
for( i in 2:len(sls)){
  slss[i]=capture.output(cat(tdf[tt[i-1]:(tt[i]-1),'selector']))
}
lsst=mfnfo$lsst
ms=na.omit(lsst[!lsst %in% sls]) # missing
if (file.exists('c:/my playlists/missing.M3U'))
  file.remove('c:/my playlists/missing.M3U')

if(len(ms)>0){
  if(!is.na(ms)){
    writeLines(paste('D:/PNMTALL/RPDNClips/',ms,sep=''),'c:/my playlists/missing.M3U')
    ppkn=readLines('c:/my playlists/missing.M3U')
    bits=1048575L
    flist1 = paste('D:/PNMTALL/RPDNClips/',mfnfo$lsst[bitwAnd(mfnfo$xx,bits) == 0],sep='') # wa1.wpl and wa.wpl only to add to ppk
    ppkn=unique(c(ppkn,flist1))
    ppk=sapply(1:len(ppkn), function(x) paste(ppkn[x],' ',cleanwplsfn(ppkn[x])$fx,sep=''))
    save(ppk,file='~/ppk.RData')
  }
}

setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
save(mfnfo,wpls,file='~/mfnfo.RData')
setwd(paste(pldrive,'My Playlists',sep=""))

###################### orderall ######################
removers=NULL
source('~/pllist.git/orderallwpl.R')
if(exists('ppp')){
  if(isExtant(ppp))
    dispose(ppp)
}
