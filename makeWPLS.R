scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('~/')
load('mfnfo.RData') # load lsst, wpls and xx

makeWpl=function(flist,slctor){
  setwd(paste(pldrive,'My Playlists',sep=""))
  lss = unique(readLines(slctor))
  setwd(paste(pldrive,'My OldPlaylists',sep=""))
  writeLines(lss,slctor) # save old playlist
  setwd(paste(pldrive,'My Playlists',sep=""))
  strt=grep('media',lss)[1]
  fnnh=lss[1:(strt-1)] #wpl header
  fnnt=lss[(length(lss)-2):length(lss)] #wpl footer
  
  js="            <media src=\"c:\\My Videos\\RPDNClips\\%s\"/>"
  adds=sprintf(js,basename(flist))
  lsx1=c(fnnh,adds,fnnt)
  m3uname <- paste(pldrive,'My Playlists/',sep='')
  write(lsx1,paste(m3uname,slctor,sep=''))
}
wpls = sort(dir('c:/my Playlists',pattern = '*.wpl'))
flist1=NULL
for(selector in  wpls){
  print(selector)
  bits=0
  b=which(selector==wpls)
  bits = bitOr(bits,2^(b-1))
  print(bits)
  flisto=flist1
  flist1a = mfnfo[bitAnd(mfnfo$xx,bits) == bits & is.na(mfnfo$md5sn),]$lsst
  flist1=c(flist1a,mfnfo[bitAnd(mfnfo$xx,bits) == bits & !is.na(mfnfo$md5sn),]$nfn)
  if(selector=='wa1.wpl'){
    flist1=flist1[!flist1 %in% flisto]
  }
  flist = paste(drive,'My Videos/RPDNClips/',flist1,sep='')
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
lsst1=mfnfo[is.na(mfnfo$md5sn),]$lsst
lsst=c(lsst1,mfnfo[!is.na(mfnfo$md5sn),]$nfn)
ms=(lsst[!lsst %in% sls]) # missing
unlink('missing.m3u')
if(length(ms)>0)
  writeLines(paste('C:/My Videos/RPDNClips/',ms,sep=''),'missing.M3U')

setwd(paste(drive,'My Videos/RPDNClips',sep=""))
save(mfnfo,wpls,file='~/fnfo.RData')
setwd(paste(pldrive,'My Playlists',sep=""))

###################### orderall ######################
source('~/pllist.git/orderallwpl.R')


