scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
setwd('~/')
load('~/mfnfo.RData') # load lsst, wpls and xx

makeWpl=function(flist,slctor){
  setwd(paste(pldrive,'My Playlists',sep=""))
  load('~/headFoot.RData')
  header=sub('fns',file_path_sans_ext(slctor),header)
  header=sub('3611',len(flist),header)
  js="            <media src=\"c:\\MyVideos\\RPDNClips\\%s\"/>"
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
  bits = bitOr(bits,2^(b-1))
  print(bits)
  flisto=flist1
  flist1 = mfnfo[bitAnd(mfnfo$xx,bits) == bits,]$lsst
  if(selector=='wa1.wpl'){
    flist1=flist1[!flist1 %in% flisto]
  }
  flist = paste(drive,'MyVideos/RPDNClips/',flist1,sep='')
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
ms=(lsst[!lsst %in% sls]) # missing
unlink('c:/my playlists/missing.M3U')
if(length(ms)>0)
  writeLines(paste('C:/MyVideos/RPDNClips/',ms,sep=''),'c:/my playlists/missing.M3U')

setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
save(mfnfo,wpls,file='~/mfnfo.RData')
setwd(paste(pldrive,'My Playlists',sep=""))

###################### orderall ######################
source('~/pllist.git/orderallwpl.R')


