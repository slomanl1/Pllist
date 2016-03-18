scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
require(tcltk)
source("~/Local.R")

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  source('~/pllist.git/combiner.R')
  print('Combiner Done')
  setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
  allall=c(dir(pattern='*.mpg'),dir(pattern='*.wmv'))
  
  setwd(paste(pldrive,'My Playlists',sep=""))
  wpls = sort(dir(pattern = '*.wpl'))
  
  lns  = NA
  xx=NULL
  lsst=NULL
  dx=data.frame(lss='',xx=0,wpl='')[0,] # create empty data frame structure
  
  for (i in 1:length(wpls)) {
    lss1=unique(readLines(wpls[i]))
    lss=NULL
    for(j in 1:length(lss1)){
      lss[j]=substr(lss1[j],regexpr('Clips',lss1[j])[1]+6,regexpr('mpg|wmv',lss1[j])[1]+2)
    }
    dx=rbind(dx,data.frame(lss=lss,xx=2^(i-1),wpl=wpls[i],stringsAsFactors = FALSE))
  }
  dxu=dx[nchar(dx$lss)>0,]
  print('build dx Done')
  dx=dxu[order(as.numeric(gsub('[a-z]|_','',dxu$lss,ignore.case = TRUE))),]
  tl1=data.frame(table(dx$lss))
  tl=tl1[order(as.numeric(gsub('[a-z]|_','',tl1$Var1,ignore.case = TRUE))),]
  ts=1
  for(i in 2:(nrow(tl)+1))
  {
    ts[i]=ts[i-1]+tl[i-1,'Freq']
  }
  #ts[i+1]=ts[i]
  print('Build ts Done')
  xo=sapply(1:(nrow(tl)), function(x) dx[ts[x]:(ts[x+1]-1),'xx'])
  xn=sapply(1:(nrow(tl)), function(x) {
    xx=unlist(xo[[x]])
    xy=0
    for (i in 1:len(xx)) 
      xy=bitOr(xy,xx[i]) 
    return(xy)
  }) 
  print('Build xo/xn Done')
  tl$xx=xn
  tl$lsst=sub('_REN','',tl$Var1)
  setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
  print('Refreshing mfnfo')
  load('~/mfnfo.RData')
  setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
  mg1=merge(mfnfo,tl[,c('Var1','xx')],by.x='lsst',by.y='Var1')
  mg1$xx=mg1$xx.y
  mfnfo=mg1[,names(mfnfo)]
  rn1=as.character(mfnfo$lsst)
  rn=rn1[file.exists(rn1)]
  fn=allall[(!allall %in% rn)]
  addfnfo=file.info(fn)
  if(nrow(addfnfo)>0){
    addfnfo$lsst=sub('REN_New','',fn)
    addfnfo$xx=0 # not in any playlist
    addfnfo$md5s=md5sum(addfnfo$lsst)
    addfnfo=addfnfo[,names(mfnfo)]
  # if file renamed just replace xx with xx of old file before rename(otherwise it was added)
    for(i in 1:nrow(addfnfo)){
      newxx=mfnfo[which(mfnfo$md5s==addfnfo[i,'md5s']),'xx']
      if(len(newxx)==0)
        next
      addfnfo[i,'xx']=newxx
    }
    mfnfo=rbind(mfnfo,addfnfo)
    tod=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    lastr=nrow(mfnfo)
    mfnfo[(lastr-len(fn)+1):lastr,c(7,8,9)]=tod
    save(mfnfo,wpls,file='~/mfnfo.RData')
    print(paste(nrow(addfnfo),'records added in addfnfo'))
    source('~/pllist.git/makeWPLS.R')
  }else{
    save(mfnfo,wpls,file='~/mfnfo.RData')
  }
  source('~/pllist.git/makeunique.R')
  print('ORDERALL')
  source('~/pllist.git/orderallwpl.R')
  print('Build Done')
}

