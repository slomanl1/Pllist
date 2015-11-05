scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
require(tcltk)
source("~/Local.R")

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  source('~/Pllist/pllist.git/combiner.R')
  print('Combiner Done')
  setwd(paste(drive,'My Videos/RPDNClips',sep=""))
  allall=c(dir(pattern='*.mpg'),dir(pattern='*.wmv'),dir(pattern='*.flv'))
  
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
  for(i in 2:nrow(tl))
  {
    ts[i]=ts[i-1]+tl[i-1,'Freq']
  }
  ts[i+1]=ts[i]
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
  setwd(paste(drive,'My Videos/RPDNClips',sep=""))
  print('Refreshing fnfo')
  fnfo=file.info(as.character(tl$Var1))
  fnfo$xx=xn
  fnfo$lsst=tl$Var1
  save(fnfo,wpls,file='~/fnfo.RData')
  source('~/Pllist/pllist.git/makeunique.R')
  print('ORDERALL')
  source('~/Pllist/pllist.git/orderallwpl.R')
  print('Build Done')
}

