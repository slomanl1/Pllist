rm(list=ls())
require(bitops)
require(tcltk)
source("~/Local.R")

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {

source('~/Pllist/combiner.R')
setwd(paste(drive,'My Videos/RealPlayer Downloads',sep=""))
allall=c(dir(pattern='*.mpg'),dir(pattern='*.wmv'),dir(pattern='*.flv'))

setwd(paste(pldrive,'My Playlists',sep=""))
wpls = dir(pattern = '*.wpl')

lss  = ""
lns  = NA
lnsw = NA

for (i in 1:length(wpls)) {
	lnst=unique(readLines(wpls[i]))
	lss = c(lss,lnst)
	lnsw = c(lnsw,array(i,length(lnst)))
}




for(i in 1:length(lss)){
	if (length(grep('mpg',lss[i])>0))
		lss[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('mpg',lss[i])[1]+2)
	else
		lss[i]=substr(lss[i],regexpr('loads',lss[i])[1]+6,regexpr('wmv',lss[i])[1]+2)

}

lnst = unique(allall)
lss = c(lss,lnst)
lnsw = c(lnsw,array(length(wpls)+1,length(lnst)))
lnswp = 2^(lnsw-1)
filtt = grep('wmv',lss)
filtc = grep('mpg',lss)
filtd = grep('flv',lss)
lsst = unique(c(lss[filtt],lss[filtc],lss[filtd]))
nclsst=nchar(lsst)
print('Phase 1 done')
xx=array(0,dim=length(lsst))
pb = winProgressBar(title = "R progress bar", label = "",
               min = 0, max = length(lsst), initial = 0, width = 300)
shell('nircmd.exe win activate "rsession"')
for (i in 1:length(lsst)) {
  if(i %% 1000 == 0)
    setWinProgressBar(pb, i, title = paste('R progress bar',round(100*i/length(lsst),0),'%'), label = NULL)
	founds = (lsst[i] == lss)
  fnds=lss[founds]
  fndwp=lnswp[founds]
  ncfnds=nchar(fnds)
  nclssti=nclsst[i]
  lnf=length(fnds)
	for (j in 1: lnf) {
		if (nclssti == ncfnds[j])
 			xx[i] = xx[i] + fndwp[j]
       }
}
setWinProgressBar(pb, i, title = paste('R progress bar','Finalizing...', label = NULL))
xx <- bitOr(xx,2^(length(wpls)+1))
setwd("~/")
save(xx,lsst,wpls,file='xxxx.RData')
print('MAKEUNIQUE')
source('~/Pllist/makeunique.R')
print('ORDERALL')
source('~/Pllist/orderallwpl.R')


} else print('CANNOT OPEN FLK')


