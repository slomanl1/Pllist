rm(list=ls())
require(bitops)
source("~/Local.R")

if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  
setwd('~/')
load('xxxx.RData')

selectL = select.list(c('AND','OR','NOT'),graphics=TRUE)
print(selectL)
if (length(selectL > 0)) { 
	selector = select.list(wpls,multiple=TRUE,graphics=TRUE)
	if (length(selector) > 0) {
	print(selector)
	bits=0
	a=1:length(wpls)
	for (i in 1:length(selector)){
		b=a[selector[i]==wpls]
		if (!is.na(b)) bits = bitOr(bits,2^(b-1))
	}
	if (selectL == 'AND')
		flist = lsst[bitAnd(xx,bits) == bits]
	else
		if (selectL == 'OR')
			flist = lsst[bitAnd(xx,bits) > 0]
		else #NOT
			flist = lsst[bitAnd(xx,bits) == 0]
	
	flist = flist[flist!='']

	if(length(flist)==0)
		print('No Records Found')
	else {
	  flist = paste(drive,'My Videos/RealPlayer Downloads/',flist,sep='')
    indxs=regexpr('wpl',wpls)[1:length(wpls)]
    fname=""
    for (i in 1:length(wpls)){
      if (bitAnd(bits,2^(i-1))>0)    
        fname = paste(fname,substr(wpls[i],1,indxs[i]-2),sep='_')
      }
	m3uname <- paste(pldrive,'My Playlists/',sep='')
  write(flist,paste(m3uname,selectL,fname,'.M3U',sep='_'))
	}
}
}
} else
  print('No flk')
