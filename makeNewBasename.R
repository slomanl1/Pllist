load('~/TTare mnfo entries with nowpls in basename')
flist1=NULL
for(selector in  wpls){
  print(selector)
  bits=0
  b=which(selector==wpls)
  bits = bitwOr(bits,2^(b-1))
  print(bits)
  flist1 = tt[bitwAnd(tt$xx,bits) == bits,]$lsst
  tt[!tt$lsst %in% flist1,b+2]=''
  tt[tt$lsst %in% flist1,b+2]=sub('.wpl','',wpls)[b]
}
colnames(tt)=c('lsst','xx',sub('.wpl','',wpls))
to=tt
to$ext=''
for(i in 1:nrow(to))
  to[i,'ext']=gsub(' ','',trim(capture.output(cat(paste(to[i,3:22])))))
load('~/TTare mnfo entries with nowpls in basename')
save(to,tt,file='~/ToTT.RData')
rm(tt)
j=matrix(NA,11,3)

j[1,]=strsplit('b,bfa,bfa',',')[[1]]
j[2,]=strsplit('bl,blah,blah',',')[[1]]
j[3,]=strsplit('sfall,ussf,ussf',',')[[1]]
j[4,]=strsplit('sfall,ussfa,ussfa',',')[[1]]
j[5,]=strsplit('sfall,ussfd,ussfd',',')[[1]]
j[6,]=strsplit('ussf,ussfa,ussfa',',')[[1]]
j[7,]=strsplit('ussf,ussfd,ussfd',',')[[1]]
j[8,]=strsplit('utp,utpfd,utpfd',',')[[1]]
j[9,]=strsplit('utp,stp,utp',',')[[1]]
j[10,]=strsplit('stp,utpfd,utpfd',',')[[1]]
j[11,]=strsplit('swa,uwa,uwa',',')[[1]]

xo=to
for(i in 1:nrow(j)){
 ixs=which(grepl(j[i,1],xo$ext) & grepl(j[i,2],xo$ext))
 for(ix in ixs){
  xo[ix,'ext']=(sub(j[i,1],'',xo[ix,'ext']))
  print(paste(i,ix,xo[ix,'ext']))
 }
}

xo[which(grepl('fd',xo$ext) &!grepl('ussfd',xo$ext)),'ext']=
  sub('fd','utpfd',xo[which(grepl('fd',xo$ext) &!grepl('ussfd',xo$ext)),'ext'])


