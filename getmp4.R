url='https://www.intporn.org/threads/my-hot-video-collection-exclusive-and-very-hard-to-find-part-2.1031152/page-%d'
za=NULL
i=10
zz=''
while(len(zz)>0 | i==10) 
{

  urr=sprintf(url,i);
  xx=shell(paste('curl',urr),intern = TRUE)
  zz=fi('.mp4',xx)
  zz=zz[!grepl('nitro|html|FHD',zz)]
  rdate=head(fi('Release Date',xx),1)
  print(paste(i,rdate))
  za=c(rdate,za,zz)
  i=i+1
}

zal=fi('.mp4',unlist(strsplit(za,'/|target')))

zx=substr(zal,1,nchar(zal)-2) # mp4 file names
ss=unique(sub(' HD','',zx))
i=i-2
save(za,ss,i,file='~/intprnmvs.RData')
