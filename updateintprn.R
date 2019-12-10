cd('C:\\Users\\Larry\\AppData\\Local\\Google\\Chrome\\User Data\\Default')
load('~/intprnmvs.RData')

url='https://www.intporn.org/threads/my-hot-video-collection-exclusive-and-very-hard-to-find-part-2.1031152/page-%d'

za=NULL
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
i=i-2
save(za,ss,i,file='~/intprnmvs.RData')
bb=readLines('Bookmarks')
ww=which(grepl('intporn',bb,ignore.case = TRUE))
gl=gregexpr('Page',bb[ww],ignore.case = TRUE)

for(j in 1:len(gl)){
  poss=gl[[j]][1]
  nn=strtoi(substr(bb[ww[j]],poss+5,poss+7))
  if(is.na(nn)| nn < i){
    nn=strtoi(substr(bb[ww[j]],poss+5,poss+8))
    if(is.na(nn))
      next
  }
  bb[ww[j]]=sub(nn,i,bb[ww[j]])
}

writeLines(bb,'Bookmarks')

