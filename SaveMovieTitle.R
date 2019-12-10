load('~/mfnfo.rdata')
nmsa=dir('~/',pattern='MovieNames',full.names = TRUE)
nms=select.list(nmsa,multiple = TRUE,graphics = TRUE)

movienamesa=movienames[0,]
for (nm in nms){
  print(nm)
  load(nm)
  movienamesa=rbind(movienamesa,movienames)
}

movienamesa$tb=basename(movienamesa$tt)
mgg=merge(mfnfo,movienamesa,by.x='lsst',by.y='tb')
mgg$mn=sub("'",'',mgg$mn)
mgg$mn=gsub(':','',mgg$mn)
mgg$mn=sub("'",'',mgg$mn)
mgg$mn=sub(":",'',mgg$mn)
mgg$mn=sub("/:",'',mgg$mn)
mgg$mn=sub("/:",'',mgg$mn)
mgg$mn=sub("/:",'',mgg$mn)
mgg$mn=gsub("/:",'',mgg$mn)
mnx=substr(mgg[grepl('Mari0',mgg$mn),'mn'],9,1000)
mgg[grepl('Mari0',mgg$mn),'mn']=mnx
save(mgg,file='~/clpsmovie.RData')
wrStud(lsst=mgg$tt,studio=mgg$studio, dmComment=mgg$cmt,Title=mgg$mn)

