cls=c(dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/my videos/rpdnclips',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/Users/LarrySloman/Downloads',recursive = TRUE,full.names = TRUE))
clt=subset(cls,grepl('_New',cls))
clo=sub('_New','',clt)
clb=sub('_New.mov','',clt)
clx=cls[which(substr(cls,1,regexpr('.',cls,fixed=TRUE)-1) %in% clb)]
if(len(clx)>0){
  cat(paste('\n',clo[file.exists(clo)],'size',file.size(clo[file.exists(clo)]),'EXISTS\n'))
  answ=gconfirm('Are you Sure?')
  if(answ){
    unlink(clo[file.exists(clo)])
    file.rename(clt[!file.exists(clo)],clo[!file.exists(clo)])
  }
}else
  print('None Found')
