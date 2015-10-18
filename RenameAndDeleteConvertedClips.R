setwd('c:/My Videos/RPDNClips')
cls=dir('c:/My Videos/RPDNClips')
clt=subset(cls,grepl('_New',cls))
clo=sub('_New','',clt)
unlink(clo)
file.rename(clt,clo)
clb=sub('_New.mov','',clt)
clx=cls[which(substr(cls,1,regexpr('.',cls,fixed=TRUE)-1) %in% clb)]


