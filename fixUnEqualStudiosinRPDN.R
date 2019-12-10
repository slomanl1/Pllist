load('~/dfan.RData')
xx=which(sapply(1:nrow(dfan),function(x) grepl(dfan[x,'studio'],dfan[x,'filename'],fixed=TRUE))==0)
dx=dfan[xx,]
dx=subset(dx,grepl('RPDNC',filename))
dx$bn=as.integer(gsub('[a-z|A-Z|_|-]','',as.character(basename(dx$filename))))
pos=regexpr('_',as.character(dx$filename))
dx$stud=substr(dx$filename,pos+1,nchar(dx$filename)-4)
if(nrow(dx)){
  dx[is.na(dx$DMComment),'DMComment']=''
  wrStud(dx$filename,dx$stud,dx$DMComment)
}else{
  galert('NONE FOUND')
  
}

