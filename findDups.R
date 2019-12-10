cls=NULL
cls=c(cls,dir(choose.dir(),recursive = TRUE,full.names = TRUE))
sz=file.size(cls)
cfs=data.frame(cls,sz,stringsAsFactors = FALSE)
dps=cfs[cfs$sz %in% sort(dups(cfs$sz)),]
dps$md5s=md5sum((dps$cls))
finq=normalizePath(dps[(!duplicated(dps$md5s)),'cls'],winslash = '/')
print(finq)


