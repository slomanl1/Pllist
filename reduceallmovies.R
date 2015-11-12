cd('~/')
cls=NULL
cls=c(dir('D:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/PNMTALL',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/my videos/rpdnclips',recursive = TRUE,full.names = TRUE))
cls=c(cls,dir('c:/Users/LarrySloman/Downloads',recursive = TRUE,full.names = TRUE))
cls=cls[which(!grepl('_New',cls))]
dfa=data.frame(cls,sz=file.size(cls))
dfa=dfa[order(dfa$sz,decreasing=TRUE),]
writeLines('','~/msgis.txt')
for( fn in dfa$cls)
{
  if(file.exists(fn) & file.size(fn)==dfa[which(dfa$cls==fn),'sz']){
    if(file.exists('~/out.mp4')){
      file.remove('~/out.mp4')
    }
    print(fn)
    msgi=shell(paste('c:/users/LarrySloman/converth265.bat "',
                fn,'" c:/users/LarrySloman/Documents/out.mp4',sep=''),translate = TRUE,intern=TRUE)
    print(tail(msgi))
    nfn=paste(file_path_sans_ext(fn),'_New.',file_ext(fn),sep='')
    file.copy('~/out.mp4',nfn)
    writeLines(msgi,'~/temp.txt')
    file.append('~/msgis.txt','~/temp.txt')

    print(nfn)
  }
}

