cd('~/')
ss='shell("mediainfo %s | findstr /I name >> mediainfo.txt")'
dirsa=dir('D:/PNMTALL',full.names = TRUE)
dirs=select.list(dirsa,multiple = TRUE,graphics = TRUE)

for(dd in dirs)
  if(!is.na(dd)){
    print(paste('SELECTED',dd))
    if(file.exists('mediainfo.txt'))
      file.remove('~/mediainfo.txt')
    
    xx=dir(dd,full.names = TRUE)
    cmds=sprintf(ss,normalizePath(xx,winslash = '/'))
    eval(parse(text=cmds))
    
    tt=readLines('mediainfo.txt')
    tx=data.frame(tt,ff=as.numeric(as.factor(substr(tt,1,15))))
    tx$mn=''
    mvnm=tx[which(grepl('Movie name',tt)),'ff'][1]
    if(!is.na(mvnm)){
      for(i in 1:nrow(tx)){if(tx[i,'ff']==mvnm) tx[i-1,'mn']=as.character(tx[i,'tt'])}
      cc=subset(tx,nchar(mn)>0)[,c('tt','mn')]
      cc$tt=sub('Complete name                            : ','',cc$tt)
      cc$mn=sub('Movie name                               : ','',cc$mn)
      movienames=cc
      
      names(cc)=c('filename','MovieName')
      save(movienames,file=paste('~/MovieNames',basename(dd),'.RData',sep=''))
    }else{
      galert('No Movie Names Found')
    }
  }
