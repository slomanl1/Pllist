source("~/Local.R")
library(tools)
setwd('~/')
if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
  source(scriptStatsRemoveAll) #clear bones
  source("~/Local.R")
  shell('dir "C:\\My Videos\\RPDNClips" /od/b > ddd.txt')
  lns=readLines('ddd.txt')
  unlink('ddd.txt')
  bn=gsub('[a-z|A-Z|_]','',lns)
  last=tail(sort(as.integer(bn)),1)
  
  setwd('C:\\RealPlayerDownloads')
  fns1=dir(pattern='*.mp4')
  fns1=fns1[!grepl('.mp4.',fns1)]
  exts=c('blah.mp4','bfa.mp4','ussf.mp4','dd.mp4','bl.mp4','ussfd.mp4','pna.mp4','blfd.mp4','utpfd.mp4','ussfa.mp4','uwa.mp4')
  odr=sample(last:last+length(fns1),length(fns1),replace=T) 
  if(length(odr)!=0){
    load('~/mfnfo.RData')
    setwd(paste(drive,'My Videos/RPDNClips',sep=""))
    ex=file.exists(mfnfo$lsst)
    exn=file.exists(paste(file_path_sans_ext(mfnfo$lsst),'_New.wmv',sep=''))
    mfnfo=mfnfo[ex|exn,] #remove non existent files from fnfo
    fns=fns1[order(odr)]
    fn=fns
    setwd('C:\\RealPlayerDownloads')
    last=last+1
    for (i in 1:length(fns)){
      cc=''
      for (j in 1:length(exts)){
        if (grepl(exts[j],fns[i])){
          cc=substr(exts[j],1,nchar(exts[j])-4)}
      }
      exist=TRUE
      while (exist){
        fn[i]=paste(as.character(last),cc,'.wmv',sep='')
        fs=file.info(fns[i])
        if(fs$size %in% mfnfo$size &
           identical(md5sum(fns[i]), md5sum(fns[fs$size %in% mfnfo$size]))){
          exist=FALSE
          print(c(fns[i],'Dup file found'))
          next
        }else{
          
          last=last+1
          if(file.exists(paste(drive,"My Videos\\RPDNClips\\",fn[i],sep=''))){
            print(paste(fn[i],'exists'))
            next
          }else{
            print(paste(fn[i], 'added'))
            file.copy(fns[i],paste(drive,"My Videos\\RPDNClips\\",fn[i],sep=''))
            exist=FALSE
          }
        }
      }
      unlink(fns[i])
    }
    source("~/Local.R")
    setwd(paste(pldrive,'My Playlists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    setwd(paste(drive,'My Videos/RPDNClips',sep=""))
    addfnfo=file.info(fn)
    addfnfo$lsst=fn
    addfnfo$xx=2^(length(wpls)-1) # wa1.wpl
    addfnfo$md5s=md5sum(fn)
    addfnfo$nfn=paste(file_path_sans_ext(fn),'_New.wmv',sep='')
    addfnfo$md5sn=NA
    mfnfo=rbind(mfnfo,addfnfo)
    save(mfnfo,wpls,file='~/mfnfo.RData')
    setwd(paste(pldrive,'My Playlists',sep=""))
    lss = unique(readLines('wa1.wpl'))
    js="            <media src=\"c:\\My Videos\\RPDNClips\\%s\"/>"
    adds=sprintf(js,fn)
    lsx1=c(lss[1:(length(lss)-3)],adds,tail(lss,n=3))
    m3uname <- paste(pldrive,'My Playlists/',sep='')
    write(lsx1,paste(m3uname,'wa1.wpl',sep='')) # add new clips to wa1.wpl
    lsx2=c(lss[1:15],adds,tail(lss,n=3))
    write(lsx2,paste('C:/My Videos/','fns.wpl',sep=''))
    shell('wmplayer "c:\\My Videos\\fns.wpl')
    unlink('c:/my Videos/fns.wpl')
    
  }else print('No new files found')
} else print('CANNOT OPEN FLK')




