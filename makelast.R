source("~/Local.R")
library(tools)
setwd('~/')
if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
  scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
  source(scriptStatsRemoveAll) #clear bones
  source("~/Local.R")
  fn=NULL
  shell('dir "C:\\MyVideos\\RPDNClips" /od/b > ddd.txt')
  lns=readLines('ddd.txt')
  unlink('ddd.txt')
  bn=gsub('[a-z|A-Z|_]','',lns)
  last=tail(sort(as.integer(bn)),1)
  
  setwd('C:\\RealPlayerDownloads')
  fns1=dir(pattern='*.mp4|*.mov')
  fns1=fns1[!(grepl('.mp4',fns1)&(grepl('.mov',fns1)))]
  exts1=c('ah','blah.mp4','bfa.mp4','ussf.mp4','dd.mp4','bl.mp4','cs.mp4','ussfd.mp4','pn.mp4','blfd.mp4','utpfd.mp4','ussfa.mp4','uwa.mp4','utp.mp4')
  exts=gsub('.mp4','',exts1,fixed=TRUE)
  odr=sample(last:last+length(fns1),length(fns1),replace=T) 
  if(length(odr)!=0){
    load('~/mfnfo.RData')
    setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
    ex=file.exists(mfnfo$lsst)
    mfnfo=mfnfo[ex,] #remove non existent files from fnfo
    fns=fns1[order(odr)]
    fn=fns
    setwd('C:\\RealPlayerDownloads')
    last=last+1
    for (i in 1:length(fns)){
      cc=capture.output(cat(sort(exts[unlist(sapply(exts,function(x) gregexpr(x,fns[i])))>0]),sep=''))
      for(x in 1:len(exts))
        cc=sub(paste(exts[x],exts[x],sep=''),exts[x],cc)
      exist=TRUE
      ren=ifelse(any(grepl('_REN',mfnfo$lsst)),'_REN','')
      while (exist){
        fn[i]=paste(as.character(last),cc,ren,'.wmv',sep='')
        fs=file.info(fns[i])
        if(fs$size %in% mfnfo$size &
           identical(md5sum(fns[i]), md5sum(fns[fs$size %in% mfnfo$size]))){
          exist=FALSE
          print(c(fns[i],'Dup file found'))
          next
        }else{
          
          last=last+1
          if(file.exists(paste(drive,"MyVideos\\RPDNClips\\",fn[i],sep=''))){
            print(paste(fn[i],'exists'))
            next
          }else{
            print(paste(fn[i], 'added to RPDNClips'))
            file.copy(fns[i],paste(drive,"MyVideos\\RPDNClips\\",fn[i],sep=''))
            exist=FALSE
          }
        }
      }
      unlink(fns[i])
    }
    setwd(paste(pldrive,'My Playlists',sep=""))
    wpls = sort(dir(pattern = '*.wpl'))
    setwd(paste(drive,'MyVideos/RPDNClips',sep=""))
    addfnfo=file.info(fn)
    addfnfo$lsst=fn
    addfnfo$xx=2^(length(wpls)-1) # wa1.wpl
    addfnfo$md5s=md5sum(fn)
    mfnfo=rbind(mfnfo,addfnfo)
    tod=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    lastr=nrow(mfnfo)
    mfnfo[(lastr-len(fn)+1):lastr,c(7,8,9)]=tod
    save(mfnfo,wpls,file='~/mfnfo.RData')
    setwd(paste(pldrive,'My Playlists',sep=""))
    lss = unique(readLines('wa1.wpl'))
    js="            <media src=\"c:\\MyVideos\\RPDNClips\\%s\"/>"
    adds=sprintf(js,fn)
    lsx1=c(lss[1:(length(lss)-3)],adds,tail(lss,n=3))
    m3uname <- paste(pldrive,'My Playlists/',sep='')
    write(lsx1,paste(m3uname,'wa1.wpl',sep='')) # add new clips to wa1.wpl
    lsx2=c(lss[1:(which(trim(lss)=='<seq>')[1])],adds,tail(lss,n=3))
    write(lsx2,paste('C:/MyVideos/','fns.wpl',sep=''))
    shell('wmplayer "c:\\MyVideos\\fns.wpl')
    unlink('c:/MyVideos/fns.wpl')
    
  }else print('No new files found')
} else print('CANNOT OPEN FLK')

setwd(paste(pldrive,'My Playlists',sep=""))
for(fnx in fn){
  exx=exts[unlist(sapply(exts,function(x) gregexpr(x,fnx)))>0]

  for(ex in exx){
    waps=paste(ex,'.wpl',sep='')
    print(paste(fnx,'Added to',waps))
    lss = unique(readLines(waps))
    js="            <media src=\"c:\\MyVideos\\RPDNClips\\%s\"/>"
    adds=sprintf(js,fnx)
    lsx1=c(lss[1:(length(lss)-3)],adds,tail(lss,n=3))
    m3uname <- paste(pldrive,'My Playlists/',sep='')
    write(lsx1,paste(m3uname,waps,sep='')) # add new clips
  }
}
if(len(fn)>0)
  source('~/pllist.git/BuildxxALT.R')
