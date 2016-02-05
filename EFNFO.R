### This program puts the mtime of the _new HEVC file back to the corresponding 
###       file mtime in the efnfo from old E: backup
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/efnfo.RData')
load('~/dfan.RData')
load('~/mssg.RData')
dff=data.frame(filename=dfan$filename,bn=basename(sub('_New','',dfan$filename)),dtitle=dfan$Title,stringsAsFactors = FALSE)
mgdf=merge(efnfo,dff,by='bn',all.y = TRUE)
mgdf$Dmtime=file.mtime(as.character(mgdf$filename.y))
mgdf$Msize=file.size(as.character(mgdf$filename.y))
mgdf=mgdf[,c(1:10,12,13,11)]
mgdn1=subset(mgdf,grepl('_New',filename.y))
# mgdo=subset(mgdf,!grepl('_New',filename.y))
# mgdo$tdif=mgdo$mtime-mgdo$Dmtime
# emg=merge(efnfo,mgdo[,c('bn','tdif','Dmtime')],by='bn',all.x = TRUE) # add Dmtime to correct mtime in efnfo
# emg[!is.na(emg$tdif) & emg$tdif>0,'mtime']=emg[!is.na(emg$tdif) & emg$tdif>0,'Dmtime']
# efnfo=emg[,names(efnfo)]
# save(efnfo,file='~/efnfo.RData')
mgdnx=mgdn1[0,]
while(nrow(mssg)>0){
  mgdn2=merge(mgdn1,mssg,by='bn',all.x = TRUE)
  mgdn2$mtime.x=as.character(mgdn2$mtime.y)
  # get e: filenames and mtime from filename.x.y and mtime.y
  mgdn2[is.na(mgdn2$filename.x.x),'filename.x.x']=mgdn2[is.na(mgdn2$filename.x.x),'filename.x.y']
  mgdn2[is.na(mgdn2$filename.x.x),'mtime.x']=mgdn2[is.na(mgdn2$filename.x.x),'mtime.y']
  mgdn=mgdn2[,c('bn','filename.x.x',"size","isdir","mode","mtime.x","ctime","atime",
                "exe","filename.y.x","dtitle.x","Dmtime.x" )]
  #names(mgdn)=names(mgdn1) ########
  mgdn3=mgdn[is.na(mgdn$filename.x.x),]
  agg=mgdn3[which(sapply(1:(nrow(mgdn3)),
                         function(x) return(agrepl(mgdn3$dtitle[x],mgdn3$bn[x],ignore.case = TRUE,
                                                   max.distance = .6)))==1),c('filename.y.x','dtitle.x','bn')]
  #-----------------------------------------------------------------------------------------
  print(agg)
  agg=agg[!agg$bn %in% mssg$bn,]
  j=nrow(agg)
  i=nrow(mssg)+1
  k=1
  while(j>0){
    mssg[i,]=NA
    mssg[i,]$bn=agg[k,]$bn
    mssg[i,]$filename.y=normalizePath(as.character(agg[k,]$filename.y.x),winslash = '/')
    mssg[i,]$dtitle=agg[k,]$dtitle.x
    mssg[i,]$Dmtime=file.mtime(as.character(mssg[i,]$filename.y))
    i=i+1
    j=j-1
    k=k+1
  }
  fix(mssg)
  save(mssg,file='~/mssg.RData')
  mgdnx=mgdn[!is.na(mgdn$mtime.x),]
  if(nrow(mgdnx)==0)
    next
  dd=paste(sub(mgdnx$bn,'',mgdnx$filename.y.x,fixed=TRUE),mgdnx$bn,sep='')
  mgdn1[mgdn1$bn %in% mgdnx$bn,]$filename.x=sub(substr(dd,1,1),'E',dd)
  mgdn1[mgdn1$bn %in% mgdnx$bn,]$mtime=mgdnx$mtime.x
  mssg=mssg[!mssg$bn %in% mgdnx$bn,] # remove entries with filename filled in manually
  save(mssg,file='~/mssg.RData')
}
### GET title from EDrive backup of originals of e:/PNMTALL of bn in print expression above
dx=mgdnx
if(nrow(mgdnx)>0){
  reverter=mgdnx[abs(as.POSIXlt(mgdnx$mtime.x)-as.POSIXlt(mgdnx$Dmtime.x))>40,c('filename.y.x','mtime.x','Dmtime.x')]
dx=subset(reverter,!is.na(mtime.x))
}
if(nrow(dx)>0){
  dx$dtn=as.POSIXlt(dx$mtime.x)
  dx$fn=normalizePath(as.character(dx$filename.y.x),winslash = '/')
  dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                 ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
  cmds=paste('shell(','"fdate',dx$fn,dx$times,'")')
  for( i in 1:nrow(dx))
    eval(parse(text=cmds[i]))
}else
  print('NO NEW RECORDS FOUND')

dfx=normalizePath(dfan[!grepl('_New',dfan$filename),'filename'],winslash = '/')
load('~/svts.Rdata')
dfx=dfx[!dfx %in% svts]
print(paste('files remaining',len(dfx)))
print(' ')
for(fn in dfx){
  cat(which(fn == dfx),' ')
  svt=fn
  cmdd=paste('shell("mediainfo.exe',svt,' >meta.txt",mustWork=NA,translate=TRUE)')
  eval(parse(text=cmdd))
  svts[i]=svt
  HEVC[i]=FALSE
  meta=readLines('meta.txt')
  unlink('meta.txt')
  if(any(gregexpr('HEVC',meta)>0)){
    svts[i]=svt
    HEVC[i]=TRUE
    print(svt)
  }
  i=i+1
}
save(svts,HEVC,i,file='~/svts.Rdata')

