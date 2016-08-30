########## Changes date of clip files to random if filename info has more than 7 characters#############
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
setwd('c:/PNMTALL/rpdnclips')
load('~/mfnfo.RData') # load fnfo, lsst, wpls and xx
lsst=mfnfo$lsst
lsstn=gsub('_REN','',lsst)
bn=gsub('[a-z|A-Z]','',lsstn)
bc=gsub('[0-9]','',lsstn)
bn=substr(bn,1,nchar(bn)-2)
namer=mfnfo
namer$mtime=file.mtime(mfnfo$lsst)
namer$mlsst=namer$lsst
namer$lsst=gsub('_REN','',namer$lsst)
namer$bn=as.integer(bn)
namer$info=bc
namer$ofn=NA
namer=namer[!duplicated(namer$lsst),]
nn=(namer[nchar(namer$info)>7,])
nx=nn[sample(order(nn$mtime)),c(2,7)]
nx$lsst=nn$lsst
mtime=nx$mtime
svtO=nx$lsst
dtn=mtime+(8*3600) #add 8 hours to make GMT
fn=normalizePath(as.character(svtO),winslash = '/')
dx=data.frame(dtn,fn,times=NA)

dx$times=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
               ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')

cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
eval(parse(text=cmd))
