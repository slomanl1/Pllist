scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
if(file.exists('~/writeErrorLog.txt')){
  dd=readLines('~/writeErrorLog.txt')
  dirs=dir('D:/PNMTALL',full.names = TRUE)
  dirs=subset(dirs,!grepl('lnk',dirs))
  dirsx=dirs
  shell(paste('dir', 'D:\\PNMTALL',' /S/B/OD >  zz.txt'))
#  shell(paste('dir', 'C:\\PNMTALL',' /S/B/OD >> zz.txt'))
  zz1 = readLines('zz.txt')
  unlink('zz.txt')
  zz2 = zz1[which(grepl('.',zz1,fixed = TRUE) &
                    !grepl('RECYCLE|RPDN',zz1,fixed=TRUE) & 
                    !(file_ext(zz1) %in% c('txt','rar','part','exe','crdownload','msi','pdf')) & (file.size(zz1)>0) &
                    toupper(dirname(zz1)) %in% toupper(normalizePath((dirs),winslash = '/',mustWork=TRUE)))]
  zz=zz2[toupper(dirname(zz2)) %in% toupper(dirsx)]
  bn=basename(zz)
  pr=gregexpr('RATE|TRIM',bn)
  pr=unlist(pr)
  tt1=substr(bn,1,pr-1)
  tt=tt1[nchar(tt1)>0]
}else{
  tt=''
  mm=""
  dd=''
  writeLines(mm,'~/writeErrorLog.txt')
}

dd=dd[!(file.exists(dd))]
bnw=basename(dd)
prw1=gregexpr('RATE|TRIM',bnw)
missing=NULL
if(len(prw1)>0){
  prw=sapply(1:len(prw1), function(x) prw1[[x]][1])
  ttw1=substr(bnw,1,prw-1)
  ttw=ttw1[nchar(ttw1)>0]
  missing=ttw[!ttw %in% tt]
}
if(len(missing)>0){
  found=unlist(sapply(1:len(missing), function(x) zz[which(grepl(missing[x],zz,fixed=TRUE))]))
  xx=grepl('character',(sapply(1:len(missing), function(x) zz[which(grepl(missing[x],zz,fixed=TRUE))])))
  missing=missing[which(xx)]
}
if(len(missing)>0){  
  load('~/EPNMTALLyy.RData') # load cc
  efound=unlist(sapply(1:len(missing), function(x) cc[which(grepl(missing[x],cc,fixed=TRUE))]))
  zw=sapply(1:len(missing), function(x) cc[which(grepl(missing[x],cc,fixed=TRUE))])
  zm=which(sapply(1:len(missing), function(x) (len(zw[[x]])>0))==TRUE)
  ss=1:len(missing)
  ss=ss[!ss %in% zm] # remove efound 
  missing=unique(missing[ss])
  print('MISSING')
  catt(missing)
  print('EFOUND')
  catt(efound)
  if(len(missing)>0){
    mm=unlist(sapply(1:len(missing), function(x) dd[which(grepl(missing[x],dd,fixed=TRUE))]))
#    writeLines(mm,'~/writeErrorLog.txt')
  }
  save(efound,file='~/Efound.RData')
}else{
  print('NO MISSING RECORDS FOUND')
  unlink('~/writeErrorLog.txt')
}


