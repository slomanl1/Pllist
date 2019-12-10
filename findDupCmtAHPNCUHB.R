load('~/mfnfo.rdata')
cd('D:/pnmtall/RPDNClips')
for(zm in c('AH','PN','HB','CU','Yom')){
  print(paste('#################### zm=',zm))
  if(zm=='HB')
    ahs=gregexpr(' HB\\+',mfnfo$cmt,ignore.case = TRUE)
  if(zm=='AH')
    ahs=gregexpr(' AH\\+',mfnfo$cmt,ignore.case = TRUE)
  if(zm=='CU')
    ahs=gregexpr(' CU\\+',mfnfo$cmt,ignore.case = TRUE)
  if(zm=='PN')
    ahs=gregexpr(' PN\\+',mfnfo$cmt,ignore.case = TRUE)
  if(zm=='Yom')
    ahs=gregexpr(' Yom\\+',mfnfo$cmt,ignore.case = TRUE)
  
  xx=sapply(1:len(ahs),function(x) ahs[[x]])
  sz=sapply(1:len(xx),function(x) len(xx[[x]]))
  ixs=which(sz==2)
  catters=(mfnfo[ixs,'cmt'])
  for(catter in catters){
    ss=strsplit(catter,' ')
    ix=which(grepl(zm,ss[[1]],ignore.case = TRUE))
    catt(ix)
    sq=1:len(ix)
    cc=ss[[1]]
    cout=capture.output(cat(cc[!cc %in% cc[ix[1]]]))
    catt(cout)
    iz=ixs[which(catter==catters)]
    wrStud(mfnfo[iz,'lsst'],mfnfo[iz,'studio'],cout)
  }
}
