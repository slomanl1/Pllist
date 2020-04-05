isDST=function() grepl('MDT',Sys.time())
WriteDate=function(filename,ss){
  #print(paste('ss=',ss,filename))
  dx=data.frame(dtn=NA,fn=NA,times=NA)
  dx[1,'dtn']=ss
  dx$dtn=as.POSIXlt(ss)+(8*3600) # add 8 hours to make GMT
  dx[1,'fn']=normalizePath(as.character(filename),winslash = '/')
  dx[1,'times']=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                      ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
  
  cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
  eval(parse(text=cmd))
}



