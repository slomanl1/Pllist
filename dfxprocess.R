source('~/pllist.git/EnterStartStop.R')
require(gdata)

if(!exists('Fdate')){
  Fdate=FALSE
}else{
  filename=svt
}
if(!Fdate)
  filename=file.choose()

if(!exists('BypassE')){
  BypassE=FALSE
}

if(!BypassE)
  ALTGinput(paste("Enter mod date/time",filename), FALSE)

if(len(ss)){
  print(paste('ss=',ss,filename))
  dx=data.frame(dtn=NA,fn=NA,times=NA)
  dx[1,'dtn']=ss
  dx$dtn=as.POSIXlt(dx$dtn)+(6*3600) # add 6 hours to make GMT
  if(!isDST())
    dx$dtn=dx$dtn+3600 # add one more hour for Standard Time (MST)
  dx[1,'fn']=normalizePath(as.character(filename),winslash = '/')
  dx[1,'times']=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                      ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
  
  cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
  eval(parse(text=cmd))
}
