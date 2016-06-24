source('~/pllist.git/StartMyGuiTrimmer.R') # for tailfile()
txl=''
gi=function (x, y) 
{
  ix = which(grepl(toupper(x), toupper(y), fixed = TRUE))
  return(y[grepl(toupper(x), toupper(y), fixed = TRUE)])
}

ffmpegProgressBar = function() {
  print('HELLO - FFMPEG PROGRESS BAR')
  source('~/pllist.git/StartMyGuiTrimmer.R') # for tailfile()
  setwd('~/')
  if(!exists('blockFile')){
    load('blockFileNames.RData')
    unlink('blockFileNames.RData')
  }
  print(paste('svt-',svt,'blockFile-',blockFile,'metaFile-',metaFile))
  
  gi=function (x, y) 
  {
    ix = which(grepl(toupper(x), toupper(y), fixed = TRUE))
    return(y[grepl(toupper(x), toupper(y), fixed = TRUE)])
  }
  
  load(metaFile) # get metadata saved in reduceallmovies.R/StartMyGuiTrimmer.R
  unlink(metaFile)
  dur=subset(ddd,grepl('Duration',ddd))[1]
  mns=substr(dur,unlist(gregexpr(':',dur))+2,nchar(dur)) # xxMN yyS
  m <- gregexpr('[0-9]+',mns)
  nms=as.integer(unlist(regmatches(mns,m)))
  if(grepl('mn',mns)){
    durx=nms[1]*60 + nms[2] # duration converted to seconds
  }else{
    durx=nms[1]
  }
  Sys.sleep(1)
  if(file.exists(blockFile)){
    timeout=3
    if(!is.na(durx) & !exists('pbx'))
      pb=winProgressBar('FFMPEG PROGRESS',max=durx*1000000,label=svt,width=600) # tius is microseconds
    while(TRUE){
      if(.GlobalEnv$aborted)
        break
      Sys.sleep(1)
      xx=tailfile(blockFile,10)
      if(len(xx)==0){
        timeout=timeout-1
        if(timeout==0){
          print('TIMED OUT')
          break
        }
        next
      }
      if(any(grepl('progress=end',xx)))
        break
      tius=as.integer(strsplit(gi('out_time_ms',tail(xx)),'=')[[1]][2])
      pbtxt=paste('FFMPEG PROGRESS',ptn(tius),'/',ptn(durx*1000000),round(tius/(durx*10000),1),'%  ')
      
      if(exists('pbx')){
        if(isExtant(pbx))
          svalue(pbx)=round(tius/(durx*10000),1)
      }else{
        setWinProgressBar(pb,tius,pbtxt,label=svt)
      }
      if(exists('ww'))
        if(isExtant(ww))
          svalue(ww) = paste(svt,pbtxt,txl,'CLOSE to ABORT')
    }
    
    if(!exists('pbx'))
      close(pb)
    unlink(blockFile)
  }else{
    print('blockFile not found')
  }
}
