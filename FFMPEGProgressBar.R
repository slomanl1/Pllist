print('HELLO - FFMPEG PROGRESS BAR')
setwd('~/')

gi=function (x, y) 
{
  ix = which(grepl(toupper(x), toupper(y), fixed = TRUE))
  return(y[grepl(toupper(x), toupper(y), fixed = TRUE)])
}

load('mediainfo.RData') # get metadata saved in reduceallmovies.R
dur=subset(ddd,grepl('Duration',ddd))[1]
mns=substr(dur,unlist(gregexpr(':',dur))+2,nchar(dur)) # xxMN yyS
m <- gregexpr('[0-9]+',mns)
nms=as.integer(unlist(regmatches(mns,m)))
if(grepl('mn',mns)){
  durx=nms[1]*60 + nms[2] # duration converted to seconds
}else{
  durx=nms[1]
}
if(file.exists('block.txt')){
  pb=winProgressBar('FFMPEG PROGRESS',max=durx*1000000) # tius is microseconds
  while(TRUE){
    Sys.sleep(1)
    xx=readLines('block.txt')
    if(any(grepl('progress=end',xx)))
      break
    tius=as.integer(strsplit(gi('out_time_ms',tail(xx)),'=')[[1]][2])
    setWinProgressBar(pb,tius,paste('FFMPEG PROGRESS',ptn(tius),'/',ptn(durx*1000000),round(tius/(durx*10000),1),'%'))
  }
  close(pb)
  unlink('block.txt')
}else{
  print('block.txt not found')
}
