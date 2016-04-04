print('HELLO - FFMPEG PROGRESS BAR')
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

tailfile=function(file, n=6) {
  bufferSize <- 1024L
  size <- file.info(file)$size
  
  if (size < bufferSize) {
    bufferSize <- size
  }
  
  pos <- size - bufferSize
  text <- character()
  k <- 0L
  
  f <- file(file, "rb")
  on.exit(close(f))
  
  while(TRUE) {
    seek(f, where=pos)
    chars <- readChar(f, nchars=bufferSize)
    k <- k + length(gregexpr(pattern="\\n", text=chars)[[1L]])
    text <- paste0(text, chars)
    
    if (k > n || pos == 0L) {
      break
    }
    
    pos <- max(pos-bufferSize, 0L)
  }
  
  tail(strsplit(text, "\\n")[[1L]], n)
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
  pb=winProgressBar('FFMPEG PROGRESS',max=durx*1000000,label=svt,width=600) # tius is microseconds
  while(TRUE){
    Sys.sleep(1)
    xx=tailfile(blockFile,10)
    if(len(xx)==0){
      timeout=timeout-1
      if(timeout==0){
        break
        }
      next
    }
    if(any(grepl('progress=end',xx)))
      break
    tius=as.integer(strsplit(gi('out_time_ms',tail(xx)),'=')[[1]][2])
    pbtxt=paste('FFMPEG PROGRESS',ptn(tius),'/',ptn(durx*1000000),round(tius/(durx*10000),1),'%')
    setWinProgressBar(pb,tius,pbtxt,label=svt)
    svalue(ww) = pbtxt
  }
  close(pb)
  unlink(blockFile)
}else{
  print('blockFile not found')
}
