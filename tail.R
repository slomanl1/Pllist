tailfile=function(file, n=6) {
  bufferSize <- 10240L
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

args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(file.exists(svt)){
nn=args[2]
if(is.na(nn))
  nn=6
nn=as.numeric(nn)
cat(paste(tailfile(svt,nn),'\n',sep=''),sep='')
}else{
  print('File Not Found')
}

