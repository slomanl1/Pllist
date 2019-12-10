isHevc = function(filename){
  filename=as.character(filename)
  if(!file.exists(filename))
    return(NA)
  svt=normalizePath(filename,winslash = '/')
  meta=shell(sprintf('exiftool "%s" -CompressorID',svt),translate=TRUE,intern = TRUE)
  return(any(grepl('hev1',meta)))
}

isVC1 = function(filename){
  filename=as.character(filename)
  if(!file.exists(filename))
    return(NA)
  svt=normalizePath(filename,winslash = '/')
  meta=shell(sprintf('mediainfo "%s"',svt),translate=TRUE,intern = TRUE)
  return(any(grepl('Video 9',meta)|grepl('VC-1',meta)|grepl('Video 8',meta)))
}
