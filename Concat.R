idd="C:/PNMTALL/RPDNCLIPS"
flss=NULL
fls=NULL
while(TRUE){
  idd=choose.dir(idd,caption='Choose Input Directory')
  if(is.na(idd))
    break
  print(paste('idd chosen=',idd))
  cd(idd)
  cd()
  while(!is.na(idd)){
    print(paste('idd chosen=',idd))
    fls=choose.files()
    
    if(!len(fls))
      break
    flss=c(flss,fls)
    tbl=gtable(flss,cont=gwindow())
  }
  if(!len(fls))
    break
}

if(len(flss)>1){
  od=choose.dir('~/',caption='Choose Output Directory')
  if(!is.na(od)){
    svtti=ginput('Enter output file name (.mp4)')
    svtt=paste(svtti,'.mp4',sep='')
    ofx=paste(od,svtt,sep = '\\')
    writeLines(' ',ofx)
    of=normalizePath(ofx,winslash='/')
    unlink(ofx)
    if(len(svtt) & len(svtti)){
      flss=paste("file '",flss,"'",sep='')
      writeLines(flss,'~/mylist.txt') 
      cmdd=paste('shell("ffmpeg.exe -f concat -i c:/users/Larry/Documents/mylist.txt -c copy',of,'",mustWork=NA,translate=TRUE)')
      print(cmdd)
      eval(parse(text=cmdd))
    }else{
      print('Invalid output file name')
    }
  }else{
    print('no directory chosen')
  }
  dispose(tbl)
}else{
  print('Invalid Input, at least 2 files required')
}

