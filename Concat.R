fls=choose.files(caption='Choose files to concatenate')
if(len(fls)>1){
  od=choose.dir('~/',caption='Choose Output Directory')
  if(!is.na(od)){
    svtti=ginput('Enter output file name (.mp4)')
    svtt=paste(svtti,'.mp4',sep='')
    ofx=paste(od,svtt,sep = '\\')
    writeLines(' ',ofx)
    of=normalizePath(ofx,winslash='/')
    if(len(svtt)){
      fls=paste("file '",fls,"'",sep='')
      writeLines(fls,'~/mylist.txt') 
      cmdd=paste('shell("ffmpeg.exe -f concat -i c:/users/Larry/Documents/mylist.txt -c copy',of,'",mustWork=NA,translate=TRUE)')
      print(cmdd)
      eval(parse(text=cmdd))
    }else{
      print('Invalid output file name')
    }
  }else{
    print('no directory chosen')
  }
}else{
  print('Invalid Input, at least 2 files required')
}
