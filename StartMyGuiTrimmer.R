StartMyGUI <- function() {
  startt=EnterStartStop()
  if(.GlobalEnv$convert){
    .GlobalEnv$convert=FALSE
    print('Convert H265')
    unlink('~/out.mp4')
    shell(paste('c:/Users/Larry/Documents/hexDump/bin/converth265.bat "',
                svt,'" c:/Users/Larry/Documents/out.mp4',sep=''),translate = TRUE)
    if(!file.rename('out.mp4',svt))
      print('file rename back to orig failed')
    startt=NULL
  }
  
  print(len(startt))
  if(len(startt)>0){
    endtt=0
    if(!.GlobalEnv$ToEnd)
      endtt=EnterStartStop("Enter Time Duration (secs) or (mm:ss), 
                           or Enter/Esc for End of File\n",TRUE)
    
    if(startt > 0 & len(endtt)){
      svtt='c:/RealPlayerDownloads/trimmed.mp4'
      unlink('~/temppt.mp4')
      unlink(svtt)
      file.rename(svt,'~/temppt.mp4')
      if(.GlobalEnv$ToEnd){
        endtt=10000
        .GlobalEnv$ToEnd=FALSE
      }
      cmdd=paste('shell("ffmpeg.exe -ss',startt,' -i c:/users/Larry/Documents/temppt.mp4 -t',endtt,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
      print(cmdd)
      eval(parse(text=cmdd))
      svt1=sub('TRIM','',svt)
      svtO=paste(file_path_sans_ext(svt1),'_cut.',file_ext(svt1),sep='') # add _New to original filename
      file.rename(svtt,svtO) # replace svt has trimmed with start to end
      file.rename('~/temppt.mp4',svt) # keep original file
    }else
      print('Invalid start/end time')
  }
  return()
} 