len=function(x) length(x)
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/testnewginput.R')
ToEnd=FALSE
StartMyGUI <- function() {
  startt=EnterStartStop()
  print(len(startt))
  if(len(startt)>0){
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
shell('nircmd win min process rscript.exe')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets)
library(gWidgetsRGtk2)
library(tools)
args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(is.na(svt))
  svt=file.choose()
print(paste('svt=',svt))
StartMyGUI()

