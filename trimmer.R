len=function(x) length(x)
EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n"){
  .GlobalEnv$ss=NULL
  while(TRUE){
    ginput(x, icon="question", title=svt, handler = function(h,...) .GlobalEnv$ss=h$input)
    startt= .GlobalEnv$ss   
    if(len(startt)>0){
      if(!is.na(as.integer(startt))){
        break # good integer
      }else{
        cpos=regexpr(':',startt)
        if(cpos>0){
          f1=as.integer(substr(startt,1,cpos-1))
          f2=as.integer(substr(startt,cpos+1,nchar(startt)))
          if (f1>=0 & f1<60 & f2>=0 & f2<60){
            break # good mm:ss
          }
        }
        
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}
StartMyGUI <- function(handler=function(h,...) {
  dispose(h$obj)
}) {
  startt=EnterStartStop()
  print(len(startt))
  if(len(startt)>0){
    endtt=EnterStartStop("Enter Time Duration (secs) or (mm:ss), or Esc for End of File\n")
    svtt='c:/RealPlayerDownloads/trimmed.mp4'
    unlink('~/temppt.mp4')
    unlink(svtt)
    file.rename(svt,'~/temppt.mp4')
    if(len(endtt)==0){
      endtt=10000
    }
    cmdd=paste('shell("ffmpeg.exe -ss',startt,' -i c:/users/LarrySloman/Documents/temppt.mp4 -t',endtt,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    svtO=paste(file_path_sans_ext(svt),'_New.',file_ext(svt),sep='') # add _New to original filename
    file.rename(svtt,svtO) # replace svt has trimmed with start to end
    file.rename('~/temppt.mp4',svt) # keep original file
    }
  return()
} 
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
#gtkMain()
