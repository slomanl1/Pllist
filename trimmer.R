EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n"){
  .GlobalEnv$ss=NULL
  while(TRUE){
    ginput(x, icon="question", handler = function(h,...) .GlobalEnv$ss=h$input)
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
    endtt=EnterStartStop("Enter Time Duration (secs) or (mm:ss)\n")
    unlink('~/temppt.mp4')
    file.rename(svt,'~/temppt.mp4')
    svtt1=gsub(' ','',svt) # remove spaces for ffmpeg (does not accept " in filename's)
    svtt='c:/RealPlayerDownloads/trimmed.mp4'
    entf=FALSE
    if(len(endtt)==0){
      entf=TRUE
      endtt=10000
    }
    cmdd=paste('shell("ffmpeg.exe -ss',startt,' -i c:/users/LarrySloman/Documents/temppt.mp4 -t',endtt,'-c:v copy -c:a copy',svtt,'",mustWork=NA,translate=TRUE)')
    print(cmdd)
    eval(parse(text=cmdd))
    if(entf){
      file.rename(svtt,svt) # replace svt has trimmed with start to end
    }else{
      file.rename('~/temppt.mp4',svt)  # put back original, trimmed.mp4 has trimmed
    }
  }
  return()
} 
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets)
library(gWidgetsRGtk2)
args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(is.na(svt))
  svt=file.choose()
print(paste('svt=',svt))
StartMyGUI()
gtkMain()
