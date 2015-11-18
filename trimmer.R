args <- commandArgs(TRUE)
save(args,file='~/args.RData')
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
load('~/args.RData')
print(args)
source('~/pllist/pllist.git/EnterStartStop.R')
cd('~/')
#svt=normalizePath(file.choose(),winslash = '/')
svt=args[1]
load('~/headfoot.RData')
writeLines(as.character(c(header,paste('<media src="',svt,'"/>'),footer),sep = ''),'~/svt.wpl')
#shell("wmplayer c:\\Users\\LarrySloman\\Documents\\svt.wpl")
unlink('~/svt.wpl')
startt=NULL
print(svt)

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

