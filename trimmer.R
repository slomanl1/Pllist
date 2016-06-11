source('~/.RProfile') # required for standalone version
len=function(x) length(x)
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')

ToEnd=FALSE
.GlobalEnv$convert=FALSE
.GlobalEnv$bOK=FALSE
.GlobalEnv$Fdate=FALSE
done=FALSE
print('Trimmer hello')

shell('nircmd win min process rscript.exe')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)


args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(is.na(svt))
  svt=normalizePath(file.choose(),winslash = '/')
xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                 svt,'" ' ,sep=''),translate = TRUE, intern = TRUE)
durx=paste(subset(xx,grepl('Format  ',xx))[2],subset(xx,grepl('Duration  ',xx))[1])
dur=gsub('  ','',durx)
print(paste('svt=',svt,dur))
alrt=galert(paste(svt,dur),delay = 10000)	
StartMyGUI()
if(isExtant(alrt))
  dispose(alrt)
