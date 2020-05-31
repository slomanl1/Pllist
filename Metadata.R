source('~/.RProfile') # required for standalone version
len=function(x) length(x)
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
source('~/pllist.git/rmmovname.R')
load('~/dirtbl.RData')
library(lubridate)

ToEnd=FALSE
.GlobalEnv$convert=FALSE
.GlobalEnv$bOK=FALSE
.GlobalEnv$Fdate=FALSE
.GlobalEnv$Fmeta=FALSE
done=FALSE
print('Trimmer hello')

shell('nircmd win min process rscript.exe')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)

cd('D:/PNMTALL/RPDNClips')
args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(is.na(svt))
  svt=normalizePath(file.choose(),winslash = '/')
xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
               svt,'" ' ,sep=''),translate = TRUE, intern = TRUE)
stud=subset(xx,grepl('XMP',xx))
studio='NA'
if(len(stud)>0)
  studio=strsplit(subset(xx,grepl('XMP',xx)),':')[[1]][2]
durx=paste(subset(xx,grepl('Format  ',xx))[2],subset(xx,grepl('Duration  ',xx))[1],'-',studio)
dur=gsub('  ','',durx)
print(cat(paste(svt,'\n',dur,'\n')))
#alrt=galert(paste(svt,dur),delay = 10000)	
editMeta()
#dispose(alrt)

