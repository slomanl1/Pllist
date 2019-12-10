scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
setwd('c:/my videos/rpdnclips')
unlink('c:/users/Larry/mimes.txt')
shell('exiftool *.* | findstr /I "Name Extension Mime" > c:\\users\\LarrySloman\\mimes.txt')
mn=readLines('c:/users/Larry/mimes.txt')
#unlink('c:/users/Larry/mimes.txt')
fname=mn[grepl('File Name',mn)]
fext=mn[grepl('File Type Extension',mn)]
mimet=mn[grepl('MIME Type',mn)]
term=min(len(fname),len(fext),len(mimet)) # adjust length because of an extra file in clips directory (mimes.txt)
mimes=data.frame(fname=fname[1:term],fext=fext[1:term],mimet=mimet[1:term])
mimes$fname=gsub(' ','',mimes$fname)
mimes$fname=trim(gsub("FileName:",'',mimes$fname))
mimes$fext=gsub(' ','',mimes$fext)
mimes$fext=trim(gsub("FileTypeExtension:",'',mimes$fext))
mimes$mimet=gsub(' ','',mimes$mimet)
mimes$mimet=trim(gsub("MIMEType:",'',mimes$mimet))
load('~/fnfo.RData')
mgg=merge(fnfo,mimes,by.y = 'fname',by.x='lsst')
hxx=as.character(as.hexmode(mgg$xx))
ofn=mimes$fname
nfn=paste(file_path_sans_ext(mimes$fname),'_',hxx,'.',mimes$fext,sep='')
mgn=mgg[,names(fnfo)]
mgn$lsst=nfn
mgn$hxx=as.character(as.hexmode(mgn$xx))
#file.rename(ofn,nfn)
bn=gsub('[a-z|A-Z]','',nfn)########################CHECK
last=as.integer(tail(bn,1))###############################

