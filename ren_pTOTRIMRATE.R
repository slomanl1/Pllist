source('~/.RProfile') # required for standalone version
scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
load('~/dirtbl.rdata')
load('~/reserved.RData')
library(gWidgets2)
options(guiToolkit = "RGtk2")

cd('D:/pnmtall/newdownloads')
dd=dir()
vlist=NULL
du=fi('.rar',dd)
if(len(du)>0){
  for (ffn in du){
    vlist=fi('mp4',shell(paste('c:/Users/Larry/Documents/hexDump/bin/unrar.exe lb "',
                ffn,'" ' ,sep=''),translate = TRUE, intern = TRUE))
    shell(paste('c:/Users/Larry/Documents/hexDump/bin/unrarmp.bat "',
                ffn,'" ' ,sep=''),translate = TRUE, intern = TRUE)
    shell(sprintf('nircmd moverecyclebin "%s"',ffn),translate=TRUE)
    print(paste(ffn,'Moved to recycle bin'))
  }
}
dz=fi('.zip',dd)
if(len(dz)>0){
  for (ffn in dz){
    vn=unzip(ffn,list=TRUE)[1,'Name']
    vlist=c(vlist,vn)
    unzip(ffn,vn) # extract the mp4
    shell(sprintf('nircmd moverecyclebin "%s"',ffn),translate=TRUE)
    print(paste(ffn,'Moved to recycle bin'))
  }
}
vlist=c(vlist,fi(' ',dd))
vlist=vlist[!grepl('crdownload',vlist)]
dux=vlist
dx=fi('mp4',dd)
if(len(dx)>0 | len(du)>0){
  streams=shell('streams -nobanner -s',intern = TRUE)
  xx=sub('\r','',streams)
  xx=sub('mp4:','mp4',xx,ignore.case = TRUE)
  xx=sub('\\.c','c',xx)
  xx=sub('\\.c','c',xx)
  dd=c(xx,dux)
  pathh=normalizePath(dd,winslash = '/',mustWork = FALSE)
  paths=pathh[file.exists(dd) & !file.info(dd)$isdir & !grepl('TRIMRATE_Trim',dd)]
  if(len(paths)>0){
    nfn=sapply(1:len(paths), function(x) {
      sub(file_ext(paths[x]),'TRIMRATE.mp4',paths[x])
    })
    nfn=sub('\\.','',nfn)
    nfn=gsub('_|-|&|,','',nfn)
    nfn=gsub(' ','',nfn)
    nfn=gsub('(','',nfn,fixed = TRUE)
    nfn=gsub(')','',nfn,fixed = TRUE)
    nfn=gsub('RATE-','RATE_',nfn,fixed=TRUE)
    nfn=sub('cutNew','cut_New',nfn)
    nfn=gsub(reserved,'',nfn,ignore.case = TRUE)
    nfn=gsub('[0-9][0-9][0-9]p','',nfn) # remove 3 integers plus the p
    nfn=gsub('[0-9]','',nfn) #remove rest of integers
    ############# PUT back original file extensions #############
    nfn=sapply(1:len(nfn), function(x) {
      sub(paste('.',file_ext(nfn[x]),sep=''),paste('.',file_ext(paths[x]),sep=''),nfn[x],fixed=TRUE)
    })
    
    ofn=paths
    ofnn=ofn[!ofn==nfn]
    nfnn=nfn[!ofn==nfn]
    if(len(nfnn)>0) {
      shell('streams -nobanner -s -d')
      rss=file.rename(ofnn,nfnn)
      galert(paste(sum(rss),'Files Renamed'))
      print(paste(sum(rss),'Files Renamed'))
      print(nfnn)
      
      nfx=nfnn[rss]
      zz=subset(nfx,!grepl('cut',nfx)&file_ext(nfx)=='mp4')
      zx=(lapply(dirtbl$Var1,function(x) {grepl(x,zz)}))
      xxx=as.data.frame(zx)
      names(xxx)=dirtbl$Var1
      rownames(xxx)=file_path_sans_ext(zz)
      #xxx=xxx[,sort(colnames(xxx))]
      studio=sapply(1:nrow(xxx), function(x) colnames(xxx)[which(as.logical(xxx[x,]))][1])
      #studio=tail(sapply(1:len(studios),function(x) studios[[x]][1]),1)
      catt(studio)
      nfnnn=sapply(1:len(studio),function(x){return(sub(studio[x],paste(studio[x],'-',sep=''),nfnn[x]))})
      rss1=file.rename(nfnn,nfnnn)
      print(paste(sum(rss1),'Files Renamed'))
    }else{
      print('No FILES FOUND to RENAME')
      if(interactive())
        galert('No FILES FOUND to RENAME')
    }
  }else{
    galert('No FILES FOUND in RealPlayerDownloads')
  }
}else{
  galert('No mp4 FILES FOUND in RealPlayerDownloads')
}
rm('reserved')


