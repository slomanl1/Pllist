library(gWidgets)
options(guiToolkit = "RGtk2")   
setwd('~/')
if(exists('w'))
  if(isExtant(w))
    dispose(w)
if(exists('fwind'))
  if(isExtant(fwind))
    dispose(fwind)
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones

len=function(x) length(x)
delay500=function(){
  x=1000000
  while(TRUE)
  {
    x=x-1
    if(x==0)
      break
  }
}
get_list_content <- function (fnx,cmts) data.frame(fnx,cdts=as.character(file.mtime(fnx)),comments=cmts,stringsAsFactors =FALSE)

dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA,DMComment=NA)
am2 = readLines('allmetadata.txt')
am1=am2 #[!grepl("Subtitle                        : |DM Comment                      : ",am2)] 
am = am1[!grepl('Ingredients|Pantry|Album Title|Handler|exiftool',am1)][3:len(am1)]
ttl = c(which(substr(am,1,1) == '='),len(am))
flds=c(NA,NA,NA,NA,'Title',NA,'Comment','SubTitle',NA,'DM Comment')
pb = winProgressBar(title = "R progress bar", label = "",
                    min = 1, max = length(ttl)-1, initial = 0, width = 300)
for(i in 1:(len(ttl)-1)){
  setWinProgressBar(pb, i, title = paste('Parsing Metadata', label = NULL))
  dfan[i,'filename']=  substr(am[ttl][i],10,nchar(am[ttl][i]))
  j=1
  while(ttl[i]+j < ttl[i+1]){
    tmpp=am[ttl[i]+j]
    if(is.na(tmpp)){
      break
    }
    fld=flds[as.integer(attributes(regexpr('Title|Comment|Subtitle|DM Comment',tmpp))[1])]
    if(len(fld)==1)
      dfan[i,fld]=tmpp
    j=j+1
  }
}
dfan$Title=    sub("Title                           :",'',dfan$Title)
dfan$Comment=  sub("Comment                         :",'',dfan$Comment)
dfan$SubTitle= sub("Subtitle                        :",'',dfan$SubTitle)
dfan$DMComment=sub("DM Comment                      :",'',dfan$DMComment)
tpexist=FALSE
avail=FALSE
changed=FALSE
close(pb)

lnttl='Enter Search Criteria'
dflt = ''
if(file.exists('dfltsave.RData'))
  load('dfltsave.RData')

jerking=FALSE
while(!jerking)
{
  
  linerw=gwindow(height = 20, title=lnttl)
  obj <- gedit(text=dflt,container=linerw)
  shell('nircmd win activate title "Enter Search Criteria"')
  focus(obj)=TRUE
  addhandlerchanged(obj, handler=function(h,...)
    .GlobalEnv$avail=TRUE)
  addHandlerDestroy(obj, handler=function(h,...){
    .GlobalEnv$avail=TRUE
  })
  
  liner=NULL
  while(!avail)
  {}
  lnttl='Enter Search Criteria'
  if(isExtant(obj)){
    liner=svalue(obj)
    dispose(obj)
    if(exists('w'))
      if(isExtant(w))
        dispose(w)
    tpexist=FALSE
  }
  ################ REBUILD an from dfan ################
  an=paste(ifelse(is.na(dfan$Title),'',    paste('Title:',dfan$Title,sep='')),
           ifelse(is.na(dfan$Comment),'',  paste('Comment:',dfan$Comment,sep='')),
           ifelse(is.na(dfan$SubTitle),'', paste('Subtitle:',dfan$SubTitle,sep='')),
           ifelse(is.na(dfan$DMComment),'',paste('DM Comment:',dfan$DMComment,sep='')))
  
  
  if (is.null(liner))
    break
  
  if (nchar(liner) > 0)
  {
    dflt = liner
    save(dflt,file='dfltsave.RData')
    srct=unlist(strsplit(toupper(liner),' '))
    #        anttl=subset(an[ttl],!grepl('.lnk',an[ttl],fixed=TRUE))
    anttl=paste(dfan$filename,an)
    anttlu=toupper(anttl)
    pnoln=NA
    allc=NA
    
    for (i in 1:len(srct))
      allc=c(allc,which(grepl(srct[i],anttlu,fixed = TRUE)))
    idxs=as.integer(names(which(table(allc)==len(srct))))
    pnoln=dfan[idxs,'filename'] # how many match criteria?
    fns = NULL  
  }else
    break
  
  gdfopen=FALSE
  gdframe = get_list_content(pnoln,an[idxs])
  unsorted=TRUE
  fnames=gdframe[order(gdframe$cdts,decreasing = unsorted),]
  source('~/pllist/pllist.git/testplots.R')
  while(!avail)
  {}
  if(changed){
    changed=FALSE
    dfix=which(grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE))
    print(paste("dfix=",dfix,fnames[idx,'fnx']))
    if(fwind[,'filename']!=dfan[dfix,'filename']){
      answ=gconfirm('Rename - Are you Sure?')
      if(answ){
        if(!file.rename(dfan[dfix,'filename'],fwind[,'filename'])){ 
          print(paste("file rename FAILED from=",dfan[dfix,'filename'],"to=",fwind[,'filename']))
        }else{
          print(paste("file rename SUCCESSFUL from=",dfan[dfix,'filename'],"to=",fwind[,'filename']))
        }
      }
    }
    dfan[dfix,1:4]=fwind[,1:4]
    print('DFAN CHANGED')
    dispose(w)
    next #rebuild an from updated dfan
  }
  fns = ssv
  ssv = NULL #clear bones
  avail = FALSE
  if (len(fns) > 0) { # null HAS LENGTH 0
    dispose(w)
    writeLines(fns,'fns.m3u') # Write playlist
    load('headfoot.RData')
    writeLines(as.character(c(
      header,paste('<media src="',fns,'"/>'),footer
    ),sep = ''),'fns.wpl')
    shell("wmplayer c:\\Users\\LarrySloman\\Documents\\fns.wpl")
    Passt=TRUE
    unsorted=FALSE
    avail=FALSE
    emsg = 'OK'
    
  }
}



