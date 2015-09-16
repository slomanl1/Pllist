library(gWidgets)
options(guiToolkit = "RGtk2")   
setwd('~/')
if(exists('w'))
  if(isExtant(w))
    dispose(w)
if(exists('fwind'))
  if(isExtant(fwind))
    dispose(fwind)
if(exists('obj'))
  if(isExtant(obj))
    dispose(obj)

scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
get_list_content <- function (fnx,cmts) data.frame(fnx,cdts=paste(as.character(file.mtime(fnx)),file.size(fnx)),comments=cmts,stringsAsFactors =FALSE)

len=function(x) length(x)
fi=function(x,y) y[grepl(x,y,fixed=TRUE)] # find within function
file.ext=function(x) substr(x,nchar(x)-2,nchar(x))
delay500=function(){
  x=1000000
  while(TRUE)
  {
    x=x-1
    if(x==0)
      break
  }
}
extras=NULL
fmissing=NULL
deleted=FALSE

dirs=c(dir('D:/PNMTALL',full.names = TRUE),dir('C:/PNMTALL',full.names = TRUE))
dirs=subset(dirs,!grepl('lnk',dirs))
if (file.exists('D:/PNMTALL')) {
  shell('dir D: | findstr Volume > volz.txt')
  volz = readLines('volz.txt')
  unlink('volz.txt')
  if (grepl('Volume',volz[1]) & !grepl('RPDN',volz[1]))
  {
    vname = paste('D:\\',substr(volz[1],23,100),'.txt',sep = '')
    print(paste('VNAME =',vname))
    sfname = paste(substr(vname,1,nchar(vname) - 4),'.RData',sep = '')
    YesorNo=select.list(c('No','Yes'),preselect = 'No',title='DELETE sfname and choose folders?',graphics = TRUE)
    if(YesorNo=='Yes'){
      dirpaths=select.list(basename(dirs),graphics = TRUE,multiple = TRUE,preselect = basename(dirs))
      if(len(dirpaths)==0)
        stop('Aborted')
      unlink(sfname)
      save(dirpaths,file='dirpaths.RData')
    }else{
      if(YesorNo=='')
        stop('Aborted')
      else
        load('dirpaths.RData')
    }
    dirsx=dirs[basename(dirs) %in% dirpaths]
    shell(paste('dir', 'D:\\PNMTALL',' /S/B/OD >  zz.txt'))
    shell(paste('dir', 'C:\\PNMTALL',' /S/B/OD >> zz.txt'))
    zz1 = readLines('zz.txt')
    zz2 = zz1[which(grepl('.',zz1,fixed = TRUE) &
                      !grepl('RECYCLE|.txt|.RData|RPDN|.tmp',zz1,fixed=TRUE) &
                      toupper(dirname(zz1)) %in% toupper(normalizePath((dirs),winslash = '/')))]
    zz=zz2[toupper(dirname(zz2)) %in% toupper(dirsx)]
    if (!file.exists(sfname)) {
      writeLines('','allmetadata.txt')
      for(dirpath in dirpaths){
        print(dirpath)
        shell(paste('getm',dirs[basename(dirs) %in% dirpath],' >>  allmetadata.txt')) 
      }
      dts = file.mtime(zz) # file dates
      #unlink('allmetadata.txt')
    }else{
      load(sfname)
      dto = file.mtime(zz) # new file dates
      dmissing = NULL
      if (len(dts) == len(dto))
        dmissing = zz[!dto %in% dts] # add records with new date to dmissing
      ttl = which(substr(am,1,1) == '=')
      xmissing = zz[!suppressWarnings(normalizePath(zz)) %in% suppressWarnings(normalizePath(substr(am[ttl],10,1000)))]
      missing1 = unique(c(dmissing,xmissing))
      fmissing = suppressWarnings(normalizePath(missing1, winslash = "/"))
      extras = am[ttl][!suppressWarnings(normalizePath(substr(am[ttl],10,1000))) %in% suppressWarnings(normalizePath(zz))]
      dts = dto # replace old dates

    }
  } 
}   
if (len(fmissing) > 0) {
  for (i in 1:len(fmissing)) {
    cmdd = "shell('getm D: >> allmetadata.txt')"
    fpp = file.path(substr(fmissing[i],1,2),
                    substr(dirname(fmissing[i]),3,nchar(dirname(fmissing[i]))), basename(fmissing[i]))

    cmdy=sub('getm D:', paste('echo','========', fmissing[i]),cmdd) # write filename to metadata
    suppressWarnings(eval(parse(text = cmdy)))
    cmdx = sub('D:',fpp,cmdd)
    suppressWarnings(eval(parse(text = cmdx)))
    print(paste('Added   ',fmissing[i],'to allmetadata'))
  }
}
###########################################
procExtras=function() {
  if(len(extras)>0){
    exidxs=which(trim(substr(am,10,nchar(am))) %in% substr(extras,10,nchar(extras))) # extra indices in am[]
    ttidxs=which(ttl%in%exidxs)
    for (i in 1:len(exidxs)){
      nexttlidx=ttl[ttidxs[i]+1]
      idx=ttl[ttidxs[i]]
      if(is.na(nexttlidx)){
        print(paste('removed from allmetadata.txt',am[idx]))
        am[idx:len(am)]=NA
        break #indicates terminal condition (last ttl)
      }
      j=0
      while((idx+j)!=nexttlidx){
        print(paste('removed from allmetadata.txt',am[idx+j]))
        am[idx+j]=NA
        j=j+1
      }
    }  
    am=am[!is.na(am)]
    ttl = which(substr(am,1,1) == '=')
    writeLines(am,'allmetadata.txt')

  }
}
####################################
if(len(extras) | len(fmissing) | !file.exists(sfname)){
  procExtras()
  dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA,DMComment=NA)
  am1 = readLines('allmetadata.txt')
  am = am1[!grepl('Ingredients|Pantry|Album Title|Handler|exiftool',am1)]
  am=am[!is.na(am) & nchar(am)>0] # clean up na and empty metadata
  ttl = c(which(substr(am,1,1) == '='),len(am)+1)
  ducc=sum(duplicated(suppressWarnings(normalizePath(substr(am[ttl],10,1000)))))
  print(paste(ducc,'Duplicates found'))  
  flds=c(NA,NA,NA,NA,'Title',NA,'Comment','SubTitle',NA,'DM Comment')
  pb = winProgressBar(title = "R progress bar", label = "",
                      min = 1, max = length(ttl)-1, initial = 0, width = 300)
  for(i in 1:(len(ttl)-1)){
    setWinProgressBar(pb, i, title = paste('Parsing Metadata', label = ifelse(i>1,dirname(dfan[i-1,'filename']),'')))
    dfan[i,'filename']=  substr(am[ttl][i],10,nchar(am[ttl][i]))
    j=1
    while(ttl[i]+j < ttl[i+1]){
      tmpp=am[ttl[i]+j]
      if(is.na(tmpp)){ # indicates terminal condition
        break
      }
      fld=flds[as.integer(attributes(regexpr('Title|Comment|Subtitle|DM Comment',tmpp))[1])]
      if(len(fld)==1){
        fld=sub('DM Comment','DMComment',fld) # convert DM Comment field name to disallow spaces
        dfan[i,fld]=tmpp
      }
      j=j+1
    }
  }
  dfan$Title=    trim(sub("Title                           :",'',dfan$Title))
  dfan$Comment=  trim(sub("Comment                         :",'',dfan$Comment))
  dfan$SubTitle= trim(sub("Subtitle                        :",'',dfan$SubTitle))
  dfan$DMComment=trim(sub("DM Comment                      :",'',dfan$DMComment))
  
  dfan$Title=    gsub("'",'',dfan$Title)
  dfan$Title=    gsub(",",'',dfan$Title)
  dfan$Comment=  gsub("'",'',dfan$Comment)
  dfan$SubTitle= gsub("'",'',dfan$SubTitle)
  dfan$DMComment=gsub("'",'',dfan$DMComment)
  save(am,ttl,dts,dfan,file = sfname)
  close(pb)
}

tpexist=FALSE
avail=FALSE
changed=FALSE

lnttl='Enter Search Criteria'
dflt = ''
if(file.exists('dfltsave.RData'))
  load('dfltsave.RData')
Passt=FALSE
jerking=FALSE
while(!jerking)
{
  
  if(!Passt){
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
  }else
    Passt=FALSE
  ################ REBUILD an from dfan ################
  an=paste(ifelse(is.na(dfan$Title)     ,'', paste('Title: ',dfan$Title,sep='')),
           ifelse(is.na(dfan$DMComment) ,'', paste('Comment: ',dfan$DMComment,sep='')),
#           ifelse(!is.na(dfan$Comment) | is.na(dfan$DMComment),'', 
#                                             paste('Comment: ',dfan$DMComment,sep='')),
           ifelse(!is.na(dfan$SubTitle)&!nchar(dfan$SubTitle)  ,'', paste('Subtitle: ',dfan$SubTitle,sep='')))
  an=gsub('Title:  ','Title: ',an,ignore.case = TRUE)
  an=sub("Title:NA",'',an)
  an=gsub(',','',an)
  an=gsub('Comment:  ','Comment: ',an)
  an=gsub("Comment: NA",'',an)
  an=gsub('Subtitle:  ','Subtitle: ',an,ignore.case = TRUE)
  an=gsub("Subtitle: NA",'',an,ignore.case = TRUE)
  an=gsub("Subtitle:NA",'',an,ignore.case = TRUE)
  
  
  if (is.null(liner))
    break
  
  if (nchar(liner) > 0)
  {
    dflt = liner
    save(dflt,file='dfltsave.RData')
    srct=unlist(strsplit(toupper(liner),' '))
    anttl=paste(dfan$filename,an)
    anttlu=toupper(anttl)
    pnoln=NA
    allc=NA
    
    for (i in 1:len(srct))
      allc=c(allc,which(grepl(srct[i],anttlu,fixed = TRUE)))
    idxs=as.integer(names(which(table(allc)==len(srct))))
    if(len(idxs)==0)
    {
      print('Non found')
      avail=FALSE
      next
    }
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
  {
    delay500()
    if(isExtant(tab)){
      enabled(tbutton)=(len(svalue(tab))!=0)
      enabled(dbutton)=(len(svalue(tab))!=0)
    }
  }
  if(changed | deleted){
    dfix=which(grepl(trim(fnames[idx,'fnx']),dfan[,'filename'],fixed=TRUE))
    ofn=dfan[dfix,'filename']
    dispose(w)
  }
  if(changed){
    changed=FALSE
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
    if(nchar(trim(dfan[dfix,'Comment']))==0){
      dfan[dfix,'Comment']='--'
    }else{
      dfan[dfix,'DMComment']=dfan[dfix,'Comment']
    }
    if(file.ext(dfan[dfix,'filename'])=='wmv'){
      gmessage('Cannot write metadata to wmv files')
    }else{
      print(paste('Updating Metadata in',dfan[dfix,'filename']))
      cmdd=paste("shell('exiftool -DMComment=",'"',dfan[dfix,'Comment'],'" -Title=" ',
                 dfan[dfix,'Title'],'", -SubTitle=" ',dfan[dfix,'SubTitle'],'" ',dfan[dfix,'filename'],"')",sep='')
      writeLines(cmdd,'Jester.R') 
      answ=gconfirm('Update Metadata - Are you Sure?')
      if(answ){
        source('jester.R')
        ttllorig=paste(trim(dfan[dfix,'filename']),'_original',sep='')
        if(file.exists(ttllorig)){
          unlink(ttllorig)
          extras=trim(paste("========",ofn)) # remove old metadata associated with the old file
          procExtras()
        }else
          print(paste('Orig file not found for deletion - could be a WMV file',ttllorig))
      }
    }
    next #rebuild an from updated dfan
  }
  if(deleted){
    dfan=dfan[rownames(dfan)!=dfix,] # remove deleted file from dfan and rebuild an
    extras=paste("========",ofn) # remove old metadata associated with the old file
    procExtras()
    deleted=FALSE
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



