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
get_list_content <- function (fnx,cmts) data.frame(fnx,Date=as.character(file.info(fnx,extra_cols=FALSE)$ctime),Size=prettyNum(file.size(fnx),big.mark = ","),comments=cmts,stringsAsFactors =FALSE)

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
nexistpas=FALSE

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
    source('~/pllist/pllist.git/selecttt.R')
    if(destroyed)
      stop('Aborted')
    YesorNo=vall
    if(YesorNo=='YES'){
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
    galert('working')
    dirsx=dirs[basename(dirs) %in% dirpaths]
    shell(paste('dir', 'D:\\PNMTALL',' /S/B/OD >  zz.txt'))
    shell(paste('dir', 'C:\\PNMTALL',' /S/B/OD >> zz.txt'))
    zz1 = readLines('zz.txt')
    zz2 = zz1[which(grepl('.',zz1,fixed = TRUE) &
                      !grepl('RECYCLE|.txt|.RData|RPDN|.tmp',zz1,fixed=TRUE) &
                      toupper(dirname(zz1)) %in% toupper(normalizePath((dirs),winslash = '/')))]
    zz=zz2[toupper(dirname(zz2)) %in% toupper(dirsx)]
    dirtbl=as.data.frame(table(as.character(dirname(zz))))
    nfiles=sum(dirtbl$Freq)
    nf=0
    if (!file.exists(sfname)) {
      nexistpas=TRUE
      writeLines('','allmetadata.txt')
      pb = winProgressBar(title = "R progress bar", label = "",
                          min = 1, max = nfiles, initial = 0, width = 300)
      for(dirpath in dirpaths){
        ng = dirtbl[which(dirpath==basename(as.character(dirtbl$Var1))),'Freq']
        nf = nf+ng
        setWinProgressBar(pb, nf, title = paste(dirpath,'-',ng,';',(nfiles-nf),'files remaining'))
        shell(paste('getm',dirs[basename(dirs) %in% dirpath],' >>  allmetadata.txt')) 
      }
      close(pb)
      dts = file.mtime(zz) # file dates
      #unlink('allmetadata.txt')
    }else{
      load(sfname)
      dto = file.mtime(zz) # new file dates
      dmissing = NULL
      if (len(dts) == len(dto)){
        dmissing = zz[!dto %in% dts] # add records with new date to dmissing
      }
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
    cmdd = "shell('getm D: >> allmetadata.txt',translate=TRUE)"
    fpp = normalizePath(file.path(substr(fmissing[i],1,2),
                                   substr(dirname(fmissing[i]),3,nchar(dirname(fmissing[i]))), basename(fmissing[i])),winslash='/')

    cmdy=sub('getm D:', paste('echo','========', fmissing[i]),cmdd) # write filename to metadata
    suppressWarnings(eval(parse(text = cmdy)))
    cmdx = sub('D:',paste('"',fpp,'"',sep=''),cmdd,fixed=TRUE)
    suppressWarnings(eval(parse(text = cmdx)))
    print(paste('Added   ',fmissing[i],'to allmetadata'))
  }
  am1 = readLines('allmetadata.txt')
  am = am1[!grepl('Ingredients|Pantry|Album Title|Handler|exiftool',am1)]
  am=am[!is.na(am) & nchar(am)>0] # clean up na and empty metadata
  ttl = c(which(substr(am,1,1) == '='),len(am)+1)
  print(paste('missing dups=',sum(duplicated(suppressWarnings(normalizePath(substr(am[ttl],10,1000)))),na.rm = FALSE)))
  extras=am[ttl][duplicated(am[ttl])]
}
###########################################
procExtras=function() {
  if(len(extras)>0){
    exidxs=which(trim(substr(am,10,nchar(am))) %in% trim(substr(extras,10,nchar(extras)))) # extra indices in am[]
    if(len(exidxs)){
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
}
####################################
if(len(extras) | len(fmissing) | !file.exists(sfname)){
  procExtras()
  dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA,DMComment=NA)
  am1 = readLines('allmetadata.txt')
  am = am1[!grepl('Codec|Ingredients|Pantry|Album Title|Handler|exiftool',am1)]
  am=am[!is.na(am) & nchar(am)>0] # clean up na and empty metadata
  ttl = c(which(substr(am,1,1) == '='),len(am)+1)
  ducc=sum(duplicated(suppressWarnings(normalizePath(substr(am[ttl],10,1000)))),na.rm = FALSE)
  if(ducc){
    print(paste(ducc,'Duplicates found'))  
    stop('TERMINATED - DUPLICATES FOUND')
  }
  save(am,ttl,dts,file = sfname)
  print('sfname written')
  
  flds=c(NA,NA,NA,NA,'Title',NA,'Comment','SubTitle',NA,'DM Comment','Description')
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
      fld=flds[as.integer(attributes(regexpr('Title|Comment|Subtitle|DM Comment|Description',tmpp))[1])]
      if(len(fld)==1){
        fld=sub('DM Comment|Description','DMComment',fld) # convert DM Comment field name to disallow spaces
        dfan[i,fld]=tmpp
      }
      j=j+1
    }
  }
  dfan$Title=    trim(sub("Title                           :",'',dfan$Title))
  dfan$Comment=  trim(sub("Comment                         :",'',dfan$Comment))
  dfan$SubTitle= trim(sub("Subtitle                        :",'',dfan$SubTitle))
  dfan$DMComment=trim(sub("DM Comment                      :",'',dfan$DMComment))
  dfan$DMComment=trim(sub("Description                     :",'',dfan$DMComment))
  
  dfan$Title=    gsub("'",'',dfan$Title)
  dfan$Title=    gsub(",",'',dfan$Title)
  dfan$Comment=  gsub("'",'',dfan$Comment)
  dfan$SubTitle= gsub("'",'',dfan$SubTitle)
  dfan$DMComment=gsub("'",'',dfan$DMComment)
  
  dfan[which(nchar(trim(dfan$Title))==0),'Title']=NA
  for(cll in 1:ncol(dfan))
    dfan[which(dfan[,cll]=='NA'),cll]=NA # convert character "NA" to NA
  close(pb)
}
if(nexistpas){ # sfname does not exist
  nexistpas = FALSE
  if(file.exists('dfan.RData')){
    dfanNew=dfan
    load('dfan.RData')
  }else{ # load old dfan to get comments not written into metadata for wmv files
    dfanNew=dfan[0,]
  }
  ################################## GET THE COMMENTS FROM old dfan and merge with newdfan ###############
  dfg=merge(dfan,dfanNew[,c('filename','Comment')],by='filename',all = TRUE)
  dfg[!is.na(dfg$Comment.y),'Comment.x']=dfg$Comment.y[!is.na(dfg$Comment.y)]
  dfan=dfg[,c("filename", "Title", "Comment.x", "SubTitle", "DMComment")]
  names(dfan)=names(dfanNew)
  dfan[which(nchar(trim(dfan$Title))==0),'Title']=NA
  dfan[which(nchar(trim(dfan$DMComment))==0),'DMComment']=NA
  for(cll in 1:ncol(dfan))
    dfan[which(dfan[,cll]=='NA'),cll]=NA # convert character "NA" to NA
  
  dfan$filename = normalizePath(dfan$filename)
  dfanNew$filename=normalizePath(dfanNew$filename)
  dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
  save(dfan,dfg,file='dfan.Rdata')
  print('Dfan.RData written')
}else{
  if(!exists('dfan')){
    load('dfan.RData')
    dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
    dfanNew=dfan
  }
}



tpexist=FALSE
avail=FALSE
changed=FALSE

lnttl='Enter Search Criteria'
dflt = ''
if(file.exists('dfltsave.RData'))
  load('dfltsave.RData')
Passt=FALSE
while(TRUE)
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
  if(!exists('dfanNew')){
    dfanNew=dfan
  }
  dfanx=dfan[file.exists(dfan$filename)&dfan$filename %in% dfanNew$filename,]
  an=paste(ifelse(is.na(dfanx$Title)     ,'', paste('Title: ',dfanx$Title,sep='')),
           ifelse(!is.na(dfanx$SubTitle)&!nchar(dfanx$SubTitle)  ,'', paste('Subtitle: ',dfanx$SubTitle,sep='')),
           ifelse(is.na(dfanx$Comment)|!is.na(dfanx$DMComment),'',    paste('Comment: ',dfanx$Comment,sep='')),
           ifelse(is.na(dfanx$DMComment) ,'', paste('Comment: ',dfanx$DMComment,sep='')))
  
  an=gsub('Title:  ','Title: ',an,ignore.case = TRUE)
  an=sub("Title:NA",'',an)
  an=sub("Title: NA",'',an)
  an=gsub("Title:   ",'',an)
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
    anttl=paste(dfanx$filename,an)
    anttlu=toupper(anttl)
    pnoln=NA
    allc=NULL
    
    for (i in 1:len(srct))
      allc=c(allc,which(grepl(srct[i],anttlu,fixed = TRUE)))
    idxs=as.integer(names(which(table(allc)==len(srct))))
    if(len(idxs)==0)
    {
      print('Non found')
      avail=FALSE
      next
    }
    pnoln=dfanx[idxs,]$filename
    fns = NULL  
  }else
    break

  gdfopen=FALSE
  gdframe = get_list_content(pnoln,an[idxs])
  unsorted=TRUE
  fnames=gdframe[order(gdframe$Date,decreasing = unsorted),]
  fnames$comments=trim(fnames$comments)
  source('~/pllist/pllist.git/testplots.R')
  while(!avail)
  {
    delay500()
    if(isExtant(tab)){
      enabled(tbutton)=(len(svalue(tab))!=0)#CHECKsvalue(tab) S/B global in testplots.R
      enabled(dbutton)=(len(svalue(tab))!=0)
      enabled(mbutton)=(len(svalue(tab))!=0)
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
    print('DFAN CHANGED') # debug only may not need extra print here
    if(nchar(trim(dfan[dfix,'Comment']))==0){
      dfan[dfix,'Comment']=NA
    }else{
      dfan[dfix,'DMComment']=dfan[dfix,'Comment']
    }
    if(file.ext(trim(dfan[dfix,'filename']))%in% c('wmv','flv')){
      gmessage('Cannot write metadata to wmv or flv files')
    }else{
      fnc=normalizePath(dfan[dfix,'filename'],winslash = '/')
      print(paste('Updating Metadata in',fnc))
      cmdd=paste("shell('exiftool -DMComment=",'"',dfan[dfix,'Comment'],'" -Title=" ',
                 dfan[dfix,'Title'],'", -SubTitle=" ',dfan[dfix,'SubTitle'],'" ',fnc,"')",sep='')
      writeLines(cmdd,'Jester.R') 
      print(paste('Added to allmetadata.txt Title:',dfan[dfix,'Title']))
      print(paste('Added to allmetadata.txt Subtitle:',dfan[dfix,'SubTitle']))
      print(paste('Added to allmetadata.txt Comment:',dfan[dfix,'Comment']))
      source('jester.R')
      ttllorig=paste(trim(dfan[dfix,'filename']),'_original',sep='')
      if(file.exists(ttllorig)){
        unlink(ttllorig)
        extras=trim(paste("========",ofn)) # remove old metadata associated with the old file
        procExtras()
      }else
        print(paste('Orig file not found for deletion - could be a WMV file',ttllorig))
    }
    dfan$filename = normalizePath(dfan$filename)
    save(dfan,file='Dfan.RData')
    print('Dfan.Rdata written')
    next #rebuild an from updated dfan
  }
  if(deleted){
    dfan=dfan[rownames(dfan)!=dfix,] # remove deleted file from dfan and rebuild an
    extras=paste("========",ofn) # remove old metadata associated with the old file
    procExtras()
    deleted=FALSE
    dfan$filename = normalizePath(dfan$filename)
    save(dfan,file='Dfan.RData')
    print('Dfan.Rdata written')
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



