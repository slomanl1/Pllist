library(gWidgets)
library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/StartMyGuiTrimmer.R')

setwd('~/')
if(exists('w'))
  if(isExtant(w))
    dispose(w)
if(exists('obj'))
  if(isExtant(obj))
    dispose(obj)

scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
get_list_content <- function (fnx,cmts) data.frame(fnx,Date=as.character(file.mtime(fnx)),Size=prettyNum(as.integer(file.size(fnx)),big.mark = ","),comments=cmts,stringsAsFactors =FALSE)

extras=NULL
fmissing=NULL
deleted=FALSE
nexistpas=FALSE
fnsave=''
convert=FALSE
Fdate=FALSE
rebuild=FALSE
while(TRUE){
  exitF=FALSE
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
      if(!rebuild){
        source('~/pllist.git/selecttt.R')
      }else{
        rm(dfanNew)
        rm(dfan)
        rm(dfanx)
        fnsave=''
      }
      if(destroyed)
        stop('Aborted')
      YesorNo=vall
      if(YesorNo=='gtk-yes'){
        dirpaths=select.list(basename(dirs),graphics = TRUE,multiple = TRUE,preselect = basename(dirs))
        if(len(dirpaths)==0)
          stop('Aborted')
        unlink(sfname)
        unlink('dfltsave.RData') # clear search selections
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
      unlink('zz.txt')
      zz2 = zz1[which(grepl('.',zz1,fixed = TRUE) &
                        !grepl('RECYCLE|.txt|.RData|RPDN|.tmp|.crdownload|.exe|.rar',zz1,fixed=TRUE) &
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
          setWinProgressBar(pb, nf, title = paste(dirs[grepl(dirpath,dirs)][1],'-',ng,';',(nfiles-nf),'files remaining'))
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
        xmissing = zz[!suppressWarnings(normalizePath(zz,winslash = '/')) %in% suppressWarnings(normalizePath(substr(am[ttl],10,1000),winslash='/'))]
        fmss = unique(suppressWarnings(normalizePath(c(dmissing,xmissing), winslash = "/")))
        #fmissing=subset(fmss,!grepl('_New',fmss))
        fmissing=subset(fmss,!grepl('.crdownload|.exe|.msi',fmss))
        extras = am[ttl][!suppressWarnings(normalizePath(substr(am[ttl],10,1000),winslash='/')) %in% suppressWarnings(normalizePath(zz,winslash='/'))]
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
    am=trim(am[!is.na(am) & nchar(am)>0] ) # clean up na and empty metadata)
    ttl = c(which(substr(am,1,1) == '='),len(am)+1)
    extras=c(extras,am[ttl][duplicated(am[ttl])])
  }
  ###########################################
  procExtras=function() {
    if(len(extras)>0){
      exidxst=which(normalizePath(trim(substr(am[ttl],10,nchar(am[ttl]))),winslash = '/') %in% normalizePath(trim(substr(extras,10,nchar(extras))),winslash='/')) # extra indices in am[]
      exidxs=which(am %in% am[ttl][exidxst])      
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
  if(len(extras) | len(fmissing) | !file.exists(sfname) | rebuild){
    rebuild=FALSE
    procExtras()
    dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA,DMComment=NA)
    am1 = readLines('allmetadata.txt')
    am = am1[!grepl('Codec|Ingredients|Pantry|Album Title|Handler|exiftool',am1)]
    am=trim(am[!is.na(am) & nchar(am)>0]) # clean up na and empty metadata
    ttl = c(which(substr(am,1,1) == '='),len(am)+1)
    ducc=sum(duplicated(suppressWarnings(normalizePath(substr(am[ttl],10,1000),winslash = '/'))),na.rm = FALSE)
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
    
    dfan$filename    = suppressWarnings(normalizePath(dfan$filename,winslash = '/'))
    dfanNew$filename = suppressWarnings(normalizePath(dfanNew$filename,winslash = '/'))
    dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
    save(dfan,dfg,file='dfan.Rdata')
    print('Dfan.RData written')
  }else{
    if(!exists('dfan')){
      load('dfan.RData')
      dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
      dfanNew=dfan
    }else{
      save(dfan,file='dfan.Rdata')
      print('Dfan.RData written') # added to save newly added files to dfan
    }
  }
  
  
  
  tpexist=FALSE
  avail=FALSE
  changed=FALSE
  
  lnttl='Enter Search Criteria, Close to Exit'
  dflt = ''
  dfltidx=1
  if(file.exists('dfltsave.RData'))
    load('dfltsave.RData')
  Passt=FALSE
  liner='.'
  ANDflag=TRUE
  
  while(TRUE)
  {
    if(!Passt){
      if(exists('gxy'))
        rm(gxy)
      linerw=gwindow(height = 20, title=lnttl)
      ggp=ggroup(cont=linerw)
      obj =  gcombobox(dflt, editable=TRUE, container = ggp)
      
      ANDButton=gbutton("AND", container = ggp, handler = function(h,...) {
        .GlobalEnv$ANDflag = TRUE
        gtkMainQuit()
      })
      font(ANDButton) <- c(color="yellow4" , weight="bold") # initial RED to indicate 'AND' condition
      .GlobalEnv$ANDflag = TRUE
      
      ORButton=gbutton("OR", container = ggp, handler = function(h,...) {
        .GlobalEnv$ANDflag = FALSE
        gtkMainQuit()
      })
      font(ORButton) <- c(color="blue", weight="bold") # initial 
      
      EXITButton=gbutton("-EXIT-", container = ggp, handler = function(h,...) {
        .GlobalEnv$exitF=TRUE
        dispose(linerw)
      }) 
      font(EXITButton) <- c(color="red", weight="bold") # initial 
      
      RBButton=gbutton("REBUILD", container = ggp, handler = function(h,...) {
        .GlobalEnv$rebuild=TRUE
        dispose(linerw)
        gtkMainQuit()
      }) 
      font(RBButton) <- c(color="springgreen4", weight="bold") # initial 
      
      shell('nircmd win activate title "Enter Search Criteria"')
      
      addHandlerKeystroke(linerw, handler=function(h,...){
        if(h$key=='\r'){
          .GlobalEnv$ANDflag = TRUE
          gtkMainQuit()
        }
      })
      
      addHandlerDestroy(linerw, handler=function(h,...){
        .GlobalEnv$exitF=TRUE
        gtkMainQuit()
      })
      
      liner=NULL
      gtkMain()
      
      if(isExtant(linerw)){
        liner=svalue(obj)
        dispose(linerw)
        if(exists('w'))
          if(isExtant(w))
            dispose(w)
        tpexist=FALSE
      }
    }else
      Passt=FALSE
    if(!exists('gxy') & !exitF){
      gxy=galert(paste('Searching for',liner),delay=1000)
      Sys.sleep(1)
    }
    ################ REBUILD an from dfan ################
    if(!exists('dfanNew')){
      dfanNew=dfan
    }
    dfanNew$filename = normalizePath(dfanNew$filename,winslash = '/')
    dfan$filename = normalizePath(dfan$filename,winslash = '/')
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
      dflt[len(dflt)+1] = liner
      dflt=unique(dflt[nchar(dflt)>0])
      dfltidx=which(dflt==liner)
      if(dfltidx>1)
        dflt=unique(dflt[c(dfltidx,1:len(dflt))])
      dfltidx=which(dflt==liner)
      save(dflt,dfltidx,file='dfltsave.RData')
      srct=unlist(strsplit(toupper(liner),' '))
      anttl=paste(dfanx$filename,an)
      anttlu=toupper(anttl)
      pnoln=NA
      allc=NULL
      
      for (i in 1:len(srct))
        allc=c(allc,which(grepl(srct[i],anttlu,fixed = TRUE)))
      
      if(ANDflag)
        idxs=as.integer(names(which(table(allc)==len(srct)))) # 'AND' condition
      else
        idxs=as.integer(names(which(table(allc)>0))) # 'OR' condition
      
      if(len(idxs)==0)
      {
        galert('Non found')
        if(exists('w'))
          if(isExtant(w))
            dispose(w)
        if(exists('gxy'))
          if(isExtant(gxy))
            dispose(gxy)
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
    fnames=gdframe
    fnames$comments=trim(fnames$comments)
    fnames$sell=''
    fnames[fnames$fnx==fnsave,'sell']='++++'
    fnames=fnames[,c(5,1,2,3,4)]
    fnames=fnames[order(paste(fnames$sell,fnames$Date),decreasing = unsorted),]
    source('~/pllist.git/testplots.R')
    
    if(changed | deleted){
      dfix=which(grepl(getFnx(),dfan[,'filename'],fixed=TRUE))
      ofn=dfan[dfix,'filename']
      dispose(w)
    }
    
    renamed=FALSE
    if(changed){
      if(fwind[,'filename']!=dfan[dfix,'filename']){
        answ=gconfirm('Rename - Are you Sure?')
        if(answ){
          if(!file.rename(dfan[dfix,'filename'],fwind[,'filename'])){ 
            print(paste("file rename FAILED from=",dfan[dfix,'filename'],"to=",fwind[,'filename']))
          }else{
            print(paste("file rename SUCCESSFUL from=",dfan[dfix,'filename'],"to=",fwind[,'filename']))
            renamed=TRUE
          }
        }
      }
      if(!identical(trim(fwind[,1:4]),dfan[dfix,1:4]) | all(dfan[dfix,'DMComment'] != fwind[,'Comment'],na.rm=TRUE)){
        dfan[dfix,1:4]=trim(fwind[,1:4]) # replace dfan with new changes
        print(paste('DFAN CHANGED',dfan[dfix,'filename'])) # debug only may not need extra print here
        if(nchar(trim(dfan[dfix,'Comment']))==0){
          dfan[dfix,'Comment']=NA
        }else{
          dfan[dfix,'DMComment']=dfan[dfix,'Comment']
        }
        if(file_ext(trim(dfan[dfix,'filename']))%in% c('wmv','flv')){
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
          }else
            print(paste('Orig file not found for deletion - could be a WMV file',ttllorig))
        }
        filename=dfan[dfix,'filename']
        dx=data.frame(dtn=NA,fn=NA,times=NA)
        dx$dtn=mtme # from testplots changed handler
        dx[1,'fn']=normalizePath(as.character(filename),winslash = '/')
        dx[1,'times']=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                            ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
        cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
        eval(parse(text=cmd))
      }
      dfan$filename = normalizePath(dfan$filename,winslash = '/')
      if(renamed | changed){
        dfanNew[dfix,]=dfan[dfix,] # replace old filename with new like in dfan
        extras=paste("========",ofn) # remove old metadata associated with the old file
        procExtras()
      }
      save(dfan,file='Dfan.RData')
      print('Dfan.Rdata written')
      changed=FALSE
      next #rebuild an from updated dfan
    }
    if(deleted){
      dfan=dfan[rownames(dfan)!=dfix,] # remove deleted file from dfan and rebuild an
      extras=paste("========",ofn) # remove old metadata associated with the old file
      procExtras()
      deleted=FALSE
      dfan$filename = normalizePath(dfan$filename,winslash = '/')
      save(dfan,file='Dfan.RData')
      print('Dfan.Rdata written')
    }
    fns = ssv
    fnsave=ssv
    ssv = NULL #clear bones
    avail = FALSE
    if (len(fns) > 0) { # null HAS LENGTH 0
      dispose(w)
      tpexist=FALSE
      #writeLines(fns,'fns.m3u') # Write playlist
      load('headfoot.RData')
      writeLines(as.character(c(
        header,paste('<media src="',fns,'"/>'),footer
      ),sep = ''),'fns.wpl')
      #shell("wmplayer c:\\Users\\Larry\\Documents\\fns.wpl")
      mpc="shell('mpc-hc64.exe %s')"
      ss=capture.output(cat(fns))
      cmdd=sprintf(mpc,ss)
      eval(parse(text=cmdd))
      unlink('~/fns.wpl')
      Passt=TRUE
      unsorted=FALSE
      avail=FALSE
      emsg = 'OK'
      
    }
  }
  if(exitF)
    break
}
######################## close all windows (ignore errors) ##################
xx=ls()
classes=sapply(1:length(xx),function(x) eval(parse(text=paste('class(',xx,')')[x])))
wdws1=xx[which(classes=='GWindow')]
wdws=wdws1[sapply(1:length(wdws1),function(x) eval(parse(text=paste('isExtant(',wdws1,')')[x])))]
if(len(wdws))
  classes=sapply(1:length(wdws),function(x) eval(parse(text=paste('dispose(',wdws,')')[x])))
