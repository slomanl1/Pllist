library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/Buildmfnfo.R')

setwd('~/')
if(exists('w'))
  if(isExtant(w))
    dispose(w)
if(exists('obj'))
  if(isExtant(obj))
    dispose(obj)

scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones

setwd('D:/PNMTALL/RPDNClips')
load('~/mfnfo.RData')
dd=dir()
if(any(!file.exists(mfnfo$lsst)) | len(dd[which(!dd %in% mfnfo$lsst)])>0){
  print('MFNFO not in sync with files, REBUILD -BuildAlt')
  catt(dd[which(!dd %in% mfnfo$lsst)])
}
cd('~/')

get_list_content <- function (fnx,cmts) data.frame(fnx,Date=as.character(file.mtime(fnx)),Size=ptn(file.size(fnx)),comments=cmts,stringsAsFactors =FALSE)
Passt=TRUE
linerd='&'
extras=NULL
fmissing=NULL
deleted=FALSE
nexistpas=FALSE
fnsave=''
convert=FALSE
Fdate=FALSE
Fmeta=FALSE
rebuild=FALSE
ORflag=FALSE
ANDflag=TRUE
rchecked=FALSE
ssv=NULL
ggp=NULL
destroyed=FALSE
liner='.'
vall='No'
EPasst=FALSE
batchFlag=FALSE
ofxx=0
dfanxT=0
save(liner,ORflag,ANDflag,file='~/liner.RData')
load('~/reserved.RData')
###########################################
procExtras=function() {
  if(len(extras)>0){
    exidxst=trim(substr(am[ttl],10,nchar(am[ttl]))) %in% extras
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
      am=am[!is.na(am)&!grepl('>',am)]
      .GlobalEnv$ttl = which(substr(am,1,1) == '=')
      .GlobalEnv$am = am
      writeLines(am,'allmetadata.txt')
    }
  }
  .GlobalEnv$extras=NULL
}
####################################
while(TRUE){
  print('MAIN LOOP')
  exitF=FALSE
  dirs=dir('D:/PNMTALL',full.names = TRUE)
  dirs=subset(dirs,!grepl('lnk',dirs))
  if (file.exists('D:/PNMTALL')) {
    shell('dir D: | findstr Volume > volz.txt')
    volz = readLines('volz.txt')
    unlink('volz.txt')
    if (grepl('Volume',volz[1]) & !grepl('RPDN',volz[1]))
    {
      vname = paste('C:\\',substr(volz[1],23,100),'.txt',sep = '')
      print(paste('VNAME =',vname))
      sfname = '~/PNMTALL.RDATA'
      if(rebuild){
        rm(dfanNew)
        rm(dfan)
        rm(dfanx)
        fnsave=''
        load('~/liner.RData')
      }
      if(destroyed)
        stop('Aborted')
      YesorNo=vall
      if(YesorNo=='YES'){
        vall='No'
        dirpaths=select.list(basename(dirs),graphics = TRUE,multiple = TRUE,preselect = basename(dirs))
        if(len(dirpaths)==0)
          stop('Aborted')
        unlink(sfname)
        unlink('dfltsave.RData') # clear search selections
        unlink('~/gdframe.RData')
        unlink('~/dfan.RData') # deal with this in dfg merge code below
        save(dirpaths,file='dirpaths.RData')
        rebuild=TRUE
        dirsx=dirs[basename(dirs) %in% dirpaths]
      }else{
        if(YesorNo=='')
          stop('Aborted')
        else
          dirsx=dirs
      }
      galert('working - REBUILD')
      
      shell(paste('dir', 'D:\\PNMTALL',' /S/B/OD >  zz.txt'))
      zz1 = readLines('zz.txt')
      unlink('zz.txt')
      zz2 = zz1[which(grepl('.',zz1,fixed = TRUE) &
                        !grepl('RECYCLE|RPDN',zz1,fixed=TRUE) & 
                        !(file_ext(zz1) %in% c('txt','rar','part','exe','crdownload','msi','pdf')) & (file.size(zz1)>0) &
                        toupper(dirname(zz1)) %in% toupper(normalizePath((dirs),winslash = '/',mustWork=TRUE)))]
      zz=zz2[toupper(dirname(zz2)) %in% toupper(dirsx)]
      dirtbl=as.data.frame(table(as.character(dirname(zz))))
      save(zz,file='~/zz.RData')
      dirs=subset(dirs,dirs%in%dirtbl$Var1)
      dirpaths=basename(dirs)
      nfiles=sum(dirtbl$Freq)
      nf=0
      if (!file.exists(sfname)) {
        nexistpas=TRUE
        writeLines('','allmetadata.txt')
        pb = winProgressBar(title = "R progress bar", label = "",
                            min = 1, max = nfiles, initial = 0, width = 300)
        tfl=0 # total files
        for(dirpath in dirpaths){
          ng = dirtbl[which(dirpath==basename(as.character(dirtbl$Var1))),'Freq']
          
          fnss=dir(dirs[basename(dirs) %in% dirpath])
          fnss=subset(fnss,file_ext(fnss) %in% c('mp4','wmv','flv','asf','avi','mov'))
          tfl=tfl+len(fnss)
          #setWinProgressBar(pb, nf, title = paste(dirs[grepl(dirpath,dirs)][1],'-',ng,';',(nfiles-nf),'files remaining'))
          cx='start /HIGH /B /WAIT c:/users/Larry/Documents/getm.bat %s >> allmetadata.txt'
          cy=sprintf(cx,dirs[basename(dirs) %in% dirpath])
          shell(cy,wait = FALSE)
          while(TRUE){
            xxt=as.integer(shell('grep -c "====" allmetadata.txt',intern = TRUE))
            Sys.sleep(.5)
            setWinProgressBar(pb, xxt, title = paste(dirs[grepl(dirpath,dirs)][1],'-',len(fnss),'   ',nfiles-xxt,'files remaining'))
            if(any(grepl(paste(dirpath,'/',tail(fnss,1),sep=''),shell("tail allmetadata.txt 10",intern = TRUE),fixed = TRUE)))
              break
          }
        }
        close(pb)
        writeLines('','ttt.txt')
        file.append('allmetadata.txt','ttt.txt') # fix incomplete line at EOF
        unlink('ttt.txt')
        dts = file.mtime(zz) # file dates
      }else{
        load(sfname)
        extras=NULL
        dto = file.mtime(zz) # new file dates
        dmissing = NULL
        #if (len(dts) == len(dto))
        {
          mssng=zz[!dto %in% dts]
          if(len(mssng)){
            print(sprintf('Date Differences Detected in %s files',len(mssng)))
            dmissing=mssng # add records with new date to dmissing
          }
        }
        ttl = which(substr(am,1,1) == '=')
        ttl = c(which(substr(am,1,1) == '='),len(am)+1)
        ttl=ttl[which(!is.na(am[ttl]))]
        extras=c(extras,dmissing,substr(am[ttl],10,1000)[which(!file.exists(substr(am[ttl],10,1000)))])
        procExtras()
        xmissing = c(dmissing,zz[! (normalizePath(zz,winslash = '/',mustWork=TRUE)) %in%  
                                   (normalizePath(substr(am[ttl],10,1000),winslash = '/',mustWork=TRUE))])
        fmss = unique((normalizePath(xmissing, winslash = '/',mustWork=TRUE)))
        fmissing=subset(fmss,!grepl('.crdownload|.exe|.msi|.Rhistory',fmss))
        if(len(fmissing)){
          ttime=Sys.time()
          load('~/fmissing.RData')
          tmissing=rbind(tmissing,data.frame(fmissing,mtime=file.mtime(fmissing),ttime,stringsAsFactors = FALSE))
          tmissing=subset(tmissing,!duplicated(fmissing)&file.exists(fmissing)) # removed duplicates and deleted files
          save(tmissing,file='~/fmissing.RData')
          print(paste('Found',len(fmissing),'Files to Add'))
        }
        dts = dto # replace old dates
      }
    } 
  }   
  if (len(fmissing) > 0) {
    for (i in 1:len(fmissing)) {
      cmdd = "shell('getm D: >> allmetadata.txt',translate=TRUE)"
      fpp = normalizePath(file.path(substr(fmissing[i],1,2),
                                    substr(dirname(fmissing[i]),3,nchar(dirname(fmissing[i]))), 
                                    basename(fmissing[i])),winslash = '/',mustWork=TRUE)
      print(paste('Parsing :',(len(fmissing)-i),fpp))
      cmdy=sub('getm D:', paste('echo','========', fmissing[i]),cmdd) # write filename to metadata
      (eval(parse(text = cmdy)))
      cmdx = sub('D:',paste('"',fpp,'"',sep=''),cmdd,fixed=TRUE)
      (eval(parse(text = cmdx)))
      lll=shell('tail allmetadata.txt 20',intern = TRUE)
      pnn=tail(fi('XMP',lll),1) # get studio from XMP Toolkit field
      pnc=tail(fi('DM Comment',lll),1) # get Comment from XMP Toolkit field
      pnt=tail(fi('Title',lll),2)
      pnt=gsub('\r','',pnt)
      if(len(pnt)>0 & grepl('exiftool',pnt[1]))
        print(paste('Title',substr(pnt[2],30,nchar(pnt[2]))))
      if(len(pnn)>0){
        if(!grepl('Image',pnn)&grepl('RPDNC',fpp)){
          print(paste('Studio',substr(pnn,31,nchar(pnn)-1)))
        }
      }
      if(len(pnc)>0){
        print(paste('Comment',substr(pnc,32,nchar(pnc)-1)))
      }
      if(grepl('RPDNC',fpp) & len(pnn)==0 & len(pnc)==0){
        galert('Parse Error, no Studio in clip')
      }
      
    }
    am1 = readLines('allmetadata.txt')
    am = am1[!grepl('Ingredients|Pantry|Album Title|Handler|exiftool|Rhistory',am1)]
    am=trim(am[!is.na(am) & nchar(am)>0] ) # clean up na and empty metadata)
    ttl = c(which(substr(am,1,1) == '='),len(am)+1)
    ttl=ttl[which(!is.na(am[ttl]))]
    ducheck=normalizePath(substr(am[ttl[which(!is.na(am[ttl]))]],10,1000),winslash = '/',mustWork=TRUE)
    dupss=normalizePath(substr(am[ttl],10,1000),winslash='/',mustWork=TRUE)[duplicated(ducheck)]
    if(len(dupss))
      print('dupss found')
    extras=c(extras,dupss)  
  }
  if(rebuild)
    unlink('gdframe.Rdata')
  if(len(extras) | len(fmissing) | !file.exists(sfname) | rebuild){
    rebuild=FALSE
    procExtras()
    amm=tryCatch.W.E(readLines('allmetadata.txt'))
    warn=amm$warning
    am1=amm$value
    if(!is.null(warn))
      print(paste('Warning',warn))
    am = am1[!grepl('Codec|Ingredients|Pantry|Album Title|Handler|exiftool|Rhistory',am1)]
    am=trim(am[!is.na(am) & nchar(am)>0]) # clean up na and empty metadata
    ttl = c(which(substr(am,1,1) == '='),len(am)+1) # add an NA at the end
    ducc=sum(duplicated( (normalizePath(substr(am[ttl[which(!is.na(am[ttl]))]],10,1000),winslash = '/',mustWork=TRUE))),na.rm = FALSE)
    if(ducc){
      print(paste(ducc,'Duplicates found'))  
      stop('TERMINATED - DUPLICATES FOUND')
    }
    am=gsub('\\','/',am,fixed=TRUE) # fix improper slash from getm
    am=gsub(reserved,'',am,ignore.case = TRUE)
    writeLines(am,'allmetadata.txt')
    save(am,ttl,dts,file = sfname)
    print('sfname written1')
    print('Parsing Metadata')
    flds=c(NA,NA,NA,NA,'Title',NA,'Comment','SubTitle',NA,'DM Comment','XMP Toolkit')
    dfan=data.frame(filename=substr(am[ttl],10,nchar(am[ttl])),Title=NA,Comment=NA,SubTitle=NA,DMComment=NA,studio=NA,stringsAsFactors = FALSE)
    pb = winProgressBar(title = "R progress bar", label = "",
                        min = 1, max = length(ttl)-1, initial = 0, width = 300)
    for(i in 1:(len(ttl)-1)){
      setWinProgressBar(pb, i, title = paste('Parsing Metadata', label = ifelse(i>1,dirname(dfan[i-1,'filename']),'')))
      j=1
      while(ttl[i]+j < ttl[i+1]){
        tmpp=am[ttl[i]+j]
        if(is.na(tmpp)){ # indicates terminal condition
          break
        }
        fld=flds[as.integer(attributes(regexpr('Title|Comment|Subtitle|DM Comment|XMP Toolkit',tmpp))[1])]
        if(len(fld)==1){
          fld=sub('DM Comment','DMComment',fld) # convert DM Comment field name to disallow spaces
          fld=sub('XMP Toolkit','studio',fld)
          dfan[i,fld]=tmpp
        }
        j=j+1
      }
    }
    ttl=ttl[which(!is.na(am[ttl]))]
    dfan$Title=    trim(sub("Title                           :",'',dfan$Title))
    dfan$Comment=  trim(sub("Comment                         :",'',dfan$Comment))
    dfan$SubTitle= trim(sub("Subtitle                        :",'',dfan$SubTitle))
    dfan$DMComment=trim(sub("DM Comment                      :",'',dfan$DMComment))
    dfan$studio=   trim(sub("XMP Toolkit                     :",'',dfan$studio))
    
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
      dfan=dfan[dirname(dfan$filename)%in%dirtbl$Var1,]
    }else{ # load old dfan to get comments not written into metadata for wmv files
      dfanNew=dfan[0,]
    }
    ################################## GET THE COMMENTS FROM old dfan and merge with newdfan ###############
    dfg=merge(dfan,dfanNew[,c('filename','Comment')],by='filename',all = TRUE)
    dfg[!is.na(dfg$Comment.y),'Comment.x']=dfg$Comment.y[!is.na(dfg$Comment.y)]
    dfan=dfg[,c("filename", "Title", "Comment.x", "SubTitle", "DMComment","studio")]
    names(dfan)=names(dfanNew)[1:6] # remove description field
    dfan[which(nchar(trim(dfan$Title))==0),'Title']=NA
    dfan[which(nchar(trim(dfan$DMComment))==0),'DMComment']=NA
    for(cll in 1:ncol(dfan))
      dfan[which(dfan[,cll]=='NA'),cll]=NA # convert character "NA" to NA
    
    dfan=dfan[which(file.exists(dfan$filename)),]
    dfanNew=dfanNew[which(file.exists(dfanNew$filename)),]
    dfan$filename    =  (normalizePath(dfan$filename,winslash = '/',mustWork=TRUE))
    dfanNew$filename =  (normalizePath(dfanNew$filename,winslash = '/',mustWork=TRUE))
    dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
    save(dfan,dfg,file='dfan.Rdata')
    print('Dfan.RData written')
  }else{
    if(!exists('dfan')){
      load('dfan.RData')
      dfan=dfan[!duplicated(dfan$filename)&!grepl('_original',dfan$filename),]
      dfanNew=dfan
    }else{
      dfan=dfan[which(file.exists(dfan$filename)),]
      save(dfan,file='dfan.Rdata')
      print('Dfan.RData written') # added to save newly added files to dfan
    }
  }
  if(len(fmissing)){
    fms=dfan[which(dfan$filename %in% normalizePath(fmissing,winslash='/')),]
    if(nrow(fms)){
      fms[is.na(fms)]=''
      for (i in 1:nrow(fms)){
        cat(paste('Added File:       ',fms[i,]$filename,'to metadata\n'))
        if(nchar(trim(fms[i,]$Title)))
          cat(paste('Added Title:    ',fms[i,]$Title,'to metadata\n'))
        if(nchar(trim(fms[i,]$Comment)))
          cat(paste('Added Comment:  ',fms[i,]$Comment,'to metadata\n')) 
        if(nchar(trim(fms[i,]$SubTitle)))
          cat(paste('Added SubTitle: ',fms[i,]$SubTitle,'to metadata\n'))
        if(nchar(trim(fms[i,]$DMComment)))
          cat(paste('Added DMComment: ',fms[i,]$DMComment,'to metadata\n')) 
      }
    }
  }
  tpexist=FALSE
  avail=FALSE
  changed=FALSE
  trimmed=FALSE
  
  lnttl='Enter Search Criteria, Close to Exit'
  dflt = ''
  dfltidx=1
  if(file.exists('dfltsave.RData'))
    load('dfltsave.RData')
  
  while(TRUE)
  {
    if(!Passt){
      if(exists('gxy'))
        rm(gxy)
      
      nxflag=FALSE
      ORflag=FALSE
      exitF=FALSE
      liner=dflt[1]
      linerw=gwindow(height = 20, title=lnttl,visible = FALSE,parent=c(0,200))
      ggp=ggroup(cont=linerw)
      obj =  gcombobox(dflt, editable=TRUE, container = ggp,handler=function(h,...){
        .GlobalEnv$liner=svalue(obj)
      })
      getToolkitWidget(linerw)$move(0,0)
      
      ALLButton=gbutton("ALL", container = ggp, handler = function(h,...) {
        .GlobalEnv$nxflag=TRUE
        dispose(linerw)
        .GlobalEnv$liner='.'
        gtkMainQuit()
      })
      
      ANDButton=gbutton("AND", container = ggp, handler = function(h,...) {
        .GlobalEnv$nxflag=TRUE
        .GlobalEnv$ANDflag = TRUE
        dispose(linerw)
        gtkMainQuit()
      })
      font(ANDButton) <- c(color="yellow4" , weight="bold") # initial RED to indicate 'AND' condition
      
      ORButton=gbutton("OR", container = ggp, handler = function(h,...) {
        .GlobalEnv$nxflag=TRUE
        .GlobalEnv$ANDflag = FALSE
        .GlobalEnv$ORflag = TRUE
        dispose(linerw)
        gtkMainQuit()
      })
      font(ORButton) <- c(color="blue", weight="bold") # initial 
      
      EXITButton=gbutton("-EXIT-", container = ggp, handler = function(h,...) {
        .GlobalEnv$exitF=TRUE
        .GlobalEnv$ANDflag = FALSE
        .GlobalEnv$ORflag = FALSE
        .GlobalEnv$liner=NULL
        dispose(linerw)
      }) 
      font(EXITButton) <- c(color="red", weight="bold") # initial 
      
      RBButton=gbutton("REBUILD", container = ggp, handler = function(h,...) {
        .GlobalEnv$nxflag=TRUE
        .GlobalEnv$rebuild=TRUE
        .GlobalEnv$ANDflag = FALSE
        .GlobalEnv$ORflag = FALSE
        .GlobalEnv$gxy='' #prevent searching for popup
        .GlobalEnv$liner=NULL # prevent search
        .GlobalEnv$Passt=TRUE
        .GlobalEnv$EPasst=FALSE
        dispose(linerw)
        gtkMainQuit()
      }) 
      font(RBButton) <- c(color="springgreen4", weight="bold") # initial 
      SKButton=gbutton("SCRATCH", container = ggp, handler = function(h,...) {
        .GlobalEnv$nxflag=TRUE
        .GlobalEnv$rebuild=TRUE
        .GlobalEnv$vall='YES'
        .GlobalEnv$gxy='' #prevent searching for popup
        .GlobalEnv$liner=NULL # prevent search
        dispose(linerw)
      })
      
      .GlobalEnv$eebutton=gbutton("EmptyTrash",cont=ggp,handler = function(h,...){
        #suppressWarnings( shell('explorer ::{645FF040-5081-101B-9F08-00AA002F954E}'))
        enabled(eebutton)=FALSE
        galert('Getting recycle bin contents')
        source('~/pllist.git/EXTRACTrecyc.r')
        if(confirmed){
          shell('nircmd emptybin')
          file.remove(dd[file.exists(dd)])
          galert('RECYCLE Bin Emptied',y=200)
        }else{
          enabled(eebutton)=TRUE
        }
      })
      
      enabled(eebutton)=FALSE
      shell('nircmd win activate title "Enter Search Criteria"')
      
      addHandlerKeystroke(linerw, handler=function(h,...){
        if(h$key=='\r'){
          
          .GlobalEnv$ANDflag = TRUE
          .GlobalEnv$nxflag=TRUE
          dispose(linerw)
          gtkMainQuit()
        }
      })
      
      addHandlerDestroy(linerw, handler=function(h,...){
        if(!.GlobalEnv$nxflag){
          .GlobalEnv$exitF=TRUE # destroyed by user close, not linerw dispose
          .GlobalEnv$liner=NULL
        }
        gtkMainQuit()
      })
      visible(linerw)=TRUE
      focus(linerw)=TRUE
      
      FUN <- function(data) {
        dd='c:/'
        rryy=0
        cc=shell('dir C:\\$recycle.bin /S/B/A',intern = TRUE)
        dd=shell('dir D:\\$recycle.bin /S/B/A',intern = TRUE)
        rrxx=file.size(cc[file.access(cc,4)>-1])
        rryy=file.size(dd[file.access(dd,4)>-1])
        if(exists('eebutton',envir=.GlobalEnv)){
          Sys.sleep(.5)
          if(isExtant(.GlobalEnv$eebutton)){
            tryCatch.W.E({enabled(.GlobalEnv$eebutton)=(sum(rrxx)>129 | sum(rryy)>129)})
          }
        }
      }
      a <- gtimer(250, FUN)
      gtkMain()
      a$stop_timer()
      rm('a')
      
      if(!is.null(liner)){
        if(liner=='')
          liner='.'
        if(ANDflag)
          linerd=gsub(' ',' & ',liner)
        else
          linerd=gsub(' ',' | ',liner)
        if(!batchFlag)
          gxy=galert(paste('Searching for',linerd),delay=1000)
        Sys.sleep(.3)
        if(exists('w'))
          if(isExtant(w))
            dispose(w)
        tpexist=FALSE
      }
    }else{
      Passt=EPasst
    }
    ################ REBUILD an from dfan ################
    if(!exists('dfanNew')){
      dfanNew=dfan
    }else{
      if(nrow(dfanNew)==0){
        dfanNew=dfan
      }
    }
    dfan=dfan[which(file.exists(dfan$filename)),]
    dfan[grepl('ExifTool|XMP',dfan$studio),'studio']=''
    dfanNew=dfanNew[which(file.exists(dfanNew$filename)),]
    dfanx=dfan[dfan$filename %in% dfanNew$filename,]
    
    
    makeAN=function(dfanx){
      dfanx[which(dfanx$DMComment==dfanx$studio),'DMComment']='' # remove duplicate studio
      an=paste(ifelse(is.na(dfanx$Title)     ,'', paste('Title: ',dfanx$Title,sep='')),
               ifelse(!is.na(dfanx$SubTitle)&!nchar(dfanx$SubTitle)  ,'', paste('Subtitle: ',dfanx$SubTitle,sep='')),
               ifelse(is.na(dfanx$Comment)|!is.na(dfanx$DMComment)   ,paste('Comment: ',dfanx$DMComment,sep=''),''),
               ifelse(is.na(dfanx$DMComment) | nchar(dfanx$studio,keepNA = FALSE)>0,'', paste('Comment: ',dfanx$DMComment,sep='')),
               ifelse(is.na(dfanx$studio) | nchar(dfanx$studio,keepNA = FALSE)==0   ,'', paste('studio: ',dfanx$studio,sep='')))
      
      an=gsub('Title:  ','Title: ',an,ignore.case = TRUE)
      an=sub("Title:NA",'',an)
      an=sub("Title: NA",'',an)
      an=gsub("Title:   ",'',an)
      #an=gsub(',','',an)
      an=gsub('Comment:  ','',an)
      an=gsub('Comment: ','',an)
      an=gsub("Comment: NA",'',an)
      an=gsub('Subtitle:  ','Subtitle: ',an,ignore.case = TRUE)
      an=gsub("Subtitle: NA",'',an,ignore.case = TRUE)
      an=gsub("Subtitle:NA",'',an,ignore.case = TRUE)
      an=gsub('NA','',an)
      return(an)
    }
    
    an=makeAN(dfanx)
    if(batchFlag){
      gxy='' #prevent searching for popup
      Passt=TRUE
    }
    
    if (is.null(liner)|batchFlag){
      batchFlag=FALSE
      break
    }
    
    if (nchar(liner) > 0)
    {
      if(!exists('gxy')){
        gxy=galert(paste('Searching for',liner),delay=1000)
        Sys.sleep(.3)
      }
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
    nfnns=NULL
    ndels=NULL
    gdfopen=FALSE
    rbdgf=FALSE
    print('Starting dfan compare')
    if(file.exists('~/gdframe.RData')){
      idxnew=idxs
      dfanN=dfan
      load('~/gdframe.RData') # load dfan, gdframe
      if(!identical(dfanN,dfan)){
        nfnns=dfanN[!dfanN[,'filename'] %in% dfan[,'filename'],'filename'] # get newly added files
        ndels=dfan[!dfan[,'filename'] %in% dfanN[,'filename'],'filename'] # deleted files
        
        if(nrow(dfanN)==nrow(dfan)){
          print(system.time({
            dfanN1=dfanN[order(dfanN$filename),]
            dfan1=dfan[order(dfan$filename),]
            xx=NA
            dx=dfan1[,2:5]
            dy=dfanN1[,2:5]
            print(system.time({xx=sapply(1:nrow(dfan),function(x){ return(identical(dx[x,],dy[x,]))})}))
            idxnst=which(!xx)
          }))
        }else{
          idxnst=NULL
        }
        if(len(idxnst)){
          idxns=order(dfanN1$filename)[idxnst] # reorder indices to correspond with ordered (by filename) dfan/dfanN
          print(sprintf('Found %d files changed to update gdframe',len(idxnst)))
          idxns=unique(idxns)
          idxg=which(gdframe$fnx %in% dfan1[idxns,'filename']) # now calculate corresponding file name indies in gdframe
          gdframe[idxg,]=get_list_content(dfanN1[idxns,'filename'],makeAN(dfanN1[idxns,]))
          dfan1[idxns,]=dfanN1[idxns,]
          dfan=dfan1
          dfanN=dfanN1
        }
      }
      nfnns=nfnns[!nfnns %in% gdframe$fnx] # remove already added new files ( from filename edits)
      if(len(nfnns)){ # check for additions
        print(sprintf('Found %d files to add to gdframe',len(nfnns)))
        gdframe=rbind(gdframe,get_list_content(nfnns,'')) #add new files added
        dfan=dfanN
      }
      if(len(ndels)){ # check for deletions
        print(sprintf('Found %d files to delete from gdframe',len(ndels)))
        gdframe=gdframe[!gdframe$fnx %in% ndels,]
        dfan=dfanN
      }
      if(!all(idxnew %in% idxs)){
        idxs=idxnew
        rbdgf=TRUE
      }
    }else{
      rbdgf=TRUE
    }  
    if(rbdgf){
      if(exists('gxy'))
        if(isExtant(gxy))
          dispose(gxy)
      galert('Building gdframe',delay=8)
      if(exists('stater')){
        if(isExtant(stater))
          svalue(stater)='BUILDING GDFRAME'
      }
      print(system.time({gdframe = get_list_content(pnoln,an[idxs])}))      
    }
    
    save(dfan,gdframe,idxs,file='~/gdframe.RData')
    unsorted=TRUE
    fnames=gdframe[which(gdframe$fnx %in% pnoln),]
    fnames$comments=trim(fnames$comments)
    fnames$sell=''
    fnames[fnames$fnx==fnsave,'sell']='++++'
    fnames=fnames[,c(5,1,2,3,4)]
    fnames=fnames[order(paste(fnames$sell,fnames$Date),decreasing = unsorted),]
    
    tryCatch.W.E( source('~/pllist.git/testplots.R')) ############ TESTPLOTS CALLED HERE ########
    print('Testplots exited')
    fnsave=ssv
    bidx=0
    if(batchFlag){
      svalue(stater)='PROGRAMMING'
    }
    while(TRUE){
      if(batchFlag){
        bidx=bidx+1
        svalue(stater)=paste('PROGRAMMING',nrow(Batch)-bidx)
        fwind=Batch[bidx,1:5]
        dfix=Batch[bidx,'dfix']
        ofn=Batch[bidx,'ofn']
        mtme=Batch[bidx,'ofmtime']
        if(file.exists(fwind$filename)){
          fwind$filename=dfan[dfix,'filename'] # no renamer for trimmed to end
        }
        fwind$filename=normalizePath(fwind$filename,winslash = '/',mustWork = FALSE)
        changed=TRUE
      }
      renamed=FALSE
      if(changed){
        Passt=TRUE
        if(fwind[,'filename']!=dfan[dfix,'filename'] & file.exists(dfan[dfix,'filename'])){
          if(!file.rename(dfan[dfix,'filename'],fwind[,'filename'])){ 
            print(paste("file rename FAILED from",dfan[dfix,'filename'],"to",fwind[,'filename']))
            nextflag=FALSE
            break
          }else{
            print(paste("file rename SUCCESSFUL from",dfan[dfix,'filename'],"to",fwind[,'filename']))
            renamed=TRUE
          }
        }
        if(!identical(trim(fwind[,1:5]),dfan[dfix,c(1:4,6)]) | all(dfan[dfix,'DMComment'] != fwind[,'Comment'],na.rm=TRUE)){
          dfan[dfix,1:4]=trim(fwind[,1:4]) # replace dfan with new changes
          print(paste('DFAN CHANGED',dfan[dfix,'filename'])) # debug only may not need extra print here
          if(identical(nchar(trim(dfan[dfix,'Comment'])),0)){ # indicates comment cleared by user edit
            dfan[dfix,'Comment']=NA
            dfan[dfix,'DMComment']=NA
          }else{
            dfan[dfix,'DMComment']=dfan[dfix,'Comment']
          }
          if(file_ext(trim(dfan[dfix,'filename']))%in% c('wmv','flv')){
            #gmessage('Cannot write metadata to wmv or flv files')
            fnxz=sub(file_ext(dfan[dfix,1]),'mp4',dfan[dfix,'filename'])
            file.rename(dfan[dfix,1],fnxz)
            fnc=normalizePath(fnxz,winslash = '/',mustWork=TRUE)
          }else{
            if(file.exists(dfan[dfix,'filename'])){
              fnc=normalizePath(dfan[dfix,'filename'],winslash = '/',mustWork=TRUE)
            }else{
              fnc=normalizePath(fnx,winslash = '/',mustWork=TRUE)
            }
          }
          print(paste('Updating Metadata in',fnc))
          cmdd=sprintf('exiftool -DMComment="%s" -Title="%s", -SubTitle="%s" -XMPToolkit="%s" %s',
                       dfan[dfix,'Comment'],dfan[dfix,'Title'],dfan[dfix,'SubTitle'],fwind$studio, fnc)
          shell(cmdd)
          ttllorig=paste(fnc,'_original',sep='')
          if(file.exists(ttllorig)){
            print(paste('Added to allmetadata.txt Title:',dfan[dfix,'Title']))
            print(paste('Added to allmetadata.txt Subtitle:',dfan[dfix,'SubTitle']))
            print(paste('Added to allmetadata.txt Comment:',dfan[dfix,'Comment']))
            unlink(ttllorig)
            renamed=file.rename(fnc,dfan[dfix,1]) # back to original name (.wmv..)
          }else{
            print(paste('Orig file not found for deletion - could be a WMV or flv file',ttllorig))
          }
          if(grepl('RPDNClips',fwind$filename) & !identical(dfan[dfix,'studio'],fwind$studio)){
            fwdir=dirname(dfan[dfix,'filename'])
            fn=cleanwplsfn(dfan[dfix,'filename'])$fname
            poss=regexpr('_',fn)
            if(poss>0){  ############## REMOVE oLD STUDIO HERE
              fn=substr(fn,1,poss-1)
              fn=paste(fn,'.',file_ext(fwind$filename),sep='')
            }
            filename=paste(fwdir,'/',file_path_sans_ext(fn),'_',fwind$studio,'.',file_ext(fn),sep='')
            dfan[dfix,'studio']=fwind$studio
            print(paste('Adding to allmetadata.txt studio:',dfan[dfix,'studio']))
            renamed=file.rename(dfan[dfix,'filename'],filename) 
            dfan[dfix,'filename']=filename
          }else{
            filename=dfan[dfix,'filename']
          }
          dx=data.frame(dtn=NA,fn=NA,times=NA)
          dx$dtn=as.POSIXlt(mtme)+(3600*7) # from testplots changed handler (corrected for GMT differential)
          dx[1,'fn']=normalizePath(as.character(filename),winslash = '/',mustWork=TRUE)
          dx[1,'times']=paste('Y:',getYear(dx$dtn),' M:',getMonth(dx$dtn),' D:',getDay(dx$dtn),' H:',as.POSIXlt(dx$dtn)$hour,
                              ' I:',as.POSIXlt(dx$dtn)$min,' S:' ,as.POSIXlt(dx$dtn)$sec,sep='')
          cmd=paste('shell(','"fdate',dx$fn,dx$times,'")')
          eval(parse(text=cmd))
          dfan$filename = normalizePath(dfan$filename,winslash = '/',mustWork=FALSE)
          save(dfan,file='Dfan.RData')
          print('Dfan.Rdata written')
          if(renamed | changed){
            dfanNew[dfix,]=dfan[dfix,] # replace old filename with new like in dfan
            extras=normalizePath(ofn,winslash = '/',mustWork = FALSE) # remove old metadata associated with the old file
            procExtras()
            save(am,ttl,dts,file = sfname)
            print('sfname written')
          }
        }
      }
      nextflag=TRUE
      changed=FALSE
      
      if(bidx==nrow(Batch) & batchFlag){ #nrow is zero when no Batch
        batchFlag=FALSE
        Batch=Batch[0,]
        ofxx=0
        svalue(stater)='REBUILDING'
      }
      if(!batchFlag)
        break
    }
    if(nextflag){
      next #rebuild an from updated dfan
    }
    
    if(deleted){
      dfan=dfan[rownames(dfan)!=dfix,] # remove deleted file from dfan and rebuild an
      extras=normalizePath(ofn,winslash = '/',mustWork = FALSE) # remove old metadata associated with the old file
      procExtras()
      deleted=FALSE
      dfan=dfan[which(file.exists(dfan$filename)),]
      dfan$filename = normalizePath(dfan$filename,winslash = '/',mustWork=TRUE)
      save(dfan,file='Dfan.RData')
      print('Dfan.Rdata written')
    }
    fns = ssv
    fnsave=ssv
    ssv = NULL #clear bones
    avail = FALSE
  }
  if(bidx==nrow(Batch) & batchFlag){ #nrow is zero when no Batch
    batchFlag=FALSE
    dispose(galb)
    Batch=Batch[0,]
    ofxx=0
    break
  }
  if(exitF & !rebuild)
    break
}
######################## close all windows (ignore errors) ##################
xx=ls()
classes=sapply(1:length(xx),function(x) eval(parse(text=paste('class(',xx,')')[x])))
wdws1=xx[which(classes=='GWindow')]
wdws=wdws1[sapply(1:length(wdws1),function(x) eval(parse(text=paste('isExtant(',wdws1,')')[x])))]
if(len(wdws))
  classes=sapply(1:length(wdws),function(x) eval(parse(text=paste('dispose(',wdws,')')[x])))
shell('nircmd win close title "Action"')

