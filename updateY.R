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

guiAdd("myGUI")
volz = 'z'
voly = 'y'
tpexist = FALSE
gdfopen=FALSE
changed=FALSE
avail=FALSE
renamed=FALSE
ofnx=NULL
ssv=NULL
fmissing=NULL
EOFN = 'Title|Comment|Sub Title|File Path|Ingredients|Album|File Name|Tracks'
unsorted = TRUE
Passt=FALSE

get_list_content <- function (fnx,cmts) data.frame(fnx,cdts=as.character(file.mtime(fnx)),comments=cmts,stringsAsFactors =FALSE)

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
      unlink(sfname)
      dirpaths=select.list(basename(dirs),graphics = TRUE,multiple = TRUE,preselect = basename(dirs))
      if(nchar(dirpaths[1])==0)
        stop('Aborted')
      
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
    #updateList=zz[which(file.mtime(zz) > file.mtime('allmetadata.txt'))]
    if (!file.exists(sfname)) {
      writeLines('','allmetadata.txt')
      for(dirpath in dirpaths){
        print(dirpath)
        shell(paste('getm',dirs[grepl(dirpath,dirs)][1],' >>  allmetadata.txt'))
      }
      am1 = readLines('allmetadata.txt')
      am = am1[!grepl('Ingredients|Pantry|Album Title|Handler|exiftool',am1)][3:len(am1)]
      ttl = which(substr(am,1,1) == '=')
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
      
      if (len(extras) > 0) {
        print(paste('Removed ',substr(extras,10,1000)))
        ttle = which(!suppressWarnings(normalizePath(substr(am[ttl],10,1000))) %in% suppressWarnings(normalizePath(zz)))
        am[ttl[ttle]] = NA # remove extra file names
        if (len(ttle) > 1) {
          for (kk in 1:(len(ttle) - 1)) {
            # remove comments, title:
            lnn = dff[ttle][kk]
            if (lnn > 0) {
              for (i in 1:lnn) {
                print(am[ttl[ttle[kk]] + i])
                am[ttl[ttle[kk]] + i] = NA
              }
            }
          }
        }
      }
      if (len(fmissing) > 0) {
        #        fmissing=paste('D:\\',substr(fmissing,4,nchar(fmissing)),sep='')
        print(paste('Added   ',fmissing))
        
        for (i in 1:len(fmissing)) {
          cmdd = "shell('getm D: > metadata.txt')"
          fpp = file.path(substr(fmissing[i],1,2),
                          substr(dirname(fmissing[i]),3,nchar(dirname(fmissing[i]))), basename(fmissing[i]))
          cmdx = sub('D:',fpp,cmdd)
          suppressWarnings(eval(parse(text = cmdx)))
          amm = readLines('metadata.txt')
          amm = amm[2:len(amm)]
          amm[1] = paste("========",fmissing[i])
          for (k in 1:len(amm))
            am[len(am) + k] = amm[k]
        }
      }
    }
    asave = am
    am = am[!is.na(am)] # remove NA's
    ttl = which(substr(am,1,1) == '=') # flag title lines with === signs
    an = am
    bs1 = an[ttl[1]] # for watch debug only
    
    dff = diff(ttl) - 1
    dff[len(dff) + 1] = 1
    for (i in 1:(len(ttl))) {
      if (dff[i] > 0) {
        for (j in 1:dff[i]) {
          bs1 = an[ttl[i]] # for watch debug only
          an[ttl[i]] = paste(an[ttl[i]],an[ttl[i] + j])
          an[ttl[i]] = gsub('  ','',an[ttl[i]],fixed = TRUE)
          bs2 = an[ttl[i]] # for watch debug only
        }
      }
    }
    if(!file.exists(sfname) | len(fmissing) > 0){
      print('Updating Dfan')
      xxg=strsplit(an[ttl],paste(' ','|======== ',sep=''))
      xxt=strsplit(an[ttl],paste('Title :','|======== ',sep=''))
      xxc=strsplit(an[ttl],paste('Comment :','|======== ',sep=''))
      xxs=strsplit(an[ttl],paste('Sub Title :','|======== ',sep=''))
      dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA)
      for(i in 1:len(xxt)){
        dfan[i,'filename']= ifelse(len(trim(xxg[[i]][2]))==0,'',trim(xxg[[i]][2]))
        dfan[i,'Title']=    ifelse(len(trim(xxt[[i]][3]))==0,'',trim(xxt[[i]][3]))
        dfan[i,'Comment']=  ifelse(len(trim(xxc[[i]][3]))==0,'',trim(xxc[[i]][3]))
        dfan[i,'SubTitle']= ifelse(len(trim(xxs[[i]][3]))==0,'',trim(xxs[[i]][3]))
      }
      dfan[is.na(dfan$Comment),'Comment']=''
      dfan[is.na(dfan$Title),'Title']=''
      dfan[is.na(dfan$SubTitle),'SubTitle']=''
      print('Updating Dfan - DONE')
    }
    save(am,an,ttl,dff,dts,dfan,file = sfname)
  }
}

dflt = ''
if(file.exists('dfltsave.RData'))
  load('dfltsave.RData')
emsg = 'OK'
lnttl='Enter Search Criteria'
while (TRUE) {
  avail=FALSE
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
  }else{
    avail=TRUE
    Passt=FALSE
  }
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
  
  if (len(liner)==0) {
    cat("OK, No Files Selected\n")
    break
  }else{
    if(nchar(liner)==0)
      next
  }
  while (TRUE) {
    if (!is.na(liner))
      if (nchar(liner) > 0)
      {
        dflt = liner
        save(dflt,file='dfltsave.RData')
        srct=unlist(strsplit(toupper(liner),' '))
        anttl=subset(an[ttl],!grepl('.lnk',an[ttl],fixed=TRUE))
        anttlu=toupper(anttl)
        pnoln=NA
        allc=NA
        
        for (i in 1:len(srct))
          allc=c(allc,which(grepl(srct[i],anttlu,fixed = TRUE)))
        pnoln=anttl[as.integer(names(which(table(allc)==len(srct))))] # how many match criteria?
        fns = NULL
        if (!is.na(pnoln[1])) {
          ttls = unlist(regexpr(EOFN,pnoln))
          ttls[ttls < 0] = 500
          fnames1 = substr(pnoln,10,ttls - 2)
          fnames2 = sub('v NA','v',fnames1) # remove extra "NA" from fmissing add bug
          fnames2 = sub('mp4 NA','mp4',fnames2) # remove extra "NA" from fmissing add bug
          fnames2 = sub('mov DM','mov',fnames2)
          comments=substr(pnoln,ttls,nchar(pnoln))
          gdframe = get_list_content(fnames2,comments)
          fnames=gdframe[order(gdframe$cdts,decreasing = unsorted),]
          lnttl='Enter Search Criteria'
          source('~/pllist/pllist.git/testplots.R')
          
          print('enter sub while') # ##################### SUB WHILE #####################
          while(!changed & !avail)
          {
            delay500()
            if(isExtant(tab)){
              enabled(tbutton)=(len(svalue(tab))!=0)
              enabled(dbutton)=(len(svalue(tab))!=0)
            }
          }
          print(paste('changed handler (fw),changed=',changed))
          changed=FALSE
          if(len(ofnx)>0){
            print('changed handler (fwofnx>0)')
            nfnx=fwind[,]
            print('changed handler (nfnx created OK')
            ofnxa=dfan[grepl(fnames[idx,'fnx'],dfan[,'filename'],fixed=TRUE),]
            nfn=trim(nfnx$filename)
            ofn=trim(ofnxa$filename)
            ofc=trim(ofnxa$Comment)
            nfc=trim(nfnx$Comment)
            oft=trim(ofnxa$Title)
            nft=trim(nfnx$Title)
            ofs=trim(ofnxa$SubTitle)
            nfs=trim(nfnx$SubTitle)
            print(paste('ofnx,nfnx',ofnx,nfnx)) ######### debug only
            lnttl='Enter Search Criteria'            
            if(dirname(nfn)!=dirname(ofn)){
              print(paste('New dir name',dirname(nfn),'Not equal to old',dirname(ofn)))
              dispose(fwind)
            }else{
              if(length(nfn)>0){
                if(any(nfnx!=ofnxa)){ # all fields in DF compared
                  if(ofn!=nfn){
                    if(!file.rename(ofn,nfn)){
                      print(paste("file rename FAILED",ofn,nfn))
                      renamed=FALSE ########## KLUDGE, comes here after tab destroyed after change/dissmiss
                      nfc=ofc # prevents comment replace error
                      tix=NA # invalid filename, not in am
                    }else{
                      renamed = TRUE
                      print(paste("file rename successful,old an[ttl][idxs][idx]=",ofn,nfn,an[ttl][idxs][idx]))
                      an[ttl][idxs][idx]=sub(ofn,nfn,an[ttl][idxs][idx],fixed=TRUE) # replace old filename with new filename
                      tix=which(grepl(ofn,am,fixed=TRUE)) 
                      am[tix]=sub(ofn,nfn,am[tix],fixed=TRUE) # replace old filename with new filename in am
                    }
                  }else
                    tix=which(grepl(nfn,am,fixed=TRUE)) # find new file name in am
                  if(ofc!=nfc){
                    renamed = TRUE
                    if(nchar(ofc)>0){
                      an[ttl][idxs][idx]=sub(ofc,nfc,an[ttl][idxs][idx],fixed=TRUE) # replace old comment with new comment
                    }else{
                      an[ttl][idxs][idx]=paste(an[ttl][idxs][idx],' Comment : ',nfc)} # add new comment
                  }
                  if(oft!=nft){
                    renamed = TRUE
                    if(nchar(oft)>0){
                      an[ttl][idxs][idx]=sub(oft,nft,an[ttl][idxs][idx],fixed=TRUE) # replace old Title with new comment
                    }else{
                      an[ttl][idxs][idx]=paste(an[ttl][idxs][idx],' Title : ',nft)} # add new Title
                  }
                  if(ofs!=nfs){
                    renamed = TRUE
                    if(nchar(ofs)>0){
                      an[ttl][idxs][idx]=sub(ofs,nfs,an[ttl][idxs][idx],fixed=TRUE) # replace old sub title with new comment
                    }else{
                      an[ttl][idxs][idx]=paste(an[ttl][idxs][idx],' Title : ',nfs)} # add new sub title
                  }
                  
                  if(ofc!=nfc | oft!=nft | ofs!=nfs | ofn!=nfn){
                    dfix=which(grepl(ofn,dfan$filename,fixed =TRUE))
                    print(paste('dfix=',dfix))
                    dfan[dfix,'filename']=nfn
                    dfan[dfix,'Title']=   nft
                    dfan[dfix,'Comment']= nfc
                    dfan[dfix,'SubTitle']=nfs
                    
                    cmtt=NULL
                    ttll=NULL
                    stll=NULL
                    #tix=which(grepl(ofn,am,fixed=TRUE)) # find file name in am
                    if(nchar(dfan[dfix,'Comment'])){
                      cmtt=paste('-metadata comment=','"', dfan[dfix,'Comment'],'"',sep='')
                      tixc=which(grepl('Comment',am[tix:(tix+2)]) & !any(grepl('========',am[tix:(tix+2)],fixed=TRUE)))
                      if(len(tixc)>0)
                        am[tix+tixc-1]=dfan[dfix,'Comment']
                      else{
                        am = append(am, paste("Comment                         : ",dfan[dfix,'Comment']), after = tix)
                      }
                    }
                    ttll=' '
                    if(nchar(dfan[dfix,'Title'])){ ############ chack here IF REPLACEMENT BLANK TITLE WILL WORK ############
                      ttll=paste('-metadata title=','"',   dfan[dfix,'Title'],'"',sep='')
                      tixc=which(grepl('Title',am[tix:(tix+2)]) & !any(grepl('========',am[tix:(tix+2)],fixed=TRUE)))
                      if(len(tixc)>0)
                        am[tix+tixc-1]=dfan[dfix,'Title']
                      else{
                        am = append(am, paste("Title                         : ",dfan[dfix,'Title']), after = tix)
                      }
                    }
                    if(nchar(dfan[dfix,'SubTitle'])){
                      stll=paste('-metadata subtitle=','"',dfan[dfix,'SubTitle'],'"',sep='')
                      tixc=which(grepl('SubTitle',am[tix:(tix+2)]) & !any(grepl('========',am[tix:(tix+2)],fixed=TRUE)))
                      if(len(tixc)>0)
                        am[tix+tixc-1]=dfan[dfix,'SubTitle']
                      else{
                        am = append(am, paste("Sub Title                         : ",dfan[dfix,'SubTitle']), after = tix)
                      }
                    }

                    if(oft!=nft)                    {
                      if(nchar(nft)==0)
                        nft=" "
                      print('Updating Metadata')
                      cmdd=paste("shell('exiftool -DMComment=",'"',nfc,'" -Title=" ',nft,'" ',nfn,"')",sep='')
                      writeLines(cmdd,'Jester.R') ############TEST############ ADD SUBTITLE and compare old/new logic
                      source('jester.R')
                      ttllorig=paste(nfn,'_original',sep='')
                      if(file.exists(ttllorig))
                        unlink(ttllorig)
                      else
                        print('Orig file not found for deletion')
                    }
                  }
                  save(an,am,dfan,ttl,file='AN.RData')
                  # end of rightclickhandler for gtable (tab)
                }
              }
            }
          }
        }else{
          lnttl='Search Criteria Not Found, re-enter'
          avail=TRUE
        }
        print('enter main while')
        while (!avail & !renamed &!changed) # #################################### MAIN WHILE ##############
        {};# testplots returns ssv global
        print('exit main while')
        fns = ssv
        ssv = NULL #clear bones
        avail = FALSE
        if(renamed){
          renamed = FALSE
          load('AN.RData') # an[ttl] and dfan have been changed by testplots GUI
          save(am,an,ttl,dff,dts,dfan,file = sfname)
        } 
        
        if (len(fns) > 0) { # null HAS LENGTH 0
          dispose(w)
          writeLines(fns,'fns.m3u') # Write playlist
          load('headfoot.RData')
          writeLines(as.character(c(
            header,paste('<media src="',fns,'"/>'),footer
          ),sep = ''),'fns.wpl')
          shell("wmplayer c:\\Users\\LarrySloman\\Documents\\fns.wpl")
          emsg = 'OK'
          
        }else{
          print(paste('Non Found changed=',changed))
          emsg = 'NotFound'
          if(!changed){
            if(exists('fwind')){
              if(isExtant(fwind))
                dispose(fwind)
            }
            break
          }
        }
      }
  }
}


##################################### MODEL SHELL COMMAND ##############################################
#shell('c:/users/LarrySloman/documents/hexdump/bin/ffmpeg.exe -i D:/PNMTALL/ndbmjaw/9999_01_hdblpnsustpswa+.mov xx.mp4',translate=TRUE)
#unlink('~/xx.mp4')
#cmdd=paste("shell('c:/users/LarrySloman/documents/hexdump/bin/ffmpeg.exe -i",
#           nfn,ttll,cmtt,stll,"xx.mp4'",",translate=TRUE)")


