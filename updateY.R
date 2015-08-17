library(gWidgets)
options(guiToolkit = "RGtk2")   
setwd('~/')
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
#source('~/pllist/pllist.git/testplots.r')
len=function(x) length(x)
guiAdd("myGUI")
volz = 'z'
voly = 'y'
tpexist = FALSE
gdfopen=FALSE
changed=FALSE
ofnx=NULL
EOFN = 'Comment|Title|Sub Title|File Path|Ingredients|Album|File Name|Tracks'

get_list_content <- function (fnx,cmts) data.frame(fnx,cdts=as.character(file.mtime(fnx)),comments=cmts,stringsAsFactors =FALSE)

if (file.exists('D:/PNMTALL')) {
  shell('dir D: | findstr Volume > volz.txt')
  volz = readLines('volz.txt')
  unlink('volz.txt')
  if (grepl('Volume',volz[1]) & !grepl('RPDN',volz[1]))
  {
    vname = paste('D:\\',substr(volz[1],23,100),'.txt',sep = '')
    print(paste('VNAME =',vname))
    sfname = paste(substr(vname,1,nchar(vname) - 4),'.RData',sep = '')
    shell('dir D:\\PNMTALL /S/B/OD > zz.txt')
    shell('dir C:\\PNMTALL /S/B/OD >> zz.txt')
    zz1 = readLines('zz.txt')
    zz = zz1[which(grepl('.',zz1,fixed = TRUE) &
                     !grepl('RECYCLE|txt|RData|RPDN',zz1))]
    if (!file.exists(sfname)) {
      shell('getm D:\\PNMTALL > allmetadata.txt')
      shell('getm C:\\PNMTALL >> allmetadata.txt')
      am1 = readLines('allmetadata.txt')
      am = am1[!grepl('Ingredients|Pantry|Album Title',am1)]
      ttl = which(substr(am,1,1) == '=')
      dts = file.mtime(zz) # file dates
      #unlink('allmetadata.txt')
    }else{
      load(sfname)
      dto = file.mtime(zz) # new file dates
      dmissing = NULL
      if (len(dts) == len(dto))
        dmissing = zz[!dto %in% dts] # add records with new date to dmissing
      xmissing = zz[!basename(zz) %in% basename(am[ttl])]
      missing1 = unique(c(dmissing,xmissing))
      missing = normalizePath(missing1, winslash = "/")
      extras = am[ttl][!basename(am[ttl]) %in% basename(zz)]
      dts = dto # replace old dates
      
      if (len(extras) > 0) {
        print(paste('Removed ',substr(extras,10,1000)))
        ttle = which(!basename(am[ttl]) %in% basename(zz))
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
      if (len(missing) > 0) {
        #        missing=paste('D:\\',substr(missing,4,nchar(missing)),sep='')
        print(paste('Added   ',missing))
        
        for (i in 1:len(missing)) {
          cmdd = "shell('getm D: > metadata.txt')"
          fpp = file.path(substr(missing[i],1,2),
                          substr(dirname(missing[i]),3,nchar(dirname(missing[i]))), basename(missing[i]))
          cmdx = sub('D:',fpp,cmdd)
          suppressWarnings(eval(parse(text = cmdx)))
          amm = readLines('metadata.txt')
          amm = amm[2:len(amm)]
          amm[1] = paste("========",missing[i])
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
    if(!file.exists(sfname)){
      xx=strsplit(an[ttl],paste(EOFN,'|======== ',sep=''))
      dfan=data.frame(filename=NA,Title=NA,Comment=NA,SubTitle=NA)
      for(i in 1:len(xx))
        for(j in 2:5)
          dfan[i,j-1]=xx[[i]][j]
    }
    save(am,an,ttl,dff,dts,dfan,file = sfname)
  }
}

dflt = ''
if(file.exists('dfltsave.RData'))
  load('dfltsave.RData')
emsg = 'OK'
while (TRUE) {
  liner <- dlgInput(paste("Enter search Criteria \n",emsg),default = dflt)$res;
  if (!len(liner)) {
    # The user clicked the 'cancel' button
    cat("OK, No Files Selected\n")
    break
  }
  while (TRUE) {
    if (len(liner) > 0)
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
          fnames2 = sub('v NA','v',fnames1) # remove extra "NA" from missing add bug
          comments=substr(pnoln,ttls,nchar(pnoln))
          gdframe = get_list_content(fnames2,comments)
          #testplots(gdframe)
          fnames=gdframe
          debugSource('~/pllist/pllist.git/testplots.R')
          print('enter sub while')
          while(!.GlobalEnv$changed)
          {};
          print('changed handler (fw)')
          .GlobalEnv$changed=FALSE
          if(len(ofnx)>0){
            print('changed handler (fwofnx>0)')
            nfnx=fwind[,]
            print('changed handler (nfnx created OK')
            nfn=nfnx$fnx
            ofn=ofnx$fnx
            ofc=ofnx$comments
            nfc=nfnx$comments
            idx=.GlobalEnv$idx
            print(paste('ofnx,nfnx',ofnx,nfnx)) ######### debug only
            
            if(dirname(nfn)!=dirname(ofn)){
              print(paste('New dir name',dirname(nfn),'Not equal to old',dirname(ofn)))
              dispose(fwind)
            }else{
              if(length(nfn)>0){
                if(any(nfnx!=ofnx)){ # all fields in DF compared
                  if(ofn!=nfn){
                    if(file.rename(ofn,nfn)){
                      print(paste("file rename successful,old an[ttl][idxs][idx]=",ofn,nfn,an[ttl][idxs][idx]))
                      an[ttl][idxs][idx]=sub(ofn,nfn,an[ttl][idxs][idx],fixed=TRUE) # replace old filename with new filename
                    }else{
                      print(paste("file rename FAILED",ofn,nfn))}
                  }else{
                    an[ttl][idxs][idx]=sub(ofc,nfc,an[ttl][idxs][idx],fixed=TRUE) # replace old comments with new comments
                    save(an,file='AN.RData')
                    .GlobalEnv$renamed = TRUE
                    fnx1 = an[ttl][idxs]
                    ttls = unlist(regexpr(EOFN,fnx1))
                    ttls[ttls < 0] = 500
                    fnx= substr(fnx1,10,ttls - 2)
                    comments=substr(fnx1,ttls,nchar(fnx1))
                    tab=.GlobalEnv$tab
                    tab[,] <- get_list_content(fnx,comments) # refresh gtable(write updated table to tab)
                  }
                }
              }
            }  
            # end of rightclickhandler for gtable (tab)
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
            load('AN.RData')} # an[ttl] has been changed by testplots GUI
        }
        if (len(fns) > 0) { # null HAS LENGTH 0
          dispose(ww)
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
          if(!changed)
            break
        }
      }
  }
}
#explorer /select,d:\PNMTALL\CF-ACM\

