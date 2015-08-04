setwd('~/')
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source('~/pllist/pllist.git/testplots.r')
guiAdd("myGUI")
volz = 'z'
voly = 'y'
tpexist = FALSE

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
      am2 = am1[1:length(am1) - 1] # remove last line (RPDN.lnk)
      am = am2[!grepl('Ingredients|Pantry',am2)]
      ttl = which(substr(am,1,1) == '=')
      dts = file.mtime(zz) # file dates
      #unlink('allmetadata.txt')
    }else{
      load(sfname)
      dto = file.mtime(zz) # new file dates
      dmissing = NULL
      if (length(dts) == length(dto))
        dmissing = zz[!dto %in% dts] # add records with new date to dmissing
      xmissing = zz[!basename(zz) %in% basename(am[ttl])]
      missing1 = unique(c(dmissing,xmissing))
      missing = normalizePath(missing1, winslash = "/")
      extras = am[ttl][!basename(am[ttl]) %in% basename(zz)]
      dts = dto # replace old dates
      
      if (length(extras) > 0) {
        print(paste('Removed ',extras))
        ttle = which(!basename(am[ttl]) %in% basename(zz))
        am[ttl[ttle]] = NA # remove extra file names
        if (length(ttle) > 1) {
          for (kk in 1:(length(ttle) - 1)) {
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
      if (length(missing) > 0) {
        #        missing=paste('D:\\',substr(missing,4,nchar(missing)),sep='')
        print(paste('Added ',missing))
        
        for (i in 1:length(missing)) {
          cmdd = "shell('getm D: > metadata.txt')"
          fpp = file.path(substr(missing[i],1,2),
                          substr(dirname(missing[i]),3,nchar(dirname(missing[i]))), basename(missing[i]))
          cmdx = sub('D:',fpp,cmdd)
          suppressWarnings(eval(parse(text = cmdx)))
          amm = readLines('metadata.txt')
          amm = amm[2:length(amm)]
          amm[1] = paste("========",missing[i])
          for (k in 1:length(amm))
            am[length(am) + k] = amm[k]
        }
      }
    }
    asave = am
    am = am[!is.na(am)] # remove NA's
    ttl = which(substr(am,1,1) == '=') # flag title lines with === signs
    an = am
    bs1 = an[ttl[1]] # for watch debug only
    
    dff = diff(ttl) - 1
    dff[length(dff) + 1] = 1
    for (i in 1:(length(ttl))) {
      if (dff[i] > 0) {
        for (j in 1:dff[i]) {
          bs1 = an[ttl[i]] # for watch debug only
          an[ttl[i]] = paste(an[ttl[i]],an[ttl[i] + j])
          an[ttl[i]] = gsub('  ','',an[ttl[i]],fixed = TRUE)
          bs2 = an[ttl[i]] # for watch debug only
        }
      }
    }
    save(am,an,ttl,dff,dts,file = sfname)
    writeLines(an[ttl],vname)
  }
}

if (file.exists('y:/')) {
  shell('dir Y: | findstr Volume > voly.txt')
  voly = readLines('voly.txt')
  unlink('voly.txt')
}
if (sub('Z','Y',volz[1]) == voly[1])
{
  shell('dir z:\\ /S/B > zz.txt')
  shell('dir y:\\ /S/B > yy.txt')
  zz = readLines('zz.txt')
  yy = readLines('yy.txt')
  unlink('zz.txt')
  unlink('yy.txt')
  copyl = zz[!(substr(zz,2,100) %in% substr(yy,2,100))]
  reml = yy[!(substr(yy,2,100) %in% substr(zz,2,100))]
  unlink(reml,recursive = TRUE)
  ccdirs = sub('z:','y:',na.omit(copyl[file.info(copyl)[,'isdir']]))
  if (length(ccdirs) > 0)
    for (i in 1:length(ccdirs)) {
      dir.create(ccdirs[i])
      print(paste(ccdirs[i],'created'))
    }
  lnc = length(copyl)
  if (lnc > 0)
    for (i in 1:lnc) {
      print(paste('Copying',copyl[i],lnc - i))
      file.copy(copyl[i],sub('z:','y:',copyl[i]))
    }
  shell('xcopy z: y: /S/D/Y/J')
  print('Done')
  print(md5sum(sfname))
}else{
  cat('\n')
  cat(paste('\nVolumes in Y and Z do not match',voly,volz,'\n',sep = ''))
}
dflt = ''
emsg = 'OK'
while (TRUE) {
  liner <-
    dlgInput(paste("Enter pno search Criteria \n",emsg),default = dflt)$res;
  if (!length(liner)) {
    # The user clicked the 'cancel' button
    cat("OK, No Files Selected\n")
    break
  }
  while (TRUE) {
    if (length(liner) > 0)
      if (nchar(liner) > 0)
      {
        dflt = liner
        cmd = paste('shell("pno ',liner,'>CAPTUREPNO.TXT")')
        suppressWarnings(eval(parse(text = cmd)))
        pnoln1 = readLines('capturepno.txt')
        unlink('capturepno.txt')
        pnoln = pnoln1[4:length(pnoln1)]
        fns = NULL
        if (!is.na(pnoln[1])) {
          ttls = unlist(regexpr(
            'Comment|Title|Sub Title|File Path|Ingredients',pnoln
          ))
          ttls[ttls < 0] = 500
          fnames1 = substr(pnoln,24,ttls - 2)
          fnames = sub('v NA','v',fnames1) # remove extra "NA" from missing add bug
          testplots(fnames)
          while (!avail) {};# testplots returns ssv global
          fns = ssv
          avail = FALSE
        }
        if (length(fns) > 0) { # null HAS LENGTH 0
          writeLines(fns,'fns.m3u') # Write playlist
          load('headfoot.RData')
          writeLines(as.character(c(
            header,paste('<media src="',fns,'"/>'),footer
          ),sep = ''),'fns.wpl')
          shell("wmplayer c:\\Users\\LarrySloman\\Documents\\fns.wpl")
          emsg = 'OK'
        }else{
          print('Non Found')
          emsg = 'NotFound'
          break
        }
      }
  }
}
#explorer /select,d:\PNMTALL\CF-ACM\
