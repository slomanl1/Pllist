  shell('dir D:\\ /S/B > zz.txt')
  shell('dir c:\\PNMTALL /S/B >> zz.txt')
  shell('dir E:\\PNMTALL /S/B > yy.txt')
  zz = readLines('zz.txt')
  yy = readLines('yy.txt')
  unlink('zz.txt')
  unlink('yy.txt')
  copyl1 = zz[!(substr(zz,2,100) %in% substr(yy,2,100))]
  copyl=copyl1[!grepl('RECYCLE',copyl1)] # remove recycle bin entried
  reml = yy[!(substr(yy,2,100) %in% substr(zz,2,100))]
  unlink(reml,recursive = TRUE)
  ccdirs = sub('D:','E:',na.omit(copyl[file.info(copyl)[,'isdir']]),ignore.case = TRUE)
  ccdirs = sub('C:','E:',na.omit(copyl[file.info(copyl)[,'isdir']]),ignore.case = TRUE)
  if (len(ccdirs) > 0)
    for (i in 1:len(ccdirs)) {
      dir.create(ccdirs[i])
      print(paste(ccdirs[i],'created'))
    }
  lnc = len(copyl)
  copyll=sub('D:','E:',copyl,ignore.case = TRUE)
  copyll=sub('C:','E:',copyll,ignore.case = TRUE)
  
  if (lnc > 0)
    for (i in 1:lnc) {
      print(paste('Copying',copyll[i],lnc - i))
      file.copy(copyl[i],copyll[i])
    }
  shell('xcopy D: E: /S/D/Y/J',intern = TRUE)
  shell('bkrpdnclips.bat')
  print('Done')
