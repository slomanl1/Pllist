  shell('dir D:\\ /S/B > zz.txt')
  shell('dir c:\\PNMTALL /S/B >> zz.txt')
  shell('dir E:\\ /S/B > yy.txt')
  zz = readLines('zz.txt')
  yy = readLines('yy.txt')
  unlink('zz.txt')
  unlink('yy.txt')
  copyl = zz[!(substr(zz,2,100) %in% substr(yy,2,100))]
  reml = yy[!(substr(yy,2,100) %in% substr(zz,2,100))]
  unlink(reml,recursive = TRUE)
  ccdirs = sub('D:','E:',na.omit(copyl[file.info(copyl)[,'isdir']]))
  if (len(ccdirs) > 0)
    for (i in 1:len(ccdirs)) {
      dir.create(ccdirs[i])
      print(paste(ccdirs[i],'created'))
    }
  lnc = len(copyl)
  if (lnc > 0)
    for (i in 1:lnc) {
      print(paste('Copying',copyl[i],lnc - i))
      file.copy(copyl[i],sub('D:','E:',copyl[i]))
    }
  shell('xcopy D: E: /S/D/Y/J',intern = TRUE)
  print('Done')
