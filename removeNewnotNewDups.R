cd('~/')
shell(paste('dir', 'D:\\PNMTALL',' /S/B/OD >  zz.txt'))
zz=readLines('zz.txt')
zx=sub('_New','',zz)
dd=dups(zx)
file.remove(unique(dups(zx)))
