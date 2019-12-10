shell('dir E:\\PNMTALL\\RpdncLIPS>E:\\xx.txt')
print('1')
vv=readLines('E:\\xx.txt')
xx=vv
xx=xx[8:len(xx)]
xc=strsplit(xx,'AM|PM')
print('2')
sz=na.omit(as.integer(gsub(',','',substr(sapply(6:len(xc),function(x) xc[[x]][2]),1,18))))
fns=substr(vv,40,10000)
print('3')
cd('D:/PNMTALL/RPDNClips')
ss=na.omit(file.size(fns))
print('4')
