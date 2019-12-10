cd('D:/PNMTALL/RPDNClips')
zz=dir()
zx=subset(zz,grepl('bami|BSB|cbph|MOMb|btld|cjbmv|RB|BM|_CB|HBL|Bait|LT-LB|_BD|_BRF|_EBD|_NDB',zz)&grepl('ahb',zz))
ofn=zx
nfn=sub('ahb','ah',ofn)
file.rename(ofn,nfn)
zy=subset(zz,grepl('ahfa',zz))
ofn1=zy
nfn1=sub('ahfa','ahbfa',zy)
file.rename(ofn1,nfn1)




