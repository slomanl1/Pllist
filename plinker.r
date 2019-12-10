zz=TRUE
while(zz){
  rr=shell("plink -load Rasp -l root -pw fastsix123 ls /usr/bin | grep greppl",intern = TRUE)
  print(rr)
  if(tail(rr,1)=='greppl'){
    zz=FALSE
  }else{
    shell("plink -load Rasp -l root -pw fastsix123 cp /config/greppl /usr/bin")
    print('LOOP')
  }
}
print('DONE')
