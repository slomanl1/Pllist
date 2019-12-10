lst=ls()
ln=length(lst)
for(i in 1:ln){
  if(typeof(eval(parse(text=lst[i]))) != 'closure' & lst[i]!='i' & lst[i]!='ln' & lst[i]!='lst' ){
    #print(paste('focus(',lst[i],')'))
    eval(parse(text=paste('print(lst[i]);','print(focus(',lst[i],'))')))
  }
}
