setwd('~/')
scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(tools)
len = function(x) length(x)
vold='c'

if(file.exists('c:/')){
  shell('dir c: | findstr Volume > vold.txt')
  vold=readLines('vold.txt')
  unlink('vold.txt')
  if(grepl('Volume',vold[1]))
  {
    vname=paste('c:\\',substr(vold[1],23,100),'.txt',sep='')
    print(vname)
    sfname=paste(substr(vname,1,nchar(vname)-4),'.RData',sep='')
    shell('dir c:\\RealPlayerDownloads /S/B > dd.txt')
    zz1=readLines('dd.txt')
    zz=zz1[which(grepl('.',zz1,fixed=TRUE)&!grepl('RECYCLE',zz1)&!grepl('crdownload',zz1)&grepl('RealPlayer',zz1))]
    if(!file.exists(sfname)){
      shell('getm c: > allmetadata.txt')
      am=readLines('allmetadata.txt')
      ttl=which(substr(am,1,1)=='=' & grepl('RealPlayer',am))
      #unlink('allmetadata.txt')
    }else{
      load(sfname)
      missing=zz[!basename(zz) %in% basename(am[ttl])]
      print(paste('Added ',missing))
      extras=am[ttl][!basename(am[ttl]) %in% basename(zz)]
      print(paste('Removed ',extras))
      if(len(extras)>0){
        ############## process extras TBD TBD ##################
        ttle=which(!basename(am[ttl]) %in% basename(zz))
        am[ttl[ttle]]=NA # remove extra file names
        if(len(ttle)>1){
          for(kk in 1:(len(ttle)-1)){ # remove comments, title:
            lnn=dff[ttle][kk]
            if(lnn>0){
              for(i in 1:lnn){
                print(am[ttl[ttle[kk]]+i])
                am[ttl[ttle[kk]]+i]=NA
              }
            }
          }
        } 
      }
      if(length(missing)>0){
        missing=paste('z:',substr(missing,4,nchar(missing)),sep='')
        for (i in 1:length(missing)){
          cmdd="shell('getm z: > metadata.txt')"
          fpp=file.path(substr(missing[i],1,2), 
                        substr(dirname(missing[i]),3,nchar(dirname(missing[i]))), basename(missing[i]))
          cmdx=sub('z:',fpp,cmdd)
          suppressWarnings(eval(parse(text=cmdx)))
          amm=readLines('metadata.txt')
          amm=amm[2:length(amm)]
          amm[1]=paste("========",missing[i])
          for(k in 1:length(amm))
            am[length(am)+k]=amm[k]
        }
      }
    }
    am=am[!is.na(am)] # remove NA's
    ttl=which(substr(am,1,1)=='=') # flag title lines with === signs
    an=am
    bs1=an[ttl[1]] # for watch debug only
    
    dff=diff(ttl)-1
    for (i in 1:(length(ttl)-1)){
      if(dff[i]>0){
        for (j in 1:dff[i]){
          bs1=an[ttl[i]] # for watch debug only
          an[ttl[i]]=paste(an[ttl[i]],an[ttl[i]+j])
          an[ttl[i]]=gsub('  ','',an[ttl[i]],fixed=TRUE)
          bs2=an[ttl[i]] # for watch debug only
        }
      }
    }
    save(am,an,ttl,dff,file=sfname)
    writeLines(an[ttl],vname)
  }
}
