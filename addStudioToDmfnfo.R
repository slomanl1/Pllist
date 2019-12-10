source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/Pllist.git/writeDate.R')
writeStudio = function(mx,doPb=TRUE){
  print(paste('************* writeStudio,mx',mx[1:10,]))
  if(doPb){
    wpbb <- gwindow('writeStudio()',height=30)
    wpb=ggroup(cont=wpbb,horizontal = FALSE)
    pb <- gprogressbar(cont=wpb)
  }
  orggs=dir('D:/PNMTALL/RPDNClips',pattern='*_original',full.names = TRUE)
  orggs=c(orggs,dir('D:/PNMTALL',pattern='*_original',full.names = TRUE,recursive = TRUE))
  orggs=c(orggs,dir('c:/RealPlayerDownloads',pattern='*_original',full.names = TRUE))
  if(len(orggs))
    file.remove(orggs)
  for (i in 1:nrow(mx)){
    if(doPb){
      if(!isExtant(wpb))
      {
        if(gconfirm('WPB Destroyed, EXIT?'))
          return()
      }
      svalue(wpbb)=sprintf('AddStudioToDmfnfo-writing %s of %s in %s',i,nrow(mx),basename(mx[i,'lsst']))
      svalue(pb)=100*(i/nrow(mx)) 
    }
    dtt=file.mtime(mx[i,'lsst']) -7200 # Subtract 2 hours to make MDT
    if(!isDST())
      dtt=dtt+3600 # Add one more hour for Standard Time (MST)
    print(paste('DTT READ:',mx[i,'lsst'],dtt))
    fnc=mx[i,'lsst']
    fnxz=sub(file_ext(fnc),'mp4',fnc)
    file.rename(fnc,fnxz)
    if(!file.exists(fnxz)){
      galert(paste(fnxz,'Not Found, Skipped'))
      next
    }
    fnc=normalizePath(fnxz,winslash = '/',mustWork=TRUE)
    print('Writing Metadata')
    cmdd=sprintf("shell('exiftool -DMComment=\"%s\" -XMPToolkit=\"%s\" -Title=\"%s\" -SubTitle=\"%s\" -GPSLatitude=\"%s\" %s ')",
                 mx[i,'dmComment'],mx[i,'studio'],mx[i,'Title'],mx[i,'SubTitle'],mx[i,'Obn'],fnc)
    
    if(is.na(mx[i,'dmComment'])){
      cmdd=sub('-DMComment=\"NA\"','',cmdd)
    }
    if(is.na(mx[i,'studio'])){
      cmdd=sub('-XMPToolkit=\"NA\"','',cmdd)
    }
    if(is.na(mx[i,'Title'])){
      cmdd=sub('-Title=\"NA\"','',cmdd)
    }
    if(is.na(mx[i,'SubTitle'])){
      cmdd=sub('-SubTitle=\"NA\"','',cmdd)
    }
    if(is.na(mx[i,'Obn'])){
      cmdd=sub('-GPSLatitude=\"NA\"','',cmdd)
    }    
    
    eval(parse(text=cmdd))
    orggs=paste(fnc,'_original',sep='')
    if(!file.exists(orggs))
    {
      print('Retry one/DMComment')
      cmdd=paste("shell('exiftool -DMComment=",'"',mx[i,'studio'],'"  ',fnc,"')",sep='')
      eval(parse(text=cmdd))
      mx[i,'dmComment']=mx[i,'studio']
      
      
      print('Retry one/XMP')
      cmdd=paste("shell('exiftool -XMPToolkit=",'"',mx[i,'studio'],'"  ',fnc,"')",sep='')
      print(cmdd)
      eval(parse(text=cmdd))
    }
    
    .GlobalEnv$verf=FALSE
    cmdv=paste("shell('exiftool -XMPToolkit -DMComment -Title -SubTitle -GPSLatitude %s',intern=TRUE)")
    joe=eval(parse(text=sprintf(cmdv,normalizePath(fnc,winslash = '/'))))
    if(len(joe)>0){
      
      V1=V2=V3=V4=V5=TRUE
      xx=trim(strsplit(joe,':'))
      
      if(!is.na(mx[i,'studio'])&nchar(mx[i,'studio']))
        V1=xx[[which(grepl('XMP Toolkit',xx))]][2]==mx[i,'studio']
      
      if(!is.na(mx[i,'dmComment']) & (len(which(grepl('DM Comment',xx)))))
        V2=xx[[which(grepl('DM Comment',xx))]][2]==trim(mx[i,'dmComment'])
      
      if(!is.na(mx[i,'Title']) & len(which(grepl('Title',xx))))
        V3=xx[[which(grepl('Title',xx))]][2]==trim(mx[i,'Title'])
      
      if(!is.na(mx[i,'SubTitle']) & len(which(grepl('Subtitle',xx))))
        V4=xx[[which(grepl('Subtitle',xx))]][2]==trim(mx[i,'SubTitle'])
      
      if(!is.na(mx[i,'Obn']) & len(which(grepl('Obn',xx))))
        V5=xx[[which(grepl('Obn',xx))]][2]==trim(mx[i,'Obn'])
      
      print(paste('studio Verified:',V1,mx[i,'studio']))
      print(paste('DMComment Verified:',V2,mx[i,'dmComment'])) 
      print(paste('Title Verified:',V3,mx[i,'Title'])) 
      print(paste('SubTitle Verified:',V4,mx[i,'SubTitle']))
      print(paste('Obn Verified:',V5,mx[i,'Obn']))
      .GlobalEnv$verf=V1 & V2 & V3 & V4 & V5
    }
    
    if(verf){
      file.rename(fnc,mx[i,'lsst'])
      orggs=paste(fnc,'_original',sep='')
      zz=0
      while(!file.exists(orggs)){
        Sys.sleep(.5)
        zz=zz+1
        print(paste('waiting for original file',zz))
        if(zz==5){
          verf=FALSE
          break
        }
      }
    }
    if(!verf){
      msg=paste('WRITE FAILED', mx[i,'lsst'],mx[i,'studio'])
      print(msg)
      galert(msg,30)
      write(mx[i,'lsst'],file="~/writeErrorLog.txt",append=TRUE) # write to log
      print(paste('Write of studio', mx[i,'lsst'],mx[i,'studio'],'FAILED'))
      orggs=dir(paste(fnc,'_original',sep=''))
      if(len(orggs)==0){
        orggs=fnc
      }
      nfn=mx[i,'lsst']
      stt=file.rename(orggs,nfn)
      if(file.exists(fnc))
        file.remove(fnc)
      if(stt | file.exists(nfn)){
        galert('Original file restored success')
        print('Original file restored success')
      }else{
        galert('Original FAILED to restore')
        browser()
      }
    }else{
      file.remove(orggs)
      WriteDate(as.character(mx[i,'lsst']),as.character(dtt+2)) # add two seconds to differentiate file time comparison
    }
#     oggg=dir('D:/PNMTALL', pattern='_original',recursive = TRUE)
#     ssm=sum(grepl('_original',oggg))
#     if(ssm>0){
#       print(paste(ssm,'Orignals found'))
# #      browser()
#    }
  }
  if(doPb)
    dispose(pb)
}

getWplsXX=function(fn,mfnfoL){
  extx=gsub('.wpl','',wpls,fixed=TRUE)
  exts1=c('ah.mp4','blah.mp4','bfa.mp4','ussf.mp4','dd.mp4','bl.mp4','cs.mp4',
          'ussfd.mp4','pn.mp4','blfd.mp4','utpfd.mp4','ussfa.mp4','swa.mp4','uwa.mp4','utp.mp4','sbd.mp4','vva.mp4','su.mp4','stp.mp4','so.mp4')
  exts=gsub('.mp4','',exts1,fixed=TRUE)
  for(fnx in fn){
    exx=exts[unlist(sapply(exts,function(x) gregexpr(x,fnx)))>0]
    exx=exx[!is.na(exx) & exx!='blfd']
    bb=which(extx %in% exx)
    bits=0
    if(len(bb)>0){
      for(i in 1:len(bb)){
        bits=bitwOr(bits,(2^(bb[i]-1)))
      }
    }
    mfnfoL[mfnfoL$lsst==fnx,'xx']=bits
  }
  return(mfnfoL)
}

wrStud = function(lsst,studio,dmComment=NA,Title=NA,SubTitle=NA,Obn=NA){
  #print(paste('***************** wrStud',lsst,studio,dmComment,Title))
  studioC=NA
  dmCommentC=NA
  SubTitleC=NA
  TitleC=NA
  ObnC=NA
  
  for(k in 1:len(lsst)){
    print(paste(k,'-',lsst[k]))
    # if(hasMovieName(lsst[k]))
    #   rmmovname(lsst[k],FALSE)
    studioC[k]=   ifelse(is.na(studio[k]),NA,trim(as.character(studio[k])))
    dmCommentC[k]=ifelse(is.na(dmComment[k]),NA,trim(as.character(dmComment[k])))
    SubTitleC[k]= ifelse(is.na(SubTitle[k]),NA,trim(as.character(SubTitle[k])))
    ObnC[k]     = ifelse(is.na(Obn[k]),NA,trim(as.character(Obn[k])))
    #browser()
    TitleC[k]=    ifelse(is.na(Title[k]),NA,sub(' & ',' and ',trim(as.character(Title[k])),fixed = TRUE))
    TitleC[k]=    ifelse(is.na(TitleC[k]),NA,sub(' And ',' and ',trim(as.character(TitleC[k])),fixed = TRUE))
  }
  mx=data.frame(lsst=as.character(lsst),studio=studioC,dmComment=dmCommentC,
                Title=TitleC,SubTitle=SubTitleC,Obn=ObnC,stringsAsFactors = FALSE)
  writeStudio(mx,TRUE)
  return(.GlobalEnv$verf)
}

