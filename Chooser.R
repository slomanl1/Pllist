scriptStatsRemoveAll <- "~/Revolution/Stats/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
require(bitops)
source("~/Local.R")
while(TRUE){
  if(all(file.info(dir(pattern='*.wpl',path='c:/my playlists',full.names = TRUE))$mtime <= 
         file.info('~/mfnfo.RData')$mtime))
  {
    if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
      setwd('~/')
      load('mfnfo.RData')
      lsst=sub('        <media src=\"..\\My Videos\\RPDNClips\\','',mfnfo$lsst,fixed=TRUE)
      xx=mfnfo$xx
      selectL = select.list(c('AND','OR','NOT'),graphics=TRUE)
      print(selectL)
      if(nchar(selectL)> 0) { 
        print(nchar(selectL))
        selector = select.list(wpls,multiple=TRUE,graphics=TRUE)
        if (length(selector) > 0) {
          print(selector)
          bits=0
          a=1:length(wpls)
          for (i in 1:length(selector)){
            #b=a[selector[i]==wpls]
            b=which(selector[i]==wpls)
            if (!is.na(b)) 
              bits = bitOr(bits,2^(b-1))
          }
          if (selectL == 'AND')
            flist1 = lsst[bitAnd(xx,bits) == bits]
          else
            if (selectL == 'OR')
              flist1 = lsst[bitAnd(xx,bits) > 0]
          else #NOT
            flist1 = lsst[bitAnd(xx,bits) == 0]
          
          flist2 = flist1[flist1!='']
          
          if(length(flist2)==0)
            print('No Records Found')
          else {
            flist3 = paste(drive,'My Videos/RPDNClips/',flist2,sep='')
            flistn= paste(file_path_sans_ext(flist3),'_New.',file_ext(flist3),sep='')
            flist1=c(flist3[file.exists(flist3)],flistn[file.exists(flistn)])
            fl=data.frame(lsst=basename(flist1),fn=flist1)
            mgno=merge(fl,mfnfo[,c('lsst','mtime')])
            mgn=mgno[order(mgno$mtime),]
            flist=as.character(mgn$fn)
            indxs=regexpr('wpl',wpls)[1:length(wpls)]
            fname=""
            for (i in 1:length(wpls)){
              if (bitAnd(bits,2^(i-1))>0)    
                fname = paste(fname,substr(wpls[i],1,indxs[i]-2),sep='_')
            }
            #m3uname <- paste(pldrive,'My Playlists/',sep='')
            write(flist,paste('~/',selectL,fname,'.M3U',sep='_'))
            write(flist,'~/fns.M3U')
            mpc="shell('mpc-hc64.exe %s ')"
            ss=capture.output(cat(flist,sep='" "'))
            ss=paste('"',ss,sep='')
            cmdd=sprintf(mpc,ss)
            #eval(parse(text=cmdd))
            shell('mpc-hc64.exe c:\\Users\\Larry\\Documents\\fns.m3u')
            #unlink('~/fns.M3U')
          }
        }else{
          break
        }
      }else{
        break
      }
    }
    break
  }else{
    print('new entries found in wpls')
    yn=gconfirm('Run BuildxxALT.R')
    if(yn)
      source('~/pllist.git/BuildxxALT.R')
    else
      break
  }
}
