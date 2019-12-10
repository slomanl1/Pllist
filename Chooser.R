scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
while(TRUE){
  if(all(file.info(dir(pattern='*.wpl',path='c:/my playlists',full.names = TRUE))$mtime <= 
         file.info('~/mfnfo.RData')$mtime))
  {
    if (file.exists(paste(pldrive,'My Playlists/wa.wpl',sep=""))) {
      setwd('~/')
      load('mfnfo.RData')
      studios=unique(mfnfo$studio)
      lsst=sub('        <media src=\"..\\PNMTALL\\RPDNClips\\','',mfnfo$lsst,fixed=TRUE)
      xx=mfnfo$xx
      selectL = select.list(c('AND','OR','NOT','ONLY'),graphics=TRUE)
      print(selectL)
      if(nchar(selectL)> 0) { 
        print(nchar(selectL))
        selector = select.list(wpls,multiple=(selectL!='ONLY'),graphics=TRUE)
        if (length(selector) > 0) {
          print(selector)
          bits=0
          a=1:length(wpls)
          for (i in 1:length(selector)){
            #b=a[selector[i]==wpls]
            b=which(selector[i]==wpls)
            if (!is.na(b)) 
              bits = bitwOr(bits,2^(b-1))
          }
          if (selectL == 'AND')
            flist1 = lsst[bitwAnd(xx,bits) == bits]
          else
            if (selectL == 'OR')
              flist1 = lsst[bitwAnd(xx,bits) > 0]
          else #NOT
            flist1 = lsst[bitwAnd(xx,bits) == 0]
          
          flist2 = flist1[flist1!='']
          
          if(selectL=='ONLY')
            flist2=subset(lsst,grepl(substr(selector,1,nchar(selector)-4),lsst))
          
          if(length(flist2)==0)
            print('No Records Found')
          else {
            selecstud=select.list(studios,multiple=TRUE,graphics=TRUE,title='Select Studios, Esc/Enter ALL')
            if(len(selecstud)==0)
              selecstud=studios # ALL
            flist3 = paste(drive,'PNMTALL/RPDNClips/',flist2,sep='')
            flistn= paste(file_path_sans_ext(flist3),'_New.',file_ext(flist3),sep='')
            flist1=c(flist3[file.exists(flist3)],flistn[file.exists(flistn)])
            fl=data.frame(lsst=basename(flist1),fn=flist1)
            mgno=merge(fl,mfnfo[mfnfo$studio %in% selecstud,c('lsst','mtime')])
            mgn=mgno[order(mgno$mtime),]
            flist=unique(as.character(mgn$fn))
            which(mfnfo$lsst %in% basename(flist))
            indxs=regexpr('wpl',wpls)[1:length(wpls)]
            fname=""
            for (i in 1:length(wpls)){
              if (bitwAnd(bits,2^(i-1))>0)    
                fname = paste(fname,substr(wpls[i],1,indxs[i]-2),sep='_')
            }
            fname=paste('fns',fname,'.m3u',sep='')
            writeLines(gsub('/','\\\\',flist),fname) # Write playlist
            zz='"C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" %s'
            shell(sprintf(zz,fname))
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
    yn=gconfirm('Run Buildmfnfo.R')
    if(yn){
      source('~/pllist.git/Buildmfnfo.R')
      source('~/pllist.git/makewpls.R')
    }else{
      break
    }
  }
}
