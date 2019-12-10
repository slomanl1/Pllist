source('~/pllist.git/addStudioToDmfnfo.R') # wrStud
source('~/pllist.git/rmmovname.R')
dd = readLines('~/fns.m3u')
load('~/dfan.rdata')
ee = subset(dfan, dfan$filename %in% normalizePath(dd, winslash = '/'))
if (exists('w'))
  if (isExtant(w))
    dispose(w)
field = select.list(names(ee)[c(2,4,5)], graphics = TRUE)
if (nchar(field) > 0) {
  w <- gwindow(field, visible = FALSE,title = 'Close to ABORT')
  g <- gvbox(container = w)
  l3 <- gedit("From Text",
               editable = TRUE,
               container = g)
  l4 <- gedit("To Text",
               editable = TRUE,
               container = g)
  bb = gbutton(
    'OK',
    cont = g,
    handler = function(h,...) {
      if (!any(grepl(svalue(l3), ee[, field]))){
        galert('FROM text not found')
      }else{
        if(svalue(l4)=='To Text'){
          galert('To Text NOT SELECTED',x=300)
        }else
          gtkMainQuit()
      }
    }
  )
  addHandlerDestroy(w,handler=function(h,...){
    gtkMainQuit()
  })
  
  visible(w) <- TRUE
  gtkMain()
  
  fromfrom = svalue(l3)
  fromfrom=gsub('\\','',fromfrom,fixed=TRUE)
  fromfrom=gsub('[','',fromfrom,fixed=TRUE)
  fromfrom=gsub(']','',fromfrom,fixed=TRUE)
                
  toto = svalue(l4)
  if(isExtant(w)){
    dispose(w)
    
    cmts = ee[, field]
    while (any(grepl(fromfrom, cmts, fixed = TRUE))) {
      cmts = gsub(fromfrom, toto, cmts, fixed = TRUE)
      if(trim(fromfrom)==trim(toto))
        break;
      print('boop')
    }
    #lsst,studio,dmComment=NA,Title=NA,SubTitle=NA
    if(field=='DMComment')
      wrStud(ee$filename, ee$studio, cmts)
    if(field=='Title')
      wrStud(ee$filename,ee$studio, NA,Title=cmts)
    if(field=='SubTitle')
      wrStud(ee$filename, ee$studio, NA,NA,SubTitle=cmts)
  }else{
    galert('WRSTUD ABORTED')
  }
} else{
  galert('ABORTED')
}
