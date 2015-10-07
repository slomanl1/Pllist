require(gWidgets)
testplots = function(fnames){
  if (!.GlobalEnv$tpexist) {
    .GlobalEnv$avail = FALSE
  dcr = FALSE
  fns=data.frame(fnames,cdts=as.character(file.mtime(fnames)))
  get_list_content <- function(dcr){
    fns[order(fns$cdts,decreasing = dcr),]
  }
  
  windo <- gwindow(title='Sort test',width = 800,parent = c(0,0))
  grp <- ggroup(container = windo)
  ddl <- gtable(get_list_content(FALSE), container = grp, expand = TRUE,multiple = TRUE,
                handler = function(h,...) {
                  print(svalue(h$obj))
                  .GlobalEnv$ssv = svalue(h$obj)
                  .GlobalEnv$avail = TRUE
                }
  )
  
  addHandlerRightclick(
    ddl, handler = function(h,...) {
      if (length(svalue(h$obj)) > 0) {
        msgg = an[ttl][which(grepl(basename(as.character(svalue(h$obj))),an[ttl],fixed = TRUE))]
        msgg = substr(msgg,10,255)
        msgBox(msgg)
      }
    }
  )
  refresh <- gimage(
    "gtk-sort-descending",
    dirname   = "stock",
    container = grp,
    handler   = function(h, ...) {
      .GlobalEnv$dcr = !.GlobalEnv$dcr
      dcr = .GlobalEnv$dcr
      svalue(windo) <- ifelse(dcr,'Descending','Ascending')
      ddl[] <- get_list_content(dcr)
    })
  bg <- ggroup(container = grp)
  addSpring(bg)
  gbutton("dismiss", container = bg, handler = function(h,...) {
    .GlobalEnv$tpexist <- FALSE
    .GlobalEnv$avail = TRUE
    dispose(windo)
  })
}
}
