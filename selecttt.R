flavors <- c("NO","YES")
avail=FALSE
vall='NO'
flagg=TRUE
f <- function(h,...) {
  .GlobalEnv$avail=TRUE
  .GlobalEnv$vall=svalue(h$obj)
  .GlobalEnv$flagg=FALSE # if TRUE, indicates xw was destroyed with user close
}

xw <- gwindow("Delete sfname and rebuild?",height = 20)
gp <- ggroup(container=xw)

cb <- gcombobox(flavors, editable=TRUE, container=gp, handler=f)
enabled(cb)=FALSE
addHandlerDestroy(xw, handler = function(h,...) {
  .GlobalEnv$destroyed=.GlobalEnv$flagg
  .GlobalEnv$avail=TRUE
})
cntt=7
delay500()
shell('nircmd win activate title "Delete sfname and rebuild?"')
enabled(cb)=TRUE
focus(cb)=TRUE
enabled(cb)=TRUE
while(!avail)
{
  svalue(xw)=paste("Delete sfname and rebuild?",cntt)
  delay500()
  cntt=cntt-1
  if(cntt==0)
    break
}
if(isExtant(xw))
  dispose(xw)


