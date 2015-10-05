flavors <- c("NO","YES")
avail=FALSE
vall='NO'
f <- function(h,...) {
  .GlobalEnv$avail=TRUE
  .GlobalEnv$vall=svalue(h$obj)
}

xw <- gwindow("Delete sfname and rebuild?",height = 20)
gp <- ggroup(container=xw)

cb <- gcombobox(flavors, editable=TRUE, container=gp, handler=f)
addHandlerDestroy(xw, handler = function(h,...) {
  .GlobalEnv$avail=TRUE
#  .GlobalEnv$vall=''
})
cntt=7
delay500()
shell('nircmd win activate title "Delete sfname and rebuild?"')
focus(cb)=TRUE
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

