imageFileNames2ImageList <- function(x, dim) {
  if (nargs() == 0) {
    print("Usage:  ilist<-imageFileNames2ImageList( x , imageDimension ) ")
    return(1)
  }
  ilist <- list()
  for (i in x) {
    ilist <- lappend(ilist, antsImageRead(i, dim))
  }
  return(ilist)
} 
