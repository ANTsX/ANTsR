imageFileNames2ImageList <- function(x, dim ) {
  if (nargs() == 0) {
    print("Usage:  ilist<-imageFileNames2ImageList( x , imageDimension ) ")
    return(1)
  }
  localreadfun<-function(x){ antsImageRead(x,dim) }
  ilist<-lapply( x, localreadfun )
  return(ilist)
}
