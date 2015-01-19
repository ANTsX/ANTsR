imageFileNames2ImageList <- function(x, dim, verbose=FALSE) {
  if (nargs() == 0) {
    print("Usage:  ilist<-imageFileNames2ImageList( x , imageDimension ) ")
    return(1)
  }
  ilist <- as.list(rep(NA,length(x)))
  for (i in 1:length(x)) {
    if ( i %% 100 == 1 & verbose ) { print(i); print(Sys.time( )) }
    ilist[[i]]<-antsImageRead(x[i],dim)
  }
  return(ilist)
}
