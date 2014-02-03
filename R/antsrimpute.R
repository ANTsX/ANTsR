antsrimpute <-function( mydat, FUN=mean, ...) {
  for ( x in 1:ncol(mydat) ) mydat[is.na(mydat[,x]),x]<- FUN(as.numeric(mydat[,x]), na.rm=T, ...)
  mydat
}
