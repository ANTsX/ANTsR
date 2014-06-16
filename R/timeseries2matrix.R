timeseries2matrix <- function(img, mask) {
  labs<-sort( unique( mask[ mask > 0.001 ] ) )
  if ( length( labs ) == 1 )
    {
    logmask <- (mask == 1)
    mat <- img[logmask]
    dim(mat) <- c(sum(logmask), dim(img)[length(dim(img))])
    return(t(mat))
    }
  else { # return mean time series in each label
    logmask <- (mask == labs[1] )
    mat <- img[logmask]
    dim(mat) <- c(sum(logmask), dim(img)[length(dim(img))])
    mat<-matrix( apply(mat,FUN=mean,MARGIN=2) , ncol = 1 )
    for ( i in 2:length(labs) )
      {
      logmask <- (mask == labs[i] )
      newmat <- img[logmask]
      dim(newmat) <- c(sum(logmask), dim(img)[length(dim(img))])
      newmat<-matrix( apply(newmat,FUN=mean,MARGIN=2) , ncol=1 )
      mat<-cbind(mat,newmat)
      }
    colnames(mat)<-paste("L",labs)
    return( (mat) )
  }
} 
