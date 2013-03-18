# getNetwork
getNetwork <- function( tmat, mask, labels, timeStep=1 )
{
  labelmat <- labels2matrix( labels, mask )
  regions <- tmat %*% labelmat
  
  sums <- colSums( regions )
  for ( i in c(1:length(sums) ) )
    {
    if ( sums[i] != 0 )
      {
      regions[,i] = regions[,i] / sums[i]
      }
    }
  corrs <- cor(regions)
  corrs[is.na(corrs)] <- 0.0
  corrs[is.nan(corrs)] <- 0.0
  
  corimg <- as.antsImage( as.matrix(corrs) )
  for ( i in c(1:dim(corrs)[1] ) )
    {
      corimg[i,i] <- 0.0
    }
  
  # view all average time series plot in a single window
  #labelID <- rep( 1:max(as.array(labels)), each=dim(tmat)[1] )
  #times <- rep( timeStep*c(0:(dim(regions)[1]-1)), dim(regions)[2] )
  #dat <- data.frame( signal=as.vector(regions), time=times, id=factor(as.vector(labelID)) )
  #ggplot( data=dat, aes(x=time, y=signal, group=id, colour=id) ) + geom_line() + ylab("regional cbf") + xlab( "time (ms)" )

  return (corimg)
}
