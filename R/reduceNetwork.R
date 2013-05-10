# reduceNetwork
# extract a subnetwork by relative or absolute thresholding

reduceNetwork <- function( network, N=0, Threshold=0 )
{
  network <- as.array(network)
  if ( (N > 0) && (Threshold > 0)) {
    warning( "both N and Threshold set, N takes precedence" )
  }

  up <- upper.tri(network)
  thresh <- Threshold  
  if ( N > 0 ) {
    thresh <- sort( network[up], decreasing=TRUE )[N]
  }
  subnet <- network * (network >= thresh )

  nodelist <- which( rowSums(subnet) > 0 )
  subnet <- subnet[nodelist, nodelist]
  
  return( list(network=subnet, nodelist=nodelist ) )
}
