# NetworkCovariance.R
# From - van Wijk, B.C.M., Stam, C.J., Daffertshofer, A.: Comparing brain networks of
# different size and connectivity density using graph theory. PLoS One 5(10) (2010) e13701

NetworkCovariance <- function( n1, n2 )
{
  if ( sum(abs( dim(n1) - dim(n2) ) ) != 0 )
    {
    print( "Inputs must be of same dimension" )
    return(NULL)
    }
  N <- dim(n1)[1]
  
  n1 <- n1 - mean( n1[upper.tri(n1) ] )
  n2 <- n2 - mean( n2[upper.tri(n2) ] )
  
  cov <- 2.0 / (N*(N-1)) * sum( n1[upper.tri(n1)] * n2[upper.tri(n2)] )
  
  return( cov )
}
