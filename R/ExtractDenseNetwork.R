# ExtractDenseNetwork.R

ExtractDenseNetwork <- function( n1, density )
{
  thresh <- sort( n1[upper.tri(n1)], decreasing=TRUE )[density]
  return( n1 >= thresh )
}
