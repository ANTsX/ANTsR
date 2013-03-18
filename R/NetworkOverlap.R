# NetworkOverlap.R
#  - Essentially the DICE Metric, but for network edge overlap
# n1 - network matrix
# n2 - network matrix
# 
NetworkOverlap <- function( n1, n2 )
{
 
  intersect <- sum( n1[upper.tri(n1)] * n2[upper.tri(n2)] )
  union <- sum( n1[upper.tri(n1)] ) + sum( n2[upper.tri(n2)] )

  overlap <- 0
  if ( union > 0 )
    overlap <- 2 * intersect / union
  
  return( overlap )
}
