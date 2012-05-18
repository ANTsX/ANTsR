antsImageRead <- function( filename , pixeltype , dimension )
{
  return( .Call( "antsImageRead", filename , pixeltype , dimension ) )
}
