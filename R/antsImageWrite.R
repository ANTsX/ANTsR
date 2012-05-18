antsImageWrite <- function( image , filename )
{
  return( .Call( "antsImageWrite", image , filename ) )
}
