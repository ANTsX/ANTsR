antsImageWrite <- function( image , filename )
{
  filter = new( "antsPermuteAxesImageFilter" , inputimage_pixeltype = image@pixeltype , inputimage_dimension = image@dimension )
  antsSetInput( filter , image )
  order = as.integer( c( 1 : image@dimension ) )
  tmp = order[1]
  order[1] = order[2]
  order[2] = tmp
  antsSetOrder( filter , order )
  antsUpdate( filter )
  .Call( "antsImageWrite", antsGetOutput( filter ) , filename ) ;
}
