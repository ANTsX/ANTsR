antsImageRead <- function( filename , pixeltype , dimension  )
{
  img = .Call( "antsImageRead", filename , pixeltype , dimension )
  filter = new( "antsPermuteAxesImageFilter" , inputimage_pixeltype = pixeltype , inputimage_dimension = dimension )
  antsSetInput( filter , img )
  order = as.integer( c( 0 : (dimension-1) ) )
  tmp = order[1]
  order[1] = order[2]
  order[2] = tmp
  antsSetOrder( filter , order )
  antsUpdate( filter )
  return( antsGetOutput( filter ) )
}
