antsImageClone <- function( in_image , out_pixeltype )
{
  .Call( "antsImageClone", in_image , out_pixeltype )
}