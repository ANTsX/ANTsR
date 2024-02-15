#' Minimal function for creating r images
#'
#' r16, r27, r30, r62, r64, r85 are t1-weighted MRI slices of the 
#' human brain that have been around for a long time.  We use them 
#' for quick registration, segmentation and other processing tests.
#' This function will return these images to the user.
#'
#' @param x one of 16, 27, 30, 62, 64, 85 or 'all'. values 1 through 6 are also acceptable.
#' @return image or image list is output
#' @author Avants BB
#' @examples
#'
#' r16 = ri( 16 )
#'
#' @export ri
ri <- function( x ) {
  if ( x != 'all' ) {
    if ( x <= 6 ) x = c( 16, 27, 30, 62, 64, 85 )[x]
    img = antsImageRead( getANTsRData( paste0( "r", x ) ) ) 
    return( img )
  } else {
  ilist = list( )
  for ( x in c( 16, 27, 30, 62, 64, 85 ) ) {
    ilist = lappend( ilist, antsImageRead( getANTsRData( paste0( "r", x ) ) ) )
    }
  return( ilist )
  }
  
}

