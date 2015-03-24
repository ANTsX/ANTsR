#' Computes average of image list 
#'
#' Calculate the mean of a list of antsImages 
#' @param imageList list of antsImages 
#' @param normalize boolean determines if image is divided by mean before
#' averaging
#' @author Avants BB, Pustina D
#' @examples
#' r16 <- antsImageRead(getANTsRData('r16'))
#' r64 <- antsImageRead(getANTsRData('r64'))
#' mylist <- list(r16, r64)
#' antsAverageImages(mylist)
antsAverageImages <- function( imageList, normalize = FALSE )
  {
  avg <- imageList[[1]] * 0
  for ( i in imageList ) {
    if ( normalize ) {
      i <- i / mean( i )
    }
    avg <- avg + i
  }
  avg <- avg / length(imageList)
  return( avg )
  }
