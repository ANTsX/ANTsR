#' averages an image list with or without intensity normalization
#'
#' calculate the mean of a list of antsImages imageList: can contain anything that be cast via as.array()
#' @param imageList must be an antsImage list
#' @param normalize boolean determines if image is divided by mean before
#' averaging
#' @author Avants BB, Pustina D
antsAverageImages <- function( imageList, normalize = FALSE )
  {
  avg <- imageList[[1]] * 0
  for ( i in imageList ) {
    if ( normalize ) {
      i <- i / mean( i )
    }
    template <- template + i
  }
  avg <- avg / length(imageList)
  return( avg )
  }
