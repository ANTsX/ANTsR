#' Extension of apply to allow input of antsImage
#'
#' Extension of apply to allow input of antsImage
#'
#' @param X an antsImage
#' @param MARGIN a vector giving the subscripts which the function will be applied over.
#' @param FUN the function to be applied..
#' @param ...  optional arguments to FUN
#' @return output is antsImage of lower dimnesion than input
#' @author Duda JT
#' @examples
#'
#' img <- makeImage(c(4,4,4), rnorm(4*4*4))
#' img2 <- apply.antsImage(img, c(1,2), mean)
#'
#' @method apply antsImage
#' @export

apply.antsImage <- function( X, MARGIN, FUN, ... )
{

  ar = as.array(X)
  ar = apply( ar, MARGIN, FUN, ...)

  if ( length(MARGIN) > 1 )
    {
    ar = as.antsImage(ar)
    antsSetSpacing( ar, antsGetSpacing(X)[MARGIN])
    antsSetOrigin( ar, antsGetOrigin(X)[MARGIN])
    dir = antsGetDirection(X)[MARGIN,MARGIN]
#   BA: the code below is inconsistent with other ants approaches
#   it also has the rather nasty effect of flipping images anatomically
#   in some cases.
#    for ( i in c(1:length(MARGIN)) )
#      {
#      if ( sum(dir[i,]) == 0 )
#        {
#        dir[i,i] = 1
#        }
#      }
    antsSetDirection( ar, dir )
    }

  return(ar)

}
