#' iBind
#'
#' bind two images along their edge
#'
#' @param img1 input object, an antsImage
#' @param img2 second antsImage, same size as first
#' @param along dimension to bind along
#' @author BB Avants
#' @examples
#' fi <- antsImageRead(getANTsRData("r16"), 2)
#' mi <- antsImageRead(getANTsRData("r62"), 2)
#' bi <- iBind(fi, mi, 1)
#' multismoo <- fi %>% iBind(smoothImage(fi, 2))
#' multismoo <- multismoo %>% iBind(smoothImage(fi, 4))
#'
#' @export iBind
iBind <- function(img1, img2, along = NA) {
  if (!usePkg("abind")) {
    print("Need package 'abind' to use function 'iBind.'")
    invisible(return())
  }
  img1 <- check_ants(img1)
  img2 <- check_ants(img2)

  if (is.na(along)) {
    along <- img1@dimension
  }
  if (along > img1@dimension | along < 1) {
    along <- img1@dimension
  }
  # if ( dim(img1)[along] != dim(img2)[along] )
  # stop("cant bind images along sides of different size")
  # let abind fail if it must
  imgbind <- as.antsImage(abind::abind(as.array(img1), as.array(img2),
    along = along
  ))
  antsCopyImageInfo(img1, imgbind)
}

#' Pipe an object forward
#'
#' The \code{\%>>\%} operator pipes the object on the left-hand side to the
#' right-hand side according to the syntax.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @description Chain operators together
#' @param lhs input from left side
#' @param rhs additional params
#' @export
#' @usage lhs \%>\% rhs
NULL
