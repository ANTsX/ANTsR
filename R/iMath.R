#' R access to the ANTs program iMath
#'
#' Perform math-operations on the given image.  The first input is the
#' input (usually an 'antsImage' object), the second is the operation.
#' Additional parameters should be specific for each function.  See the
#' the iMath help in ANTs.
#'
#' @param ... all parameters
#' @author Duda JT
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask(fi)
#' mask[1:256,100:150] = 0
#' mask = iMath( mask, "GetLargestComponent" )
#' plot(mask)
#'
#' @export iMath
iMath <- function(...) {
  args <- list(...)

  if ( length(args) < 2 )
    {
    stop("Not enough inputs provided")
    }

  if ( class(args[[2]] ) != 'character')
    {
    stop("2nd param should be a character string defining the operation")
    }

  catchout<-.Call("iMathInterface", args, PACKAGE = "ANTsR")
}
