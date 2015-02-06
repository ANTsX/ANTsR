#' ImageMath
#'
#' Perform math-operations on the given image.  The first argument should
#' be dimensionality, the 2nd the output image, the third a string
#' describing the operation.  The fourth should be in the input image.
#' Additional parameters should be specific for each function.  See the
#' the ImageMath help in ANTs.
#'
#' @param ... all parameters
#' @author Shrinidhi KL
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask(fi)
#' ImageMath( 2 , fi , "GD", fi , 1 )  # gray matter dilation
#' ImageMath( 2 , mask , "Neg", mask )  # negate
#' ImageMath( 2 , mask , "D", mask )  # distance transform
#' plot(mask)
#'
#' @export ImageMath
ImageMath <- function(...) {
  args <- list(...)
  if ( class(args[[1]] ) != 'numeric' &
       class(args[[1]] ) != 'integer'  )
  {
  print(class(args[[1]]))
  print("first param should be image dimensionality")
  return(NA)
  }
  if ( class(args[[2]])[[1]] != 'antsImage' )
  {
#  print(class(args[[2]]))
#  print("2nd param should (usually) be an antsImage")
#  return(NA)
  }
  if ( class(args[[3]] ) != 'character')
  {
  print(class(args[[3]]))
  print("3rd param should be a character string defining the operation")
  return(NA)
  }
  if ( class(args[[4]])[[1]]  != 'antsImage'  )
  {
#  print(class(args[[4]]))
#  print("4th param should be an antsImage")
#  return(NA)
  }
  if (length(args) <= 1)
    args <- ""
  catchout<-.Call("ImageMath",
    .int_antsProcessArguments(args), PACKAGE = "ANTsR")
}
