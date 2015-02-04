#' ImageMath
#'
#' Perform math-operations on the given image
#'
#'
#' @param imageDimension2|3 Number of dimensions of the input image
#' @param outputImage result image
#' @param operator Must be one of the operators listed by ImageMath()
#' @param inputImage inputImage
#' @param otherParams see ImageMath() for details for each option
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mask<-getMask(fi)
#' ImageMath( 2 , fi , 'GD', fi , 1 )  # gray matter dilation
#' ImageMath( 2 , mask , 'Neg', mask )  # negate
#' ImageMath( 2 , mask , 'D', mask )  # distance transform
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
  print(class(args[[2]]))
  print("2nd param should (usually) be an antsImage")
  return(NA)
  }
  if ( class(args[[3]] ) != 'character')
  {
  print(class(args[[3]]))
  print("3rd param should be a character string defining the operation")
  return(NA)
  }
  if ( class(args[[4]])[[1]]  != 'antsImage' )
  {
  print(class(args[[4]]))
  print("4th param should be an antsImage")
  return(NA)
  }
  if (length(args) <= 1)
    args <- ""
  catchout<-.Call("ImageMath",
    .int_antsProcessArguments(args), PACKAGE = "ANTsR")
}
