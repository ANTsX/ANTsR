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
#' ImageMath( 2 , fi , "GD", fi , 1 )  # gray matter dilation
#' ImageMath( 2 , mask , "Neg", mask )  # negate
#' ImageMath( 2 , mask , "D", mask )  # distance transform
#' plot(mask)
#'
#' @export ImageMath
ImageMath <- function(...) {
  args<-list(...)
  if ( length(args) <= 1 ) args<-""
  .Call("ImageMath", int_antsProcessArguments(args), PACKAGE = "ANTsR")
}
