#' iMath
#'
#' Perform various (often mathematical) operations on the input image/s.
#' Additional parameters should be specific for each operation.  See the
#' the full iMath in ANTs, on which this function is based.
#'
#' @param img input object, usually antsImage
#' @param operation a character string e.g. "GetLargestComponent" ... the
#' special case of "GetOperations" or "GetOperationsFull" will return
#' a list of operations and brief description.
#' Some operations may not be valid (WIP), but most are.
#' @param param ... additional parameters
#' @param ... further parameter options
#' @author JT Duda
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mask<-getMask( fi )
#' op1<-iMath( fi , "GD" , 1 )  # gray matter dilation by 1 voxel
#' op2<-iMath( mask , "D" )  # distance transform
#' op3<-iMath( 0 , "GetOperations" )  # list all ops
#'
#' if ( usePkg("magrittr") ) { # string ops together
#'   lapgd <- fi %>% iMath("Laplacian",1)  %>% iMath("GD",3)
#' }
#' # Canny is useful e.g.
#' # canny = ( iMath(myreg$warpedmovout,"Normalize")*255 ) %>% iMath("Canny",1,5,12)
#'
#' @export iMath
iMath <- function( img, operation, param=NA, ... ) {

  # input is usually an 'antsImage'
  if (is.na(img))
    {
    stop("No input provided")
    }
  if ( is.na(operation) || (!is.character(operation)) )
    {
    stop("operation must be a character string")
    }

  iMathOps = NULL
  data( "iMathOps", package="ANTsR", envir=environment() )

  if ( operation == "GetOperations" | operation == "GetOperationsFull")
    {

    if ( operation == "GetOperationsFull")
      {
      return( iMathOps )
      }
    else
      {
      return( iMathOps$Operation)
      }
    }
  else
    {

    # Temp fix for backward compat
    if ( operation == "TruncateImageIntensity")
      {
      print(paste(operation, "is moving to TruncateIntensity. Please update your code"))
      operation = "TruncateIntensity"
      }

    if ( ! ( operation  %in% iMathOps$Operation ) )
      {
      stop(paste("'operation'",operation," not recognized"))
      }

    args = list()
    if ( is.na(param) )
      {
      args = list(img, operation, ...)
      }
    else
      {
      args =  list(img, operation, param, ...)
      }
    retval = .Call("iMathInterface", args, PACKAGE="ANTsR")
    }

    return( retval )

}



#' iBind
#'
#' bind two images along their edge
#'
#' @param img1 input object, an antsImage
#' @param img2 second antsImage, same size as first
#' @param along dimension to bind along
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mi<-antsImageRead( getANTsRData("r62") , 2 )
#' bi<-iBind( fi, mi , 1 )
#' multismoo<- fi %>% iBind( smoothImage(fi,2) ) %>% iBind( smoothImage(fi,4) )
#'
#' @export iBind
iBind<-function( img1, img2, along=NA ) {
  if(!usePkg("abind")){
    print("Need package 'abind' to use function 'iBind.'")
    invisible(return())
  }
  if ( is.na(along) ) along=img1@dimension
  if ( along > img1@dimension | along < 1 ) along=img1@dimensions
  if ( dim(img1)[along] != dim(img1)[along] )
    stop("cant bind images along sides of different size")
  imgbind<-as.antsImage( abind::abind(as.array(img1), as.array(img2),
    along=along ) )
  antsCopyImageInfo(img1,imgbind)
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
