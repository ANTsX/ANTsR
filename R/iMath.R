#' iMath
#'
#' Perform various (often mathematical) operations on the input image.
#' Additional parameters should be specific for each operation.  See the
#' the full ImageMath in ANTs, on which this function is based.
#'
#' @param img input object, usually antsImage
#' @param operation a character string e.g. "GetLargestComponent" ... the
#' special case of "GetOperations" or "GetOperationsFull" will return
#' a list of operations and brief description.
#' Some operations may not be valid (WIP), but most are.
#' @param param ... additional parameters
#' @param img2 ... additional image or image list
#' @param ... further parameter options
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mask<-getMask( fi )
#' op1<-iMath( fi , "GD" , 1 )  # gray matter dilation by 1 voxel
#' op2<-iMath( mask , "Neg" )  # negate
#' op3<-iMath( mask , "D" )  # distance transform
#' ops<-iMath( mask , "GetOperations" )  # list all ops
#'
#' @export iMath
iMath <- function( img, operation , param, img2 , ... ) {
#  call <- match.call() # see glm
  iMathOps <- NULL
  data( "iMathOps", package = "ANTsR", envir = environment() )
  if ( ! ( operation  %in% iMathOps$Operation ) )
    {
    stop(paste("'operation'",operation," not recognized"))
    }

  if ( class( operation ) != 'character')
  {
  print(class(operation))
  print("2nd param should be a character string defining the operation")
  return(NA)
  }
  if ( operation == "GetOperations" | operation == "GetOperationsFull" )
  {
  if ( operation == "GetOperationsFull" ) return(ops)
  return(trimops)
  }
  wh<-which( iMathOps$Operation == operation )
  if ( iMathOps$Operation[wh] == 'LabelStats' & is.antsImage(param)  )
    {
    dim<-img@dimension
    tf<-tempfile(fileext = ".csv")
    args<-list(dim,tf,operation,param,img)
    catchout<-.Call("ImageMath",
        .int_antsProcessArguments(args), PACKAGE = "ANTsR")
    df<-read.csv(tf)
    return(df)
    }
  if ( is.antsImage(img) & iMathOps$OutputType[wh] == 'antsImage-D'  )
    {
# not sure what's going on below - but shouldnt commit things that break
# the build or R CMD check or that make vignettes fail.
#    outimage = antsImageClone(img)
#    catchout<-.Call("ImageMath",
#        image, outimage, "MD", 2, PACKAGE = "ANTsR")
#
    dim<-img@dimension
    outimg<-antsImageClone(img)
    args<-list(dim,outimg,operation,img,param,...)
    catchout<-.Call("ImageMath",
      .int_antsProcessArguments(args), PACKAGE = "ANTsR")
    return(outimg)
    }
  else
    {
    print("1st param should be an antsImage")
    return(NA)
    }
}
