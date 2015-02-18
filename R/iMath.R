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
iMath <- function( img, operation , param, ... ) {
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
  # might remove
  if ( is.antsImage(img) & is.numeric(iMathOps$OutputDimensionalityChange[wh]) )
    {
    dim<-img@dimension
    outdim<-dim+as.numeric(  iMathOps$OutputDimensionalityChange[wh]  )
    outimg<-new("antsImage", img@pixeltype, outdim)
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
