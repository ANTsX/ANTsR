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
#' if ( usePkg("magrittr") ) { # string ops together
#'   lapgd <- fi %>% iMath("Laplacian",1)  %>% iMath("GD",3)
#' }
#'
#' @export iMath
iMath <- function( img, operation , param=NA, ... ) {
#  call <- match.call() # see glm
  iMathOps <- NULL
  data( "iMathOps", package = "ANTsR", envir = environment() )
  if ( operation == "GetOperations" | operation == "GetOperationsFull" )
  {
  if ( operation == "GetOperationsFull" ) return(iMathOps)
  return(iMathOps$Operation)
  }
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
  wh<-which( iMathOps$Operation == operation )
  if ( is.antsImage(img) & !is.na(iMathOps$OutputDimensionalityChange[wh]) )
    {
    dim<-img@dimension
    outdim<-dim+as.numeric(  iMathOps$OutputDimensionalityChange[wh]  )
    outimg<-new("antsImage", img@pixeltype, outdim)
    if ( is.na(param) )
      args<-list(dim,outimg,operation,img,...)
    if (!is.na(param) )
      args<-list(dim,outimg,operation,img,param,...)
    catchout<-.Call("ImageMath",
      .int_antsProcessArguments(args), PACKAGE = "ANTsR")
    return(outimg)
    }
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
if ( iMathOps$Operation[wh] == 'ReflectionMatrix'  )
  {
  dim<-img@dimension
  tf<-tempfile(fileext = ".mat")
  args<-list(dim,tf,operation,img,param)
  catchout<-.Call("ImageMath",
      .int_antsProcessArguments(args), PACKAGE = "ANTsR")
  return(tf)
  }
 stop("no matching call to iMath")
}



#' iBind
#'
#' bind two images along their edge
#'
#' @param img input object, an antsImage
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
  if ( is.na(along) ) along=img1@dimension
  if ( along > img1@dimension | along < 1 ) along=img1@dimensions
  if ( dim(img1)[along] != dim(img1)[along] )
    stop("cant bind images along sides of different size")
  imgbind<-as.antsImage( abind::abind(as.array(img1), as.array(img2),
    along=along ) )
  antsCopyImageInfo(img1,imgbind)
}
