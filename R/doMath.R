#' %doMath%
#'
#' Perform various (often mathematical) operations on the input image, using
#' functional programming style.
#' Additional parameters should be specific for each operation.  See the
#' vignettes section for complete documentation of this funciton.
#'
#' @param img input object, usually antsImage
#' @param operation a character string e.g. "GetLargestComponent" ... the
#' special case of "GetOperations" or "GetOperationsFull" will return
#' a list of operations and brief description.
#' Some operations may not be valid (WIP), but most are.
#' @param param ... additional parameters
#' @param ... further parameter options
#' @author BB Avants, A Eshaghi
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mask<-getMask( fi )
#' op1<- fi %doMath% "GD 1"   # gray matter dilation by 1 voxel
#' op2<- mask %doMath% "Neg"  # negate
#' op3<- mask %doMath% "D"    # distance transform
#' op4<- mask %doMath% "D" %doMath% "Laplacian 1 1"   # consecutive math operations are easy
#' ops<- mask %doMath% "GetOperations"  # list all ops
#'
#' @export %doMath%
`%doMath%` <- function( img, operationAndArgs ) {
#  call <- match.call() # see glm
  param<- NA
  splittedOpArg <- strsplit(operationAndArgs, split = " ")
  operation <- splittedOpArg[[1]][1]
  #check if any parameter has been passed
  if (length(splittedOpArg[[1]]) > 1)
  {
  param <- splittedOpArg[[1]][2:length(splittedOpArg[[1]])]
  }
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
  if ( iMathOps$Operation[wh] == 'LabelStats' & is.antsImage(param)  )
    {
    dim<-img@dimension
    tf<-tempfile(fileext = ".csv")
    args<-list(dim,tf,operation,param[1],img)
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
    
    if ( class(param) != 'list' )
      args<-list(dim,outimg,operation,img)
    if (class(param) == 'list' )
      #args<-list(dim,outimg,operation,img,param,...)
      args<-list(dim,outimg,operation,img)
      param <- splittedOpArg[[1]][2:length(splittedOpArg[[1]])]
      for (n in 1:length(param)){
       args <- c(args, param[n])
        }
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






























































