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

  # Hack to not break vignette
  operation = args[[2]]

  if ( (operation != "GetLargestComponent") &&
       (operation != "Canny") &&
       (operation != "Normalize") )
    {
    img = args[[1]]
    operation = args[[2]]
    param = NA
    if ( length(args) > 2)
      {
      param = args[3:length(args)]
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
  else
    {
    catchout<-.Call("iMathInterface", args, PACKAGE = "ANTsR")
    }
}
