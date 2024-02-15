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
#' tf<-getANTsRData("r27")
#' tem<-antsImageRead(tf)
#' mask = tem > 20
#' fh <- iMath( mask , "FillHoles" )  # list all ops
#' stopifnot(range(fh) == c(0, 1))
#' filled = fh > 0
#' @export iMath
iMath <- function(img, operation, param = NULL, ...) {
  img = check_ants(img)
  
  iMathOps <- NULL
  list_0 = c("Canny", "D", "FillHoles", "GC")
  list_1 = c("Laplacian",
             "MC",
             "MD",
             "ME",
             "MO",
             "MaurerDistance",
             "Normalize",
             "PadImage",
             "PeronaMalik",
             "PropagateLabelsThroughMask",
             "Sharpen",
             "TruncateIntensity")
  list_2 = c("GD",
             "GE",
             "GO",
             "GetLargestComponent",
             "Grad",
             "HistogramEqualization")
  stopifnot(length(intersect(list_0, list_1)) == 0)
  stopifnot(length(intersect(list_0, list_2)) == 0)
  stopifnot(length(intersect(list_2, list_1)) == 0)
  # input is usually an 'antsImage'
  if (missing(img))
  {
    stop("No input provided")
  }
  if (missing(operation) || (!is.character(operation)))
  {
    stop("operation must be a character string")
  }
  
  data("iMathOps", package = "ANTsRCore", envir = environment())
  
  if (operation == "GetOperations" |
      operation == "GetOperationsFull")
  {
    if (operation == "GetOperationsFull")
    {
      return(iMathOps)
    }
    else
    {
      return(iMathOps$Operation)
    }
  }
  else
  {
    # Temp fix for backward compat
    if (operation == "TruncateImageIntensity")
    {
      print(paste(
        operation,
        "is moving to TruncateIntensity. Please update your code"
      ))
      operation = "TruncateIntensity"
    }
    
    if (!(operation  %in% iMathOps$Operation))
    {
      stop(paste("'operation'", operation, " not recognized"))
    }
    
    run_binary = FALSE
    if (operation %in% "FillHolesBinary") {
      run_binary = TRUE
      operation = "FillHoles"
    }
    args = list()
    if (is.null(param))
    {
      args = list(img, operation, ...)
    }
    else
    {
      args =  list(img, operation, param, ...)
    }
    
    if (operation %in% list_0) {
      # print("yes in 0")
      retval = .Call("iMathInterface", args)
    } else if (operation %in% list_1) {
      # print("yes in 1")
      retval = .Call("iMathInterface1", args)
    } else if (operation %in% list_2) {
      # print("yes in 3")
      retval = .Call("iMathInterface2", args)
    } else {
      print("No match")
    }
    
    if (run_binary) {
      retval = retval > 0
    }
    
  }
  
  return(retval)
  
}
