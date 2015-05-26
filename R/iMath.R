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
#' MaurerDistance;Filter;None;"iMath(i,op)";distance transform;0
#' PeronaMalik;Filter;"iterations, conductance";"iMath(i,op,10,0.5)";perona malik edge preserving smoothing;0
#' Grad;Filter;sigma;"iMath(i,op,1)";gradient magnitude;0
#' Laplacian;Filter;sigma;"iMath(i,op,1)";laplacian of intensity;0
#' Canny;Filter;sigma;"iMath(i,op,1,5,12)";canny edge detector;0
#' MD;Morphology;element radius;"iMath(i,op,1)";dilation;0
#' ME;Morphology;element radius;"iMath(i,op,1)";erosion;0
#' MO;Morphology;element radius;"iMath(i,op,1)";opening;0
#' MC;Morphology;element radius;"iMath(i,op,1)";closing;0
#' GD;Morphology;element radius;"iMath(i,op,1)";grayscale dilation;0
#' GE;Morphology;element radius;"iMath(i,op,1)";grayscale erosion;0
#' GO;Morphology;element radius;"iMath(i,op,1)";grayscale opening;0
#' GC;Morphology;element radius;"iMath(i,op,1)";grayscale closing;0
#' GetLargestComponent;LabelOp;None;"iMath(i,op)";returns largest portion of binary object;0
#' Normalize;Intensity;None;"iMath(i,op)";normalize intensity into 0 1 range;0
#' TruncateIntensity;Intensity;lower and upper quantile;"iMath(i,op,0.05,0.95)";trim intensities by quantiles;0
#' Sharpen;Intensity;None;"iMath(i,op)";makes edges sharper;0
#' Pad;Basic;positive or negative padvalue;"iMath(i,op,5)";pads or de-pads image by n voxels on all sides;0
#' DistanceMap;Filter;None;"iMath(i,op)";distance transform;0
#' FillHoles;LabelOp;None;"iMath(i,op)";fills holes in binary object;0
#' WIP - BlobDetector;Filter;nblobs;"iMath(i,op,100)";scale space blob detection-negate image to be positive blobs;0
#' WIP - LabelStats;LabelOp;roiImage;"iMath(i,op,roiImg)";summarizes ROI values;NA
#' WIP - ReflectionMatrix;Transformation;axis;"iMath(i,op,0)";returns a reflection matrix itk transform in filename form;NA

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
iMath <- function( input=NA, operation=NA, param=NA, ... ) {

  # input is usually an 'antsImage'
  if (is.na(input))
    {
    stop("No input provided")
    }
  if ( is.na(operation) || (!is.character(operation)) )
    {
    stop("operation must be a character string")
    }

  args = list()
  if ( is.na(param) )
    {
    args = list(input, operation, ...)
    }
  else
    {
    args =  list(input, operation, param, ...)
    }

  catchout = .Call("iMathInterface", args, PACKAGE="ANTsR")

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


#' reflectImage
#'
#' reflects an image along its axis
#'
#' @param img1 input object, an antsImage
#' @param axis which dimension to reflect across
#' @param tx transformation type to estimate after reflection
#' @author BB Avants
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' asym<-reflectImage( fi, 1, "Affine" )$warpedmovout
#' asym<-asym-fi
#'
#' @export reflectImage
reflectImage<-function( img1, axis=NA, tx=NA ) {
  if ( is.na(axis) ) axis=( img1@dimension - 1 )
  if ( axis > img1@dimension | axis < 0 ) axis=(img1@dimension-1)
  rflct<-iMath(img1,"ReflectionMatrix",axis-1)
  if ( ! is.na(tx) )
  {
  rfi<-invisible( antsRegistration(img1,img1,typeofTransform = tx,
    outprefix = tempfile(),
    initialTransform = rflct ) )
  return( rfi )
  }
  else
  {
  return( antsApplyTransforms( img1, img1, rflct  )  )
  }
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
