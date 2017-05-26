#' Morphological image operations
#'
#' Apply imaage morphological operations
#'
#'
#' @param input input image
#' @param operation operation to apply
#'  \itemize{
#'  \item{"close"}{ Morpholgical closing}
#'  \item{"dilate"}{ Morpholgical dilation}
#'  \item{"erode"}{ Morpholgical erosion}
#'  \item{"open"}{ Morpholgical opening}
#'  }
#' @param type type of morphology
#'  \itemize{
#'  \item{"binary"}{ Binary operation on a single value}
#'  \item{"grayscale"}{ Grayscale operations}
#'  }
#' @param radius radius of structuring element
#' @param value value to operation on (type='binary' only)
#' @param shape shape of the structuring element ( type='binary' only )
#'  \itemize{
#'  \item{"ball"}{ spherical structuring element}
#'  \item{"box"}{ box shaped structuring element}
#'  \item{"cross"}{ cross shaped structuring element}
#'  \item{"annulus"}{ annulus shaped structuring element}
#'  \item{"polygon"}{ polygon structuring element}
#'  }
#' @param radiusIsParametric used parametric radius boolean (shape='ball' and shape='annulus' only)
#' @param thickness thickness (shape='annulus' only)
#' @param lines number of lines in polygon (shape='polygon' only)
#' @param includeCenter include center of annulus boolean (shape='annulus' only)
#' @return antsImage is output
#' @author Avants BB
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mask<-getMask( fi )
#' dilatedBall = morphology( mask, operation="dilate", radius=3, 
#' type="binary", shape="ball")
#' erodedBox = morphology( mask, operation="erode", radius=3, 
#' type="binary", shape="box")
#' openedAnnulus = morphology( mask, operation="open", radius=5, 
#' type="binary", shape="annulus", thickness=2)
#'
#' @export morphology
morphology <- function(input, operation, radius, type="binary", value=1, shape="ball",
                       radiusIsParametric=FALSE, thickness=1, lines=3,
                       includeCenter=FALSE) {

  if ( input@components > 1 )
    {
    stop("Multichannel images not yet supported")
    }

  sFlag = switch( shape,
          ball = 1,
          box = 2,
          cross = 3,
          annulus = 4,
          polygon = 5,
          0)

  if ( sFlag == 0 )
    {
    stop("Invalid element shape")
    }

  if ( type == "binary" )
    {
    if (operation == "dilate" )
      {
      if ( sFlag==5 )
        {
        ret = iMath(input, "MD", radius, value, sFlag, lines)
        }
      else
        {
        ret = iMath(input, "MD", radius, value, sFlag, radiusIsParametric, thickness, includeCenter)
        }
      }
    else if ( operation == "erode" )
      {
      if ( sFlag==5 )
        {
        ret = iMath(input, "ME", radius, value, sFlag, lines)
        }
      else
        {
        ret = iMath(input, "ME", radius, value, sFlag, radiusIsParametric, thickness, includeCenter)
        }
      }
    else if (operation == "open" )
      {
      if ( sFlag==5)
        {
        ret = iMath(input, "MO", radius, value, sFlag, lines)
        }
      else
        {
        ret = iMath(input, "MO", radius, value, sFlag, radiusIsParametric, thickness, includeCenter)
        }
      }
    else if (operation == "close" )
      {
      if ( sFlag==5 )
        {
        ret = iMath(input, "MC", radius, value, sFlag, lines)
        }
      else
        {
        ret = iMath(input, "MC", radius, value, sFlag, radiusIsParametric, thickness, includeCenter)
        }
      }
    else
      {
      stop( "Invalid morphology operation")
      }
    }
  else if ( type == "grayscale" )
    {
    if (operation == "dilate" )
      {
      ret = iMath(input, "GD", radius)
      }
    else if ( operation == "erode" )
      {
      ret = iMath(input, "GE", radius)
      }
    else if (operation == "open" )
      {
      ret = iMath(input, "GO", radius)
      }
    else if (operation == "close" )
      {
      ret = iMath(input, "GC", radius)
      }
    else
      {
      stop( "Invalid morphology operation")
      }
    }
  else
    {
    stop("Invalid morphology type")
    }

  return(ret)

}
