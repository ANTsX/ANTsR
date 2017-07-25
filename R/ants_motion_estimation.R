#' @rdname motion_correction
#' @title Simple motion_correction function.
#'
#' @description motion corrects 4D time series imaging data
#'
#' @param img 4D antsImage
#' @param fixed target fixed image
#' @param moreaccurate 0, 1 or 2 with higher values being more accurte
#'  (use 2 for real applications, 0 for testing)
#' @param txtype Transformation type
#' @param verbose verbosity boolean
#' @return list of outputs
#' @author Avants BB
#' @examples
#'
#' testimg<-makeImage( c(10,10,10,5),  rnorm(  5000  ) )
#' testimg<-iMath(testimg,"PadImage",5)
#' mocorr<-.motion_correction( testimg )
#' @export
.motion_correction <- function( img, fixed = NA, moreaccurate = 1,
                                txtype = "Affine", verbose=FALSE )
{
  if (is.character(img)) {
    if (length(img) != 1) {
      print("'img' should be only one filename")
      return(NULL)
    }
    img <- antsImageRead(img, dimension = 4, "float")
    inpixeltype <- img@pixeltype
  } else if (class(img) == "antsImage") {
    inpixeltype <- img@pixeltype
    if (img@pixeltype != "float") {
      print("'img' must have pixeltype  'float' ")
      img <- antsImageClone(img, "float")
    }
    if (img@dimension != 4) {
      print("'img' must have pixeltype 'float' and dimension '4'")
      return(NULL)
    }
  } else {
    print("'img' must be a filename or an 'antsImage'")
    return(NULL)
  }

  if (is.na(fixed)) {
    fixed <- new("antsImage", "float", 3)
    antsMotionCorr(list(d = 3, a = img, o = fixed))
  } else {
    if (is.character(fixed)) {
      if (length(fixed) != 1) {
        print("'fixed' should be only one filename")
        return(NULL)
      }
      fixed <- antsImageRead(fixed, dimension = 3, "float")
    } else if (class(fixed) == "antsImage") {
      if (fixed@pixeltype != "float") {
        print("'fixed' must have pixeltype  'float' ")
        fixed <- antsImageClone(fixed, "float")
      }
      if (fixed@dimension != 3) {
        print("'fixed' must have pixeltype 'float' and dimension '3'")
        return(NULL)
      }
    } else {
      print("'fixed' must be a filename or an 'antsImage'")
      return(NULL)
    }
  }

  n <- dim(img)[4]
  if (n > 10) {
    n <- 10
  }
  avg_img <- new("antsImage", "float", 3)
  moco_img <- new("antsImage", "float", 4)
  moco_params <- new("antsMatrix", "double")
  mibins = 20
  if ( moreaccurate == 3 ) {
    antsMotionCorr(list(d = 3, o = list(moco_params, moco_img, avg_img),
                        m = list(name = "MI", fixed, img, 1, mibins ),
                        t = paste(txtype, "[0.1,3,0]", sep=""),
                        i = "100x50x20",
                        u = 1, e = 1, s = "2x1x0", f = "4x2x1",
                        n = n, l = 1, v = as.numeric(verbose) ) )
  }
  if ( moreaccurate == 2 ) {
    antsMotionCorr(list(d = 3, o = list(moco_params, moco_img, avg_img),
                        m = list(name = "MI", fixed, img, 1, mibins, "regular", 0.25 ),
                        t = paste(txtype, "[0.1]", sep=""),
                        i = "100x50x30",
                        u = 1, e = 1, s = "2x1x0", f = "4x2x1",
                        n = n, l = 1, v = as.numeric(verbose)))
  }
  if ( moreaccurate == "intraSubjectBOLD" ) {
    antsMotionCorr(list(d = 3, o = list(moco_params, moco_img, avg_img),
                        m = list(name = "MI", fixed, img, 1, mibins, "regular", 0.2 ),
                        t = paste(txtype, "[0.25]", sep=""),
                        i = "50x20",
                        u = 1, e = 1, s = "1x0", f = "2x1", n = n, l = 1, v = as.numeric(verbose)))
  }
  if ( moreaccurate == 1 ) {
    antsMotionCorr(list(d = 3, o = list(moco_params, moco_img, avg_img),
                        m = list(name = "MI", fixed, img, 1, mibins, "regular", 0.25 ),
                        t = paste(txtype, "[0.1]", sep=""),
                        i = "100",
                        u = 1, e = 1, s = 0, f = 1, n = n, l = 1, v = as.numeric(verbose)))
  }
  if ( moreaccurate == 0 ) {
    antsMotionCorr(list(d = 3, o = list(moco_params, moco_img, avg_img),
                        m = list(name = "MI", fixed, img, 1, mibins, "regular", 0.02),
                        t = paste(txtype, "[0.1]", sep=""),
                        i = 3, u = 1,
                        e = 1, s = 0, f = 1, n = n, l = 1, v = as.numeric(verbose)))
  }
  moco_params <- as.data.frame( moco_params )
  mynames <- c("MetricPre", "MetricPost",
               paste('MOCOparam', 1:(ncol(moco_params)-2), sep=''))
  names( moco_params ) <- mynames
  return
  (
    list
    (
      moco_img = moco_img,
      moco_params = moco_params,
      moco_avg_img = antsImageClone(avg_img, inpixeltype )
    )
  )
}
