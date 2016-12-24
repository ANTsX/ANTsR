#' resampleImageToTarget
#'
#' Resample image by using another image as target reference.
#' This function uses antsApplyTransform with an identity 
#' matrix to achieve proper resampling.
#'
#' @param image image to resample
#' @param target image of reference, the output will be in this space
#' @param interpType Choice of interpolator. Supports partial matching.
#' \itemize{
#' \item{linear}{}
#' \item{nearestNeighbor}{}
#' \item{multiLabel}{ for label images but genericlabel is preferred}
#' \item{gaussian}{}
#' \item{bSpline}{}
#' \item{cosineWindowedSinc}{}
#' \item{welchWindowedSinc}{}
#' \item{hammingWindowedSinc}{}
#' \item{lanczosWindowedSinc}{}
#' \item{genericLabel}{ use this for label images}
#' }
#' @param imagetype choose 0/1/2/3 mapping to scalar/vector/tensor/time-series
#' @param verbose print command and run verbose application of transform.
#' @return output antsImage resampled with target's resolution/origin/orientation/direction
#' @author Pustina D
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16"))
#' fi2mm <- resampleImage(fi, c(2, 2), useVoxels = 0, interpType = 'linear')
#' resampled <- resampleImageToTarget(fi2mm, fi)
#'
#' @export resampleImageToTarget
#' 

resampleImageToTarget <- function (image, target, interpType = 'linear',
                                   imagetype = 0, verbose = FALSE, ...) {
  
  if (missing(image) | missing(target)) {
    print("missig inputs")
    return(NA)
  }
  fixed = target
  moving = image
  compose = NA
  transformlist = 'identity'
  interpolator = interpType
  
  ## compatibility with previous version's integer interpolator
  interpolator.oldoptions = c('linear','nearestNeighbor',
                              'gaussian','cosineWindowedSinc','bSpline')
  if ( is.numeric(interpolator) ) {
    interpolator = interpolator.oldoptions[ interpolator+1 ]
  }
  ### end compatibility
  
  interpolator[1] = paste(tolower(substring(interpolator[1], 
                                            1, 1)), substring(interpolator[1], 2), sep = "", collapse = " ")
  interpOpts = c("linear", "nearestNeighbor", "multiLabel", 
                 "gaussian", "bSpline", "cosineWindowedSinc", "welchWindowedSinc", 
                 "hammingWindowedSinc", "lanczosWindowedSinc", "genericLabel")
  interpolator <- match.arg(interpolator, interpOpts)
  args <- list(fixed, moving, transformlist, interpolator, 
               ...)
  if (!is.character(fixed)) {
    if (fixed@class[[1]] == "antsImage" & moving@class[[1]] == 
          "antsImage") {
      inpixeltype <- fixed@pixeltype
      warpedmovout <- antsImageClone(moving)
      f <- fixed
      m <- moving
      if ((moving@dimension == 4) & (fixed@dimension == 3) & (imagetype == 0))  stop("Set imagetype 3 to transform time series images.")
      wmo <- warpedmovout
      mytx <- list("-t", 'identity')
      
      if (is.na(compose)) 
        args <- list(d = fixed@dimension, i = m, o = wmo, 
                     r = f, n = interpolator, unlist(mytx))
      tfn <- paste(compose, "comptx.nii.gz", sep = "")
      if (!is.na(compose)) {
        mycompo = paste("[", tfn, ",1]", sep = "")
        args <- list(d = fixed@dimension, i = m, o = mycompo, 
                     r = f, n = interpolator, unlist(mytx))
      }
      myargs <- .int_antsProcessArguments(c(args))
      for (jj in c(1:length(myargs))) {
        if (!is.na(myargs[jj])) {
          if (myargs[jj] == "-") {
            myargs2 <- rep(NA, (length(myargs) - 1))
            myargs2[1:(jj - 1)] <- myargs[1:(jj - 1)]
            myargs2[jj:(length(myargs) - 1)] <- myargs[(jj + 
                                                          1):(length(myargs))]
            myargs <- myargs2
          }
        }
      }
      myverb = as.numeric(verbose)
      if (verbose) 
        print(myargs)
      .Call("antsApplyTransforms", c(myargs, "-z", 1, "-v", 
                                     myverb, "--float", 1, "-e", imagetype), PACKAGE = "ANTsR")
      if (is.na(compose)) 
        return(antsImageClone(warpedmovout, inpixeltype))
      if (!is.na(compose)) 
        if (file.exists(tfn)) 
          return(tfn)
      else return(NA)
    }
    return(1)
  }
  .Call("antsApplyTransforms", 
        .int_antsProcessArguments(
          c(args, "-z", 1, "--float", 1, "-e", imagetype)), PACKAGE = "ANTsR")
}
