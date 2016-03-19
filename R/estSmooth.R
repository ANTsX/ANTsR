#' Estimate image smoothness
#'
#' Estimates smoothness of a single image or image matrix
#'
#' @param object object may be an image of class "antsImage" or an image matrix
#' @param mask input mask, must match matrix
#' @param rdf residual degrees of freedom
#' @param scaleResid logical. if TRUE residuals are scaled
#' @param sample number of images to use for estimating smoothing (default uses all images)
#' @param verbose enables verbose output
#' 
#' @return Outputs the estimated FWHM and RPV image
#' 
#' @description
#' 
#' The partial derivatives of an image in x, y, and z directions are used to
#' create a covariance matrix which in turn is used to calculate the 
#' full-widths at half maxima (FWHM). The FWHM is equivalent to the estimated
#' image smoothness.
#' 
#' The resels per voxel image (\code{RPVImg}) represents the estimated resel at
#' each individual voxel throughout the search region. This may be used in 
#' place of volumetric measurements (or sum voxel measurements) when estimating
#' the p-value of a cluster using \code{rftPval}. The intent behind using the 
#' RPV image to estimate cluster level statistics is to offset the natural
#' probability of obtaining significant clusters solely by chance in very 
#' smooth regions at low thresholds.
#'
#' It is possible to use a single statistical field image to estimate the FWHM. 
#' However, it's recommended that FWHM estimates are obtained from the scaled 
#' residuals of statistical models (Stefan J.K et al., 1999). Therefore, this 
#' function is optimized to estimate the pooled smoothness of the residual 
#' images from a fitted model. By default residuals are scaled
#' (\code{scaleResid = TRUE}).
#' 
#' A scaling factor is used to correct for differences when using the 
#' \code{sample} option. Scaling isn't effective when the number of images is 
#' very low and typically results in an overestimation of the the FWHM. If only
#' one image or numeric vector is entered then the scaling factor is not used. 
#' If a numeric vector is entered the \code{imageMake} function is used to 
#' prepare it for smoothness estimation (see Worsley et al., 1999).
#' 
#' @references
#' Hayasaka (2004) Nonstationary cluster-size inference with random field and permutation methods.
#' 
#' Worsley K.J. (1992) A Three-Dimensional Statistical Analysis for CBF Activation Studies in Human Brain.
#' 
#' Worsley K.J. (1996) A Unified Statistical Approach for Determining Significant Signals in Images of Cerebral Activation.
#' 
#' Worsley K.J. (1999) Detecting Changes in Nonisotropic Images
#' 
#' Stefan J.K. (1999) Robust Smoothness Estimation in Statistical Parametric Maps Using Standardized Residual from the General Linear Model
#' 
#' @author Zachary P. Christensen
#' 
#' @seealso resels
#' 
#' @note function currently in beta phase
#' @examples
#' 
#' # estimatation of a single images smoothness
#' outimg1 <- makeImage(c(10, 10, 10), rnorm(1000))
#' maskimg <- getMask(outimg1)
#' myfwhm1 <- estSmooth(outimg1, maskimg)
#' 
#' # estimation of smoothness of overall sample images in a statistical model
#' outimg2 <- makeImage(c(10,10,10), rnorm(1000))
#' imat <- imageListToMatrix(list(outimg1, outimg2), maskimg)
#' variable <- rnorm(2)
#' fit <- lm(imat ~ variable)
#' myfwhm2 <- estSmooth(residuals(fit), maskimg)
#' 
#' @export estSmooth
estSmooth <- function(x, mask, rdf, scaleResid = TRUE, sample = NULL, verbose = TRUE) {
  if (missing(mask))
    stop("Must specify mask.")
  D <- mask@dimension
  # create iterators---------------------------------------------------------
  dimx <- 1:dim(mask)[1]
  dimx1 <- 2:(dim(mask)[1] + 1)
  if (D > 1) {
    dimy <- 1:dim(mask)[2]
    dimy1 <- 2:(dim(mask)[2] + 1)
  }
  if (D > 2) {
    dimz <- 1:dim(mask)[3]
    dimz1 <- 2:(dim(mask)[3] + 1)
  }
  
  if (class(x) == "antsImage") {
    scale <- 1
    n <- 1
    mrss <- 1
  } else if (class(x) == "numeric") {
    x <- matrix(x, nrow = 1)
    scale <- 1
    n <- 1
    mrss <- 1
  } else if (class(x) == "matrix") {
    if (missing(rdf))
      rdf <- nrow(x) - 1
    if (is.null(sample)) {
      nfull <- nrow(x) # original number of images (rows)
    } else {
      nfull <- nrow(x)
      rsamples <- sample(nrow(x), sample)
      x <- x[rsamples,]
    }
    if (scaleResid == "TRUE")
      mrss <- as.matrix(sqrt(colSums((x ^ 2) / rdf)), nrow = 1)
    else
      mrss <- 1
    n <- nrow(x) # number of images in sample (rows)
    scale <- (nfull / (rdf)) * (1 / n)
  }
  maskar <- as.array(mask)
  
  # set up for loop----------------------------------------------------------
  if (D == 1) {
    d1 <- m1 <- matrix(0, dim(mask)[1] + 1)
    maskar <- as.array(mask)
    m1[dimx1] <- maskar
    m3 <- ((m1[dimx1] * m1[dimx]))
    Vxx <- matrix(0, dim(mask)[1])
  } else if (D == 2) {
    d1 <- m1 <- matrix(0, dim(mask)[1] + 1, dim(mask)[2] + 1)
    maskar <- as.array(mask)
    m1[dimx1, dimy1, dimz1] <- maskar
    m3 <- ((m1[dimx1, dimy1] * m1[dimx, dimy1])) *
      ((m1[dimx1, dimy1] * m1[dimx1, dimy]))
    Vxx <- Vyy <- Vxy <- matrix(0, dim(mask)[1], dim(mask)[2])
  } else if (D == 3) {
    d1 <- m1 <- array(0, dim = dim(mask) + 1)
    maskar <- as.array(mask)
    m1[dimx1, dimy1, dimz1] <- maskar
    m3 <- ((m1[dimx1, dimy1, dimz1] * m1[dimx, dimy1, dimz1])) *
      ((m1[dimx1, dimy1, dimz1] * m1[dimx1, dimy, dimz1])) *
      ((m1[dimx1, dimy1, dimz1] * m1[dimx1, dimy1, dimz])) # mask to eliminate all cortical voxels
    Vxx <- Vyy <- Vzz <- Vxy <- Vxz <- Vyz <- array(0, dim = dim(mask))
  }
  
  # partial derivatives of each image----------------------------------------
  if (verbose)
    progress <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:n) {
    if (class(x) == "matrix")
      d1[m1 == 1] <- x[i,] / mrss
    if (D == 1) {
      dx <- (d1[dimx1] - d1[dimx]) * m3
    } else if (D == 2) {
      dx <- (d1[dimx1, dimy1] - d1[dimx, dimy1]) * m3
      dy <- (d1[dimx1, dimy1] - d1[dimx1, dimy]) * m3
    } else if (D == 3) {
      dx <- (d1[dimx1, dimy1, dimz1] - d1[dimx, dimy1, dimz1]) * m3
      dy <- (d1[dimx1, dimy1, dimz1] - d1[dimx1, dimy, dimz1]) * m3
      dz <- (d1[dimx1, dimy1, dimz1] - d1[dimx1, dimy1, dimz]) * m3
    }
    
    Vxx <- Vxx + (dx * dx)
    if (D > 1) {
      Vyy <- Vyy + (dy * dy)
      Vxy <- Vxy + (dx * dy)
    }
    if (D > 2) {
      Vzz <- Vzz + (dz * dz)
      Vxz <- Vxz + (dx * dz)
      Vyz <- Vyz + (dy * dz)
    }
    
    if (verbose)
      setTxtProgressBar(progress, i)
  }
  if (verbose)
    close(progress)
  # scale variances/covariances----------------------------------------------
  Vxx <- Vxx * scale
  if (D > 1) {
    Vyy <- Vyy * scale
    Vxy <- Vxy * scale
  }
  if (D > 2) {
    Vzz <- Vzz * scale
    Vxz <- Vxz * scale
    Vyz <- Vyz * scale
  }
  if (D == 1) {
    xyz <- Vxx * m3
  } else if (D == 2) {
    xyz <- cbind(matrix(Vxx * m3, ncol = 1), matrix(Vyy * m3, ncol = 1))
    resel.img <- (Vxx * Vyy ) + (Vxy * 2) # this needs to be checked
  } else if (D == 3) {
    xyz <- cbind(Vxx * m3, Vyy * m3, Vzz * m3)
    rpv <- (Vxx * Vyy * Vzz) +
      (Vxy * Vyz * Vxz * 2) -
      (Vyz * Vyz * Vxx) -
      (Vxy * Vxy * Vzz) -
      (Vxz * Vxz * Vyy)
  }
  # make RPV Image-----------------------------------------------------------
  rpv[rpv < 0] <- 0
  rpv <- sqrt(rpv / (4 * log(2)) ^ D)
  RPVImg <- as.antsImage(rpv * maskar)
  # estimate fwhm------------------------------------------------------------
  xyz <- sqrt((xyz) / (4 * log(2)))
  nvox <- sum(m3)
  rpv <- sum(rpv) / nvox
  xyz <- colSums(xyz) / nvox
  resels <- rpv ^ (1 / D) * (xyz / prod(xyz) ^ (1 / D))
  fwhm <- 1 / resels
  
  results <- list(fwhm = fwhm, RPVImg = RPVImg)
  results
}