#' An ants-based brain extraction script.
#'
#' Brain extraction based on mapping a template image and its mask to the input
#' image.  Should be preceded by abpN4.
#'
#' @param img whole head image to which we map a brain mask
#' @param tem Template image (the whole head) which has an associated label mask.
#' @param temmask Template's antsImage brain mask.
#' @param temregmask Template's registration mask including skull but not the face
#' @param regtype registration type: 'SyN' (fast, default), 'SyNabp' (better, slower)
#' @param tdir temporary directory (optional)
#' @param num_threads will execute
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  If \code{NULL}, will not set anything.
#' @param verbose print diagnostic messages
#' @param pad argument passed to \code{\link{iMath}} for how much to
#' zero-pad the image
#' @return outputs a brain image and brain mask.
#' @author Tustison N, Avants BB
#' @examples
#'
#' Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
#' set.seed(1)
#' n <- 64
#' fn <- getANTsRData("r16")
#' img <- antsImageRead(fn)
#' img <- resampleImage(img, c(n, n), 1, 0)
#' tf <- getANTsRData("r27")
#' tem <- antsImageRead(tf)
#' tem <- resampleImage(tem, c(n, n), 1, 0)
#' temmask <- antsImageClone(tem)
#' temmask[tem > 20] <- 1
#' temmask[tem <= 20] <- 0
#' bm <- ANTsR::abpBrainExtraction(img = img, tem = tem, temmask = temmask, num_threads = 1)
#' stopifnot(sum(bm$bmask) != prod(dim(bm$brain)))
#' bm2 <- ANTsR::abpBrainExtraction(img = img, tem = tem, temmask = temmask, num_threads = 1)
#' stopifnot(sum(bm2$bmask) != prod(dim(bm2$brain)))
#'
#' @export abpBrainExtraction
#' @importFrom magrittr %>%
#' @importFrom graphics hist par plot points
#' @importFrom grDevices colorRampPalette dev.off hsv png rainbow rgb
#' @importFrom methods new
#' @importFrom stats ar as.formula coefficients convolve
#'   cor cor.test cov dist formula glm lm
#'   lm.fit loess median model.matrix na.omit
#'   optimize p.adjust pchisq pf pnorm ppois
#'   predict pt qchisq qf qnorm qt quantile
#'   residuals rnorm sd spec.pgram spline stl
#'   t.test toeplitz ts var
#' @importFrom utils data glob2rx read.csv setTxtProgressBar tail
#'   txtProgressBar write.csv
abpBrainExtraction <- function(img, tem, temmask,
                               temregmask = NULL, regtype = "SyN", tdir = NA,
                               num_threads = 1,
                               pad = 0,
                               verbose = FALSE) {
  if (!is.null(num_threads)) {
    itk_threads <- Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    on.exit({
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    })
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)
  }

  if (missing(img) | missing(tem) | missing(temmask) |
    is.null(img) | is.null(tem) | is.null(temmask)) {
    cat("usage: abpBrainExtraction( img=imgToBExtract, tem = template, temmask = mask ) \n")
    cat(" if no priors are passed, or a numerical prior is passed, then use kmeans \n")
    return(NULL)
  }
  tempriors <- 3
  npriors <- 3

  img <- ANTsRCore::check_ants(img)
  if (pad > 0) {
    if (verbose) {
      message("Padding image")
    }
    img <- iMath(img, "PadImage", pad)
  }
  # file I/O - all stored in temp dir
  if (is.na(tdir)) {
    tdir <- tempdir()
    initafffn <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "_InitialAff.mat")
    EXTRACTION_WARP_OUTPUT_PREFIX <- tempfile(
      pattern = "antsr", tmpdir = tdir,
      fileext = "_PriorMap"
    )
  } else {
    initafffn <- paste(tdir, "antsr", "_InitialAff.mat", sep = "")
    EXTRACTION_WARP_OUTPUT_PREFIX <- paste(tdir, "antsr", "_PriorMap", sep = "")
  }
  #   # ANTs parameters begin
  #   ANTS_MAX_ITERATIONS <- "100x100x70x20"
  #   ANTS_TRANSFORMATION <- "SyN[0.1,3,0]"
  #   ANTS_LINEAR_METRIC_PARAMS <- "1,32,Regular,0.25"
  #   ANTS_LINEAR_CONVERGENCE <- "[1000x1000x1000x10,1e-7,15]"
  #   ANTS_LINEAR_CONVERGENCEFAST <- "[10x0x0x0,1e-7,10]"
  #   ANTS_METRIC <- "CC"
  #   ANTS_METRIC_PARAMS <- "1,4"
  #   # ANTs parameters end

  # atropos params
  locmrf <- paste(rep(1, img@dimension), collapse = "x")
  ATROPOS_BRAIN_EXTRACTION_INITIALIZATION <- "kmeans[3]"
  ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD <- "Gaussian"
  ATROPOS_BRAIN_EXTRACTION_CONVERGENCE <- "[3,0.0001]"
  ATROPOS_BRAIN_EXTRACTION_MRF <- paste("[0.2,", locmrf, "]")
  ATROPOS_SEGMENTATION_INITIALIZATION <- "PriorProbabilityImages"
  ATROPOS_SEGMENTATION_PRIOR_WEIGHT <- 0
  ATROPOS_SEGMENTATION_LIKELIHOOD <- "Gaussian"
  ATROPOS_SEGMENTATION_CONVERGENCE <- "[12,0.0001]"
  ATROPOS_SEGMENTATION_POSTERIOR_FORMULATION <- "Socrates"
  ATROPOS_SEGMENTATION_MRF <- paste("[0.11,", locmrf, "]")
  # atropos params end

  imgsmall <- resampleImage(img, rep(4, img@dimension))
  temsmall <- resampleImage(tem, rep(4, img@dimension))
  # careful initialization of affine mapping , result stored in initafffn
  if (!file.exists(initafffn)) {
    if (verbose) {
      message("Getting initial Affine Transformation")
    }
    if (is.null(temregmask)) {
      temp <- affineInitializer(
        fixedImage = temsmall, movingImage = imgsmall,
        searchFactor = 15, radianFraction = 0.1, usePrincipalAxis = 0,
        localSearchIterations = 10, txfn = initafffn,
        num_threads = num_threads
      )
    } else {
      temregmask <- check_ants(temregmask)
      temp <- affineInitializer(
        fixedImage = temsmall, movingImage = imgsmall,
        searchFactor = 15, radianFraction = 0.1, usePrincipalAxis = 0,
        localSearchIterations = 10, txfn = initafffn, mask = temregmask,
        num_threads = num_threads
      )
    }
  }


  # get laplacian images
  # FIXME the below antsregparams is the only part that uses these
  # so remove the comments if need lapi and lapt, otherwise just
  # inefficient
  # if (verbose) {
  #   message("Getting Laplacian of image")
  # }
  # lapi = iMath(img, "Laplacian", 1.5, 1)
  # if (verbose) {
  #   message("Getting Laplacian of template")
  # }
  # lapt = iMath(tem, "Laplacian", 1.5, 1)

  # FIXME should add mask to below via -x option
  # dtem <- antsImageClone(tem, "double")
  # dimg <- antsImageClone(img, "double")

  #   antsregparams <- list(d = img@dimension, u = 1,
  #                         o = EXTRACTION_WARP_OUTPUT_PREFIX,
  #                         r = initafffn, z = 1, w = "[0.025,0.975]",
  #                         m = paste("mattes[", antsrGetPointerName(antsImageClone(lapt,
  #                                                                                 "double")), ",", antsrGetPointerName(antsImageClone(lapi, "double")),
  #                                   ",", "0.5,32]", sep = ""),
  #                         c = "[50x50x50x10,1e-9,15]", t = "SyN[0.1,3,0]",
  #                         f = "6x4x2x1", s = "4x2x1x0")
  outprefix <- EXTRACTION_WARP_OUTPUT_PREFIX
  if (verbose) {
    message("Running Registration")
  }
  mytx <- antsRegistration(tem, img,
    typeofTransform = regtype,
    initialTransform = initafffn, mask = temregmask,
    verbose = verbose > 1
  )
  fwdtransforms <- mytx$fwdtransforms
  invtransforms <- mytx$invtransforms
  rm(mytx)
  if (verbose) {
    message("Applying Transformations")
  }
  temmaskwarped <- antsApplyTransforms(img, temmask,
    transformlist = invtransforms,
    interpolator = "nearestNeighbor",
    verbose = verbose
  )
  temmaskwarped <- thresholdImage(temmaskwarped, 0.5, 1)
  tmp <- antsImageClone(temmaskwarped)

  if (verbose) {
    message("Dilating and filling holes")
  }
  tmp <- iMath(temmaskwarped, "MD", 2)
  tmp <- iMath(tmp, "GetLargestComponent", 2)
  tmp <- iMath(tmp, "FillHoles") %>% thresholdImage(1, 2)
  gc()
  seg <- antsImageClone(img, "unsigned int")
  tmpi <- antsImageClone(tmp, "unsigned int")
  if (verbose) {
    message("Running Atropos")
  }
  atroparams <- list(
    d = img@dimension, a = img,
    m = ATROPOS_BRAIN_EXTRACTION_MRF,
    o = seg,
    x = tmpi,
    i = ATROPOS_BRAIN_EXTRACTION_INITIALIZATION,
    c = ATROPOS_BRAIN_EXTRACTION_CONVERGENCE,
    k = ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD,
    v = as.integer(verbose > 1)
  )
  atropos(atroparams)

  if (verbose) {
    message("Post-processing of atropos masks")
  }
  fseg <- antsImageClone(seg, "float")
  segwm <- thresholdImage(fseg, 3, 3)
  seggm <- thresholdImage(fseg, 2, 2)
  segcsf <- thresholdImage(fseg, 1, 1)
  segwm <- iMath(segwm, "GetLargestComponent")
  seggm <- iMath(seggm, "GetLargestComponent")
  seggm <- iMath(seggm, "FillHoles") %>% thresholdImage(1, 2)
  segwm[segwm > 0.5] <- 3
  tmp <- iMath(segcsf, "ME", 10)
  seggm[seggm < 0.5 & tmp > 0.5] <- 2
  seggm[seggm > 0.5] <- 2
  finalseg <- antsImageClone(img)
  finalseg[finalseg > 0] <- 0
  finalseg[seggm > 0.5] <- 2
  finalseg[segwm > 0.5 & seggm < 0.5] <- 3
  finalseg[segcsf > 0.5 & seggm < 0.5 & segwm < 0.5] <- 1
  rm(atroparams)

  # BA - finalseg looks good! could stop here
  tmp <- thresholdImage(finalseg, 2, 3)
  rm(finalseg)

  if (verbose) {
    message("Post-processing final segmentation")
  }
  tmp <- iMath(tmp, "ME", 2)
  tmp <- iMath(tmp, "GetLargestComponent", 2)
  tmp <- iMath(tmp, "MD", 4)
  tmp <- iMath(tmp, "FillHoles") %>% thresholdImage(1, 2)
  tmp[tmp > 0 | temmaskwarped > 0.25] <- 1
  tmp <- iMath(tmp, "MD", 5)
  tmp <- iMath(tmp, "ME", 5)
  finalseg2 <- iMath(tmp, "FillHoles") %>% thresholdImage(1, 2)
  rm(tmp)

  if (verbose) {
    message("Calculating Maurer Distance")
  }
  # FIXME - steps above should all be checked again ...
  dseg <- iMath(finalseg2, "ME", 5)
  dseg <- iMath(dseg, "MaurerDistance")
  droundmax <- 20
  dsearchvals <- c(1:100) / 100 * droundmax - 0.5 * droundmax
  mindval <- min(dseg)
  loval <- mindval
  distmeans <- rep(0, length(dsearchvals))
  ct <- 1
  for (dval in (dsearchvals)) {
    loval <- (dval - 1.5)
    dsegt <- antsImageClone(dseg)
    dsegt[dsegt >= loval & dsegt < dval] <- 1
    distmeans[ct] <- mean(img[dsegt == 1])
    ct <- ct + 1
  }
  localmin <- which.min(distmeans)
  dthresh <- dsearchvals[localmin]
  bmask <- antsImageClone(finalseg2)
  bmask <- thresholdImage(dseg, mindval, dthresh)
  brain <- antsImageClone(img)
  brain[finalseg2 < 0.5] <- 0
  if (pad > 0) {
    if (verbose) {
      message("De-Padding image")
    }
    brain <- iMath(brain, "PadImage", -pad)
    finalseg2 <- iMath(finalseg2, "PadImage", -pad)
    seg <- iMath(seg, "PadImage", -pad)
  }
  return(list(
    brain = brain, bmask = finalseg2,
    kmeansseg = seg, fwdtransforms = fwdtransforms,
    invtransforms = invtransforms,
    temmaskwarped = temmaskwarped, distmeans = distmeans,
    dsearchvals = dsearchvals,
    pad = pad
  ))
}
