#' joint label and intensity fusion
#'
#' A multiple atlas voting scheme to customize labels for a new subject. This
#' function will also perform intensity fusion. It almost directly calls the
#' \code{C++} in the ANTs executable so is much faster than other variants in ANTsR.
#' One may want to normalize image intensities for each input image before
#' passing to this function.  If no labels are passed, we do intensity fusion.
#' Note on computation time: the underlying \code{C++}
#' is multithreaded.  You can control the number of threads by setting the
#' environment variable \code{ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS} e.g. to use all or
#' some of your CPUs.  This will improve performance substantially. For instance,
#' on a macbook pro from 2015, 8 cores improves speed by about 4x.
#'
#' @param targetI antsImage to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages with intensity images
#' @param beta weight sharpness, default to 2
#' @param rad neighborhood radius, default to 2
#' @param labelList optional list containing antsImages with segmentation labels
#' @param rho ridge penalty increases robustness to outliers but also
#'   makes image converge to average
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 3
#' @param nonnegative constrain weights to be non-negative
#' @param maxLabelPlusOne boolean
#' this will add max label plus one to the non-zero parts of each label where the target mask
#' is greater than one.  NOTE: this will have a side effect of adding to the original label
#' images that are passed to the program.  It also guarantees that every position in the
#' labels have some label, rather than none.  Ie it guarantees to explicitly parcellate the
#' input data.
#' @param noZeroes boolean will zero out target mask regions that have any zero label.
#' this prevents JLF from computing a solution in regions not covered by the initial library.
#' @param verbose boolean
#' @return approximated image, segmentation and probabilities
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich, Nicholas J. Tustison
#' @keywords fusion, template
#' @importFrom grDevices colorRampPalette dev.off heat.colors hsv png rainbow rgb
#' @importFrom graphics hist par plot points
#' @importFrom magrittr %>%
#' @importFrom methods is new
#' @importFrom stats ar as.formula coefficients convolve
#' @importFrom stats cor cor.test cov dist formula glm lm
#' @importFrom stats lm.fit loess median model.matrix na.omit
#' @importFrom stats optimize p.adjust pchisq pf pnorm ppois
#' @importFrom stats predict pt qchisq qf qnorm qt quantile
#' @importFrom stats residuals rnorm sd spec.pgram spline stl
#' @importFrom stats t.test toeplitz ts var
#' @importFrom utils data glob2rx read.csv setTxtProgressBar tail txtProgressBar write.csv
#' @examples
#'
#' set.seed(123)
#' ref <- ri(1)
#' ref <- resampleImage(ref, c(50, 50), 1, 0)
#' ref <- iMath(ref, "Normalize")
#' mi <- ri(2)
#' mi2 <- ri(3)
#' mi3 <- ri(4)
#' mi4 <- ri(5)
#' mi5 <- ri(6)
#' refmask <- getMask(ref)
#' refmask <- iMath(refmask, "ME", 2) # just to speed things up
#' ilist <- list(mi, mi2, mi3, mi4, mi5)
#' seglist <- list()
#' for (i in 1:length(ilist))
#' {
#'   ilist[[i]] <- iMath(ilist[[i]], "Normalize")
#'   mytx <- antsRegistration(
#'     fixed = ref, moving = ilist[[i]],
#'     typeofTransform = c("Affine"), verbose = TRUE
#'   )
#'   mywarpedimage <- antsApplyTransforms(
#'     fixed = ref,
#'     moving = ilist[[i]],
#'     transformlist = mytx$fwdtransforms
#'   )
#'   ilist[[i]] <- mywarpedimage
#'   seg <- thresholdImage(ilist[[i]], "Otsu", 3)
#'   seglist[[i]] <- seg
#' }
#' r <- 2
#' pp <- jointLabelFusion(ref, refmask, ilist,
#'   rSearch = 2,
#'   labelList = seglist, rad = rep(r, length(dim(ref)))
#' )
#' pp2 <- jointLabelFusion(ref, refmask, ilist,
#'   rSearch = 2,
#'   labelList = seglist, rad = rep(r, length(dim(ref)))
#' )
#' testthat::expect_equal(pp2$segmentation, pp$segmentation)
#' pp <- jointLabelFusion(ref, refmask, ilist,
#'   rSearch = 2,
#'   rad = rep(r, length(dim(ref)))
#' )
#'
#' \dontrun{
#' ref <- antsImageRead(getANTsRData("ch2"))
#' n <- 50
#' ref <- resampleImage(ref, c(n, n, n), 1, 0)
#' ref <- iMath(ref, "Normalize")
#' refmask <- getMask(ref)
#' ilist <- list()
#' seglist <- list()
#' for (k in 1:5) {
#'   mi <- antsImageClone(ref) + rnorm(n^3, 0, 0.1)
#'   mykseg <- kmeansSegmentation(mi, 3, refmask)$segmentation
#'   ilist[[k]] <- mi
#'   seglist[[k]] <- mykseg
#' }
#' pp <- jointLabelFusion(ref, refmask, ilist,
#'   rSearch = 2,
#'   labelList = seglist, rad = rep(2, length(dim(ref))), verbose = TRUE
#' )
#' plot(ref, pp$segmentation)
#' plot(pp$intensity)
#' }
#'
#' @export jointLabelFusion
jointLabelFusion <- function(
    targetI,
    targetIMask,
    atlasList,
    beta = 4,
    rad = 2,
    labelList = NULL,
    rho = 0.01,
    usecor = FALSE,
    rSearch = 3,
    nonnegative = FALSE,
    maxLabelPlusOne = FALSE,
    noZeroes = FALSE,
    verbose = FALSE) {
  targetI <- check_ants(targetI)
  targetIMask <- check_ants(targetIMask)
  segpixtype <- "unsigned int"
  if (is.null(labelList)) doJif <- TRUE else doJif <- FALSE
  if (!doJif) {
    if (length(labelList) != length(atlasList)) {
      stop("length(labelList) != length(atlasList)")
    }
    if (noZeroes) {
      for (n in 1:length(labelList)) {
        targetIMask[labelList[[n]] == 0] <- 0
      }
    }
    inlabs <- sort(unique(labelList[[1]][targetIMask == 1]))
    labsum <- labelList[[1]]
    for (n in 2:length(labelList)) {
      inlabs <- sort(unique(c(inlabs, labelList[[n]][targetIMask == 1])))
      labsum <- labsum + labelList[[n]]
    }
    maxLab <- max(inlabs)
    if (maxLabelPlusOne) {
      for (n in 1:length(labelList)) {
        labelList[[n]][labelList[[n]] == 0] <- maxLab + 1
      }
    }
    mymask <- antsImageClone(targetIMask)
    mymask[labsum == 0] <- 0
  } else {
    mymask <- (targetIMask)
  }
  tdir <- tempdir()
  segdir <- tempdir()
  osegfn <- tempfile(pattern = "antsr", tmpdir = segdir, fileext = "myseg.nii.gz")
  if (file.exists(osegfn)) file.remove(osegfn)
  probs <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "prob%02d.nii.gz")
  probsbase <- basename(probs)
  searchpattern <- sub("%02d", "*", probsbase)
  mydim <- as.numeric(targetIMask@dimension)
  if (!doJif) {
    outimg <- new("antsImage", segpixtype, mydim)
    outimgi <- new("antsImage", "float", mydim)
    outs <- paste("[",
      antsrGetPointerName(outimg), ",",
      antsrGetPointerName(outimgi), ",", probs, "]",
      sep = ""
    )
  } else {
    outimgi <- new("antsImage", "float", mydim)
    outs <- antsrGetPointerName(outimgi)
  }
  # this is a temporary FIXME for some type issue i cant figure out right now
  #  outs <- paste("[",
  #    osegfn,",",
  #    antsrGetPointerName(outimgi), ",", probs, "]", sep = "")
  mymask <- antsImageClone(mymask, segpixtype)
  if (length(rad) == 1) myrad <- rep(rad, mydim) else myrad <- rad
  if (length(myrad) != mydim) {
    stop("patch radius dimensionality must equal image dimensionality")
  }
  myrad <- paste(myrad, collapse = "x")
  if (verbose == TRUE) vnum <- 1 else vnum <- 0
  if (nonnegative == TRUE) nnum <- 1 else nnum <- 0
  myargs <- list(
    d = mydim,
    t = targetI,
    a = rho, # or alpha in the paper
    b = beta,
    c = nnum, # constrain non-negative
    p = myrad, # patch radius
    m = "PC",
    s = rSearch,
    #    e =     # -e, --exclusion-image label[exclusionImage] # FIXME
    x = mymask,
    o = outs,
    v = vnum
  )
  # now add the intensity and label images
  kct <- length(myargs)
  for (k in 1:length(atlasList))
  {
    kct <- kct + 1
    myargs[[kct]] <- atlasList[[k]]
    names(myargs)[[kct]] <- "g"
    if (!doJif) {
      kct <- kct + 1
      castseg <- antsImageClone(labelList[[k]], segpixtype)
      myargs[[kct]] <- castseg
      names(myargs)[[kct]] <- "l"
    }
  }
  ANTsRCore::antsJointFusion(.int_antsProcessArguments(c(myargs)))
  if (doJif) {
    return(outimgi)
  }
  probsout <- list.files(
    path = tdir,
    pattern = glob2rx(searchpattern), full.names = TRUE,
    recursive = FALSE
  )

  segmentation_numbers <- rep(NA, length(probsout))
  for (i in 1:length(probsout)) {
    temp <- unlist(strsplit(probsout[i], "prob"))
    segnum <- tools::file_path_sans_ext(temp[length(temp)], TRUE)
    segmentation_numbers[i] <- as.integer(segnum)
  }

  probimgs <- imageFileNames2ImageList(probsout)

  if (!maxLabelPlusOne) {
    segmat <- imageListToMatrix(probimgs, mymask)
    finalsegvec <- apply(segmat, FUN = which.max, MARGIN = 2)
    finalsegvec2 <- finalsegvec * 0

    for (i in 1:length(probsout)) {
      finalsegvec2[finalsegvec == i] <- segmentation_numbers[i]
    }

    outimg <- makeImage(mymask, finalsegvec2) * mymask
    return(list(
      segmentation = outimg,
      intensity = outimgi,
      probabilityimages = probimgs,
      segmentationNumbers = segmentation_numbers,
      jlfmask = mymask
    ))
  } else {
    themaxlab <- which(segmentation_numbers == max(segmentation_numbers))
    backgroundProb <- probimgs[[themaxlab]]
    segmentation_numbers <- segmentation_numbers[-themaxlab]
    probsout <- probsout[-themaxlab]
    probimgs <- probimgs[-themaxlab]
    segmat <- imageListToMatrix(probimgs, mymask)
    fgndProb <- colSums(segmat)
    finalsegvec <- apply(segmat, FUN = which.max, MARGIN = 2)
    finalsegvec2 <- finalsegvec * 0
    for (i in 1:length(probsout)) {
      finalsegvec2[finalsegvec == i] <- segmentation_numbers[i]
    }
    outimg <- makeImage(mymask, finalsegvec2) * mymask

    # next decide what is "background" based on the sum of the first k labels vs the prob of the last one
    firstK <- probimgs[[1]] * 0
    for (i in 1:length(probimgs)) {
      firstK <- firstK + probimgs[[i]]
    }

    segmat <- imageListToMatrix(list(backgroundProb, firstK), mymask)
    bkgsegvec <- apply(segmat, FUN = which.max, MARGIN = 2) - 1
    bkgdseg <- makeImage(mymask, bkgsegvec) * mymask
    return(list(
      segmentation = outimg * bkgdseg,
      segmentation_raw = outimg,
      intensity = outimgi,
      probabilityimages = probimgs,
      segmentationNumbers = segmentation_numbers,
      backgroundProb = backgroundProb,
      jlfmask = mymask
    ))
  }
}





#' local joint label and intensity fusion
#'
#' A local version of joint label fusion that focuses on one or more specific labels.
#' This is primarily different from standard JLF because it performs registration
#' on a per label basis and focuses JLF on the label(s) alone.  It requires an
#' initial segmentation of the target region which can be provided either by
#' a manual or automated initialization.  Registration by SyN is a good choice
#' for the latter approach.
#'
#' @param targetI antsImage to be labeled
#' @param whichLabels label number(s) from the library on which to focus
#' @param targetMask a mask for the target image (optional), passed to joint fusion
#' @param initialLabel the initial approximate label(s) for the target region.
#' @param atlasList list containing antsImages with intensity images
#' @param labelList list containing antsImages with segmentation labels
#' @param submaskDilation amount to dilate initial mask to define region on which
#' we perform focused registration
#' @param typeofTransform passed to \code{antsRegistration}.
#' @param affMetric the metric for the affine part (GC, mattes, meansquares)
#' @param synMetric the metric for the syn part (CC, mattes, meansquares, demons)
#' @param synSampling the nbins or radius parameter for the syn metric
#' @param regIterations vector of iterations for syn.  we will set the smoothing
#' and multi-resolution parameters based on the length of this vector.
#' passed to \code{antsRegistration}.
#' @param affIterations vector of iterations for low-dimensional transforms.
#' @param localMaskTransform type of transform for local mask initialization;
#' would usually set to Rigid, Similarity or Affine
#' @param maxLabelPlusOne boolean
#' this will add max label plus one to the non-zero parts of each label where the target mask
#' is greater than one.  NOTE: this will have a side effect of adding to the original label
#' images that are passed to the program.  It also guarantees that every position in the
#' labels have some label, rather than none.  Ie it guarantees to explicitly parcellate the
#' input data.
#' @param noZeroes boolean will zero out target mask regions that have any zero label.
#' this prevents JLF from computing a solution in regions not covered by the initial library.
#' @param verbose boolean
#' @param ... extra parameters passed to JLF
#' @return label probabilities and segmentations
#' @author Brian B. Avants
#' @keywords fusion, template
#'
#' @export localJointLabelFusion
localJointLabelFusion <- function(
    targetI,
    whichLabels,
    targetMask,
    initialLabel,
    atlasList,
    labelList,
    submaskDilation = 10,
    typeofTransform = "SyN",
    affMetric = "meansquares",
    synMetric = "mattes",
    synSampling = 32,
    regIterations = c(40, 20, 0),
    affIterations,
    localMaskTransform,
    maxLabelPlusOne = FALSE,
    noZeroes = FALSE,
    verbose = FALSE,
    ...) {
  #  reg = antsRegistration( targetI, template, typeofTransform = typeofTransform )
  # isolate region
  myregion <- maskImage(initialLabel, initialLabel, level = whichLabels, binarize = FALSE)
  if (max(myregion) == 0) {
    myregion <- thresholdImage(initialLabel, 1, Inf)
  }
  if (max(myregion) == 0) stop(paste("Target Mask is empty in maskImage call in localJointLabelFusion"))
  myregionb <- thresholdImage(myregion, 1, Inf)
  myregionAroundRegion <- iMath(myregionb, "MD", submaskDilation)
  if (!missing(targetMask)) myregionAroundRegion <- myregionAroundRegion * targetMask
  croppedImage <- cropImage(targetI, myregionAroundRegion)
  croppedMask <- cropImage(myregionAroundRegion, myregionAroundRegion)
  croppedRegion <- cropImage(myregion, myregionAroundRegion)
  croppedmappedImages <- list()
  croppedmappedSegs <- list()
  if (missing(localMaskTransform)) localMaskTransform <- "Similarity"
  for (k in 1:length(atlasList)) {
    if (verbose) cat(paste0(k, "..."))
    initMap <- NA
    libregion <- maskImage(labelList[[k]], labelList[[k]], level = whichLabels, binarize = FALSE)
    if (max(libregion) == 0) stop(paste("Lib Mask is empty in maskImage call in localJointLabelFusion: case:", k))
    if (missing(affIterations)) {
      initMap <- tryCatch(
        {
          antsRegistration(
            smoothImage(croppedRegion, 1, sigmaInPhysicalCoordinates = FALSE),
            smoothImage(libregion, 1, sigmaInPhysicalCoordinates = FALSE),
            typeofTransform = localMaskTransform, affMetric = affMetric,
            samplingPercentage = 1.0,
            estimateLearningRateOnce = TRUE,
            verbose = verbose
          )$fwdtransforms
        },
        error = function(e) {
        }, finally = {
        }
      )
    } else {
      initMap <- tryCatch(
        {
          antsRegistration(
            smoothImage(croppedRegion, 1, sigmaInPhysicalCoordinates = FALSE),
            smoothImage(libregion, 1, sigmaInPhysicalCoordinates = FALSE),
            typeofTransform = localMaskTransform, affMetric = affMetric,
            samplingPercentage = 1.0,
            estimateLearningRateOnce = TRUE,
            affIterations = affIterations,
            verbose = verbose
          )$fwdtransforms
        },
        error = function(e) {
        }, finally = {
        }
      )
    }
    rr <- readAntsrTransform(initMap)
    if (!is.na(initMap) & all(!is.na(getAntsrTransformParameters(rr)))) {
      localReg <- antsRegistration(croppedImage, atlasList[[k]],
        regIterations = regIterations, synMetric = synMetric, synSampling = synSampling,
        typeofTransform = typeofTransform, initialTransform = initMap, verbose = verbose
      )
      transformedImage <- antsApplyTransforms(
        croppedImage, atlasList[[k]],
        localReg$fwdtransforms
      )
      transformedLabels <- antsApplyTransforms(croppedImage, labelList[[k]],
        localReg$fwdtransforms,
        interpolator = "nearestNeighbor"
      )
      croppedmappedImages[[k]] <- transformedImage
      croppedmappedSegs[[k]] <- transformedLabels
    }
  }
  croppedmappedImages <- croppedmappedImages[!sapply(croppedmappedImages, is.null)]
  croppedmappedSegs <- croppedmappedSegs[!sapply(croppedmappedSegs, is.null)]
  return(list(
    jlf = jointLabelFusion(
      croppedImage,
      croppedMask,
      atlasList = croppedmappedImages,
      labelList = croppedmappedSegs,
      maxLabelPlusOne = maxLabelPlusOne,
      verbose = verbose, ...
    ),
    croppedmappedImages = croppedmappedImages,
    croppedmappedSegs = croppedmappedSegs
  ))
}
