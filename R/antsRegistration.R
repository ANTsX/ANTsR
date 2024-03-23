#' Perform registration between two images.
#'
#' Register a pair of images either through the full or simplified interface
#' to the ANTs registration method.
#'
#' @param fixed fixed image to which we register the moving image.
#' @param moving moving image to be mapped to fixed space.
#' @param typeofTransform A linear or non-linear registration type.  Mutual
#' information metric by default. See \code{Details.}
#' @param initialTransform transforms to prepend
#' @param outprefix output will be named with this prefix.
#' @param mask Registration metric mask in the fixed image space.
#' @param movingMask Registration metric mask in the moving image space.
#' @param maskAllStages If true, apply metric mask(s) to all registration stages, instead of just the final stage.
#' @param gradStep gradient step size (not for all tx)
#' @param flowSigma smoothing for update field
#' @param totalSigma smoothing for total field
#' @param affMetric the metric for the affine part (GC, mattes, meansquares)
#' @param affSampling the sampling parameter for the affine metric
#' @param synMetric the metric for the syn part (CC, mattes, meansquares, demons)
#' @param synSampling the nbins or radius parameter for the syn metric
#' @param affIterations vector of iterations for low-dimensional registration.
#' we will set the smoothing and multi-resolution parameters based on the
#' length of this vector.
#' @param regIterations vector of iterations for syn.  we will set the smoothing
#' and multi-resolution parameters based on the length of this vector.
#' @param multivariateExtras list of additional images and metrics which will
#' trigger the use of multiple metrics in the registration process
#' in the deformable stage. Multivariate metrics needs 5 entries:
#' name of metric, fixed, moving, weight, samplingParam.
#' the list should be of the form
#' \code{ list( list( "nameOfMetric2", img, img, weight, metricParam ) ) }.
#' Another example would be \code{ list( list( "MeanSquares", f2, m2, 0.5, 0 ),
#' list( "CC", f2, m2, 0.5, 2 ) ) }.  This is only compatible with the
#' \code{SyNOnly}, \code{ElasticOnly}, or \code{antsRegistrationSyN*} transformations.
#' @param restrictTransformation This option allows the user to restrict the
#' optimization of the displacement field, translation, rigid or affine
#' transform on a per-component basis. For example, if one wants to limit
#' the deformation or rotation of 3-D volume to the first two dimensions,
#' this is possible by specifying a weight vector of \code{c(1,1,0)} for a
#' 3D deformation field or \code{c(1,1,0,1,1,0)} for a rigid transformation.
#' Restriction currently only works if there are no preceding transformations.
#' @param writeCompositeTransform if \code{TRUE}, will write transformations to h5 format.  Defaults to FALSE.
#' @param randomSeed integer random seed. combine with setting
#' ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS environment variable to limit the
#' impact of numerical differences.
#' @param samplingPercentage value between zero and one that allows the percentage
#' of points sampled to be controlled in low-dimensional metric estimation.
#' @param verbose request verbose output (useful for debugging)
#' @param printArgs print raw command line (useful for debugging)
#' @param ... additional options see antsRegistration in ANTs
#' @details
#' typeofTransform can be one of:
#' \itemize{
#'   \item{"Translation": }{Translation transformation.}
#'   \item{"Rigid": }{Rigid transformation: Only rotation and translation.}
#'   \item{"Similarity": }{Similarity transformation: scaling, rotation and
#'   translation.}
#'   \item{"QuickRigid": }{Rigid transformation: Only rotation and translation.
#'   May be useful for quick visualization fixes.'}
#'   \item{"DenseRigid": }{Rigid transformation: Only rotation and translation.
#'   Employs dense sampling during metric estimation.'}
#'   \item{"BOLDRigid": }{Rigid transformation: Parameters typical for BOLD
#'   to BOLD intrasubject registration'.'}
#'   \item{"Affine": }{Affine transformation: Rigid + scaling.}
#'   \item{"AffineFast": }{Fast version of \code{Affine}.}
#'   \item{"BOLDAffine": }{Affine transformation: Parameters typical for BOLD
#'   to BOLD intrasubject registration'.'}
#'   \item{"TRSAA": }{translation, rigid, similarity, affine (twice). please set
#'     \code{regIterations} if using this option.  this would be used in cases
#'     where you want a really high quality affine mapping (perhaps with mask).}
#'   \item{"ElasticSyN": }{Symmetric normalization: Affine + deformable transformation.
#'     Uses \code{synMetric} as optimization metric and elastic regularization.}
#'   \item{"SyN": }{Symmetric normalization: Affine + deformable transformation.
#'     Uses \code{synMetric} as optimization metric.}
#'   \item{"SyNRA": }{Symmetric normalization: Rigid + Affine + deformable transformation.
#'     Uses \code{synMetric} as optimization metric.}
#'   \item{"SyNOnly": }{Symmetric normalization: no initial transformation.
#'     Uses \code{synMetric} as optimization metric.  Assumes images are
#'     aligned by an inital transformation. Can be useful if you want to run
#'     an unmasked affine followed by masked deformable registration.}
#'   \item{"ElasticOnly": }{Elastic normalization: no initial transformation.}
#'   \item{"SyNCC": }{SyN, but with cross-correlation as the metric.
#'     Note, the default or chosen parameters will be replaced with
#'     \code{synMetric="CC", synSampling=4, synits="2100x1200x1200x20",
#'          smoothingsigmas="3x2x1x0", shrinkfactors="4x3x2x1"}. }
#'   \item{"SyNabp": }{SyN optimized for abpBrainExtraction, forces mutual information
#'     as optimization metric.}
#'   \item{"SyNBold": }{SyN, but optimized for registrations between
#'     BOLD and T1 images.}
#'   \item{"SyNBoldAff": }{SyN, but optimized for registrations between
#'     BOLD and T1 images, with additional affine step.}
#'   \item{"SyNAggro": }{SyN, but with more aggressive registration
#'     (fine-scale matching and more deformation).  Takes more time than \code{SyN}.}
#'   \item{"TV[n]": }{time-varying diffeomorphism with where 'n' indicates number of
#'             time points in velocity field discretization.  The initial transform
#'             should be computed, if needed, in a separate call to ants.registration.}
#'   \item{"TVMSQ": }{time-varying diffeomorphism with mean square metric}
#'   \item{"TVMSQC": }{time-varying diffeomorphism with mean square metric
#'   for very large deformation}
#'   \item{"Elastic": }{simple elastic deformation.  one might want to run an
#'   affine transformation before this.  may not produce diffeomorphic transformations.
#'   user may need to explore gradient and sigma parameters.  this will not produce a valid inverse deformation.  \code{totalSigma} should be greater than zero.}
#'   \item{"antsRegistrationSyN[x]":}{recreation of the antsRegistrationSyN.sh script in ANTs
#'                                    where 'x' is one of the transforms available (e.g., 't', 'b', 's')}
#'   \item{"antsRegistrationSyNQuick[x]":}{recreation of the antsRegistrationSyNQuick.sh script in ANTs
#'                                    where 'x' is one of the transforms available (e.g., 't', 'b', 's')}
#'   \item{"antsRegistrationSyNRepro[x]":}{reproducible registration.  x options as above.}
#'   \item{"antsRegistrationSyNQuickRepro[x]":}{quick reproducible registration.  x options as above.}
#' }
#' @return outputs a list containing:
#' \itemize{
#'   \item{warpedmovout: }{Moving image warped to space of fixed image.}
#'   \item{warpedfixout: }{Fixed image warped to space of moving image.}
#'   \item{fwdtransforms: }{Transforms to move from moving to fixed image.}
#'   \item{invtransforms: }{Transforms to move from fixed to moving image.}
#' }
#' Ouptut of 1 indicates failure
#' @author Shrinidhi KL, Tustison NJ, Avants BB
#' @examples
#'
#' # print help
#' antsRegistration()
#' fi <- antsImageRead(getANTsRData("r16"))
#' mi <- antsImageRead(getANTsRData("r64"))
#' mytx2 <- antsRegistration(fixed = fi, typeofTransform = "")
#' rig <- antsRegistration(
#'   fixed = fi, moving = mi,
#'   typeofTransform = "Rigid", verbose = TRUE
#' )
#' trans <- readAntsrTransform(rig$fwdtransforms, 2)
#' postrig <- antsRegistration(
#'   fixed = fi, moving = mi,
#'   typeofTransform = "Affine", initialTransform = trans
#' )
#' for (itype in c("AffineFast", "BOLDAffine")) {
#'   print(itype)
#'   mytx2 <- antsRegistration(
#'     fixed = fi, moving = mi,
#'     typeofTransform = itype
#'   )
#' }
#' mytx2 <- antsRegistration(
#'   fixed = fi, moving = mi,
#'   typeofTransform = "SyNOnly",
#'   multivariateExtras = list(list("MeanSquares", fi, mi, 0.5, 0))
#' )
#' testthat::expect_error(
#'   antsRegistration(fixed = fi, moving = mi, typeofTransform = "sdf")
#' )
#' bad <- antsRegistration(fixed = fi, moving = mi, regIterations = 40)
#' affIterations <- c(3, 2, 1, 0)
#' mytx2 <- antsRegistration(
#'   fixed = fi, moving = mi,
#'   affIterations = affIterations
#' )
#' # set below for slower but numerically repeatable results
#' # these should be set in .Renviron not by sys calls
#' #  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
#' #  Sys.setenv(ANTS_RANDOM_SEED = 20180716)
#' fi <- antsImageRead(getANTsRData("r16"))
#' mi <- antsImageRead(getANTsRData("r64"))
#' fi <- resampleImage(fi, c(60, 60), 1, 0)
#' mi <- resampleImage(mi, c(50, 50), 1, 0) # speed up
#' mytx <- antsRegistration(fixed = fi, moving = mi, typeofTransform = c("SyN"))
#' mywarpedimage <- antsApplyTransforms(
#'   fixed = fi, moving = mi,
#'   transformlist = mytx$fwdtransforms
#' )
#' mytx2 <- antsRegistration(fixed = fi, moving = mi, typeofTransform = c("SyN"))
#' mywarpedimage2 <- antsApplyTransforms(
#'   fixed = fi, moving = mi,
#'   transformlist = mytx2$fwdtransforms
#' )
#' # testthat::expect_equal(as.array(mywarpedimage), as.array(mywarpedimage2))
#'
#' \dontrun{
#' # quick visualization fix for images with odd orientation
#' mni <- antsImageRead(getANTsRData("mni"))
#' strokt1 <- antsImageRead("strokt1.nii.gz")
#' strokt1reg <- antsRegistration(
#'   fixed = mni,
#'   moving = strokt1,
#'   typeofTransform = "QuickRigid", verbose = TRUE
#' )
#' plot(strokt1reg$warpedmovout, axis = 3, nslices = 20)
#' # now - how to use a mask
#' fi <- antsImageRead(getANTsRData("r16"))
#' fiseg <- kmeansSegmentation(fi, 3)
#' mi <- antsImageRead(getANTsRData("r64"))
#' msk <- thresholdImage(fiseg$segmentation, 0, 0)
#' mytx <- antsRegistration(
#'   fixed = fi, moving = mi, typeofTransform = c("SyNCC"),
#'   mask = msk, verbose = F
#' )
#' jac <- createJacobianDeterminantImage(fi, mytx$fwdtransforms[1])
#' }
#'
#' @export antsRegistration
antsRegistration <- function(
    fixed = NULL,
    moving = NULL,
    typeofTransform = "SyN",
    initialTransform = NA,
    outprefix = NULL,
    mask = NULL,
    movingMask = NULL,
    maskAllStages = FALSE,
    gradStep = 0.2,
    flowSigma = 3,
    totalSigma = 0,
    affMetric = "mattes",
    affSampling = 32,
    synMetric = "mattes",
    synSampling = 32,
    affIterations,
    regIterations = c(40, 20, 0),
    multivariateExtras,
    restrictTransformation,
    writeCompositeTransform = FALSE,
    randomSeed,
    samplingPercentage = 0.2,
    verbose = FALSE,
    printArgs = FALSE, ...) {
  numargs <- nargs()
  if (numargs == 1 & typeof(fixed) == "list") {
    ANTsRCore::antsRegistration(.int_antsProcessArguments(c(fixed)))
    return(0)
  }
  if (nchar(typeofTransform) == 0) {
    typeofTransform <- "SyN"
  }
  if (nchar(outprefix) == 0 || length(outprefix) == 0 ||
    is.null(outprefix)) {
    outprefix <- tempfile()
  }
  find_tx <- function(outprefix) {
    alltx <- Sys.glob(paste0(outprefix, "*", "[0-9]*"))
    alltx <- alltx[!grepl("VelocityField", alltx)]
    findinv <- grepl("[0-9]InverseWarp.nii.gz", alltx)
    findaff <- grepl("[0-9]GenericAffine.mat", alltx)
    findfwd <- grepl("[0-9]Warp.nii.gz", alltx)
    velocityfield <- Sys.glob(paste0(outprefix, "*VelocityField.nii.gz"))
    L <- list(
      alltx = alltx,
      findinv = findinv,
      findfwd = findfwd,
      findaff = findaff,
      velocityfield = velocityfield
    )
    return(L)
  }
  all_tx <- find_tx(outprefix)
  pre_transform <- all_tx$alltx[all_tx$findinv | all_tx$findfwd | all_tx$findaff]
  rm(list = "all_tx")

  if (numargs < 1 | missing(fixed) | missing(moving)) {
    cat("for simplified mode: \n")
    cat(paste0(
      " antsRegistration( fixed , moving , ",
      "typeofTransform = c(\"Rigid\",\"Affine\",\"AffineFast\",",
      "\"SyN\",\"SyNCC\"),  outputPrefix=\"./antsRegOut\" \n"
    ))
    cat("")
    cat("For full mode: use standard ants call , e.g. : \n")
    cat(paste0(
      " ANTsR::antsRegistration( list( d=2,m=\"mi[r16slice.nii.gz,",
      "r64slice.nii.gz,1,20,Regular,0.05]\", t=\"affine[1.0]\", ",
      "c=\"2100x1200x1200x0\",  s=\"3x2x1x0\", f=\"4x3x2x1\", ",
      "u=\"1\", o=\"[xtest,xtest.nii.gz,xtest_inv.nii.gz]\" ) )\n"
    ))
    cat("full help: \n")
    ANTsRCore::antsRegistration(.int_antsProcessArguments(c(list("--help"))))
    return(0)
  }

  args <- list(fixed, moving, typeofTransform, outprefix, ...)
  myfAff <- "6x4x2x1"
  mysAff <- "3x2x1x0"
  metsam <- 0.2
  myiterations <- "2100x1200x1200x10"
  if (!missing(affIterations)) {
    myiterations <- paste0(affIterations, collapse = "x")
    itlen <- length(affIterations) - 1
    if (itlen == 0) {
      mysAff <- 0
      myfAff <- 1
      myiterations <- affIterations
    } else {
      mysAff <- itlen:0
      myfAff <- 2^mysAff
      mysAff <- paste(mysAff, collapse = "x")
      myfAff <- paste(myfAff, collapse = "x")
    }
  }
  if (typeofTransform == "AffineFast") {
    typeofTransform <- "Affine"
    myiterations <- "2100x1200x0x0"
  }
  if (typeofTransform == "BOLDAffine") {
    typeofTransform <- "Affine"
    myfAff <- "2x1"
    mysAff <- "1x0"
    myiterations <- "100x20"
  }
  if (typeofTransform == "QuickRigid") {
    typeofTransform <- "Rigid"
    myiterations <- "20x20x0x0"
  }
  if (typeofTransform == "DenseRigid") {
    typeofTransform <- "Rigid"
    metsam <- 0.8
  }
  if (typeofTransform == "BOLDRigid") {
    typeofTransform <- "Rigid"
    myfAff <- "2x1"
    mysAff <- "1x0"
    myiterations <- "100x20"
  }
  mysyn <- paste("SyN[", gradStep, ",", flowSigma, ",", totalSigma, "]", sep = "")
  itlen <- length(regIterations) - 1
  if (itlen == 0) {
    smoothingsigmas <- 0
    shrinkfactors <- 1
    synits <- regIterations
  } else {
    smoothingsigmas <- itlen:0
    shrinkfactors <- 2^smoothingsigmas
    smoothingsigmas <- paste(smoothingsigmas, collapse = "x")
    shrinkfactors <- paste(shrinkfactors, collapse = "x")
    synits <- paste(regIterations, collapse = "x")
  }
  if (!is.null(fixed)) {
    fixed <- check_ants(fixed)
  }
  if (!is.null(moving)) {
    moving <- check_ants(moving)
  }
  if (fixed@dimension != moving@dimension) {
    stop("Fixed and moving image dimensions are not the same.")
  }
  if (!is.character(fixed)) {
    fixed <- check_ants(fixed)
    error_not_antsImage(fixed, "fixed")
    moving <- check_ants(moving)
    error_not_antsImage(moving, "moving")
    if (is.antsImage(fixed) & is.antsImage(moving)) {
      inpixeltype <- fixed@pixeltype
      tvTypes <- c(paste0("TV[", 1:8, "]"))
      ttexists <- FALSE
      # change this to a match.arg
      allowableTx <- c(
        "Translation", "Rigid", "Similarity", "Affine", "TRSAA",
        "SyN", "SyNRA", "SyNOnly", "SyNCC", "SyNabp", "SyNBold", "SyNBoldAff",
        "SyNAggro", "SyNLessAggro", tvTypes,
        "TVMSQ", "TVMSQC", "ElasticSyN", "Elastic", "ElasticOnly"
      )
      # Perform checking of antsRegistrationSyN transforms later
      ttexists <- typeofTransform %in% allowableTx || grepl("antsRegistrationSyN", typeofTransform)
      if (ttexists) {
        initx <- initialTransform
        if (class(initx) == "antsrTransform") {
          tempTXfilename <- tempfile(fileext = ".mat")
          initx <- invertAntsrTransform(initialTransform)
          initx <- invertAntsrTransform(initx)
          writeAntsrTransform(initx, tempTXfilename)
          initx <- tempTXfilename
        }
        moving <- antsImageClone(moving, "float")
        fixed <- antsImageClone(fixed, "float")
        warpedfixout <- antsImageClone(moving)
        warpedmovout <- antsImageClone(fixed)
        f <- antsrGetPointerName(fixed)
        m <- antsrGetPointerName(moving)
        wfo <- antsrGetPointerName(warpedfixout)
        wmo <- antsrGetPointerName(warpedmovout)

        fMaskStr <- "NA"
        mMaskStr <- "NA"

        if (is.list(mask) && !is.null(movingMask)) {
          stop("Ambiguous definition for the moving mask.")
        }

        if (is.list(mask)) {
          fixedMaskBinary <- antsImageClone(thresholdImage(mask[[1]], 0, 0, 0, 1), "unsigned char")
          fMaskStr <- antsrGetPointerName(fixedMaskBinary)

          movingMaskBinary <- antsImageClone(thresholdImage(mask[[2]], 0, 0, 0, 1), "unsigned char")
          mMaskStr <- antsrGetPointerName(movingMaskBinary)
        } else {
          if (!is.null(mask)) {
            fixedMaskBinary <- antsImageClone(thresholdImage(mask, 0, 0, 0, 1), "unsigned char")
            fMaskStr <- antsrGetPointerName(fixedMaskBinary)
          }
          if (!is.null(movingMask)) {
            movingMaskBinary <- antsImageClone(thresholdImage(movingMask, 0, 0, 0, 1), "unsigned char")
            mMaskStr <- antsrGetPointerName(movingMaskBinary)
          }
        }

        maskOption <- paste0("[", fMaskStr, ",", mMaskStr, "]")
        if (maskAllStages) {
          earlyMaskOption <- maskOption
        } else {
          earlyMaskOption <- "[NA,NA]"
        }

        if (is.na(initx)) {
          initx <- paste("[", f, ",", m, ",1]", sep = "")
        }
        if (typeofTransform == "SyNBold") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,",
              affSampling, ",regular,", samplingPercentage, "]",
              sep = ""
            ),
            "-t", "Rigid[0.25]", "-c", "[1200x1200x100,1e-6,5]", "-s", "2x1x0",
            "-f", "4x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn, "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "SyNBoldAff") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling,
              ",regular,", samplingPercentage, "]",
              sep = ""
            ),
            "-t", "Rigid[0.25]", "-c", "[1200x1200x100,1e-6,5]", "-s", "2x1x0",
            "-f", "4x2x1",
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling,
              ",regular,", samplingPercentage, "]",
              sep = ""
            ),
            "-t", "Affine[0.25]", "-c", "[200x20,1e-6,5]", "-s", "1x0",
            "-f", "2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling,
              "]",
              sep = ""
            ),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "ElasticSyN") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.25]", "-c", "2100x1200x200x0", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors,
            "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "SyN") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.25]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "SyNRA") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Rigid[0.25]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.25]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors, "-u",
            "0", "-z", "1", "-o", paste("[", outprefix, ",",
              wmo, ",", wfo, "]",
              sep = ""
            ),
            "-x", maskOption
          )
        }
        if (typeofTransform == "ElasticOnly") {
          mysyn <- paste("GaussianDisplacementField[", gradStep, ",", flowSigma, ",", totalSigma, "]", sep = "")
        }
        if (typeofTransform == "SyNOnly" | typeofTransform == "ElasticOnly") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas, "-f", shrinkfactors,
            "-u", "0", "-z", "1", "-o", paste("[", outprefix, ",",
              wmo, ",", wfo, "]",
              sep = ""
            )
          )
          if (!missing(multivariateExtras)) {
            args0 <- list(
              "-d", as.character(fixed@dimension), "-r", initx,
              "-m", paste(synMetric, "[", f, ",", m,
                ",1,", synSampling, "]",
                sep = ""
              )
            )
            args1 <- list()
            for (mm in 1:length(multivariateExtras)) {
              if (length(multivariateExtras[[mm]]) != 5) {
                stop(paste0(
                  "multivariate metric needs 5 entries: ",
                  "name of metric, fixed, moving, weight, ",
                  "samplingParam"
                ))
              }
              args1 <- lappend(args1, list(
                "-m", paste(
                  as.character(multivariateExtras[[mm]][[1]]), "[",
                  antsrGetPointerName(multivariateExtras[[mm]][[2]]), ",",
                  antsrGetPointerName(multivariateExtras[[mm]][[3]]), ",",
                  as.character(multivariateExtras[[mm]][[4]]), ",",
                  as.character(multivariateExtras[[mm]][[5]]), "]",
                  sep = ""
                )
              ))
            }
            args2 <- list(
              "-t", mysyn,
              "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
              "-s", smoothingsigmas, "-f", shrinkfactors,
              "-u", "0", "-z", "1", "-o", paste("[", outprefix, ",",
                wmo, ",", wfo, "]",
                sep = ""
              )
            )
            args <- lappend(args0, args1)
            args <- lappend(args, args2)
          }
          args <- lappend(args, list("-x", maskOption))
        }

        if (typeofTransform == "SyNAggro") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.25]", "-c", "2100x1200x1200x100", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "SyNCC") {
          synMetric <- "CC"
          synSampling <- 4
          synits <- "2100x1200x1200x20"
          smoothingsigmas <- "3x2x1x0"
          shrinkfactors <- "4x3x2x1"
          mysyn <- paste("SyN[0.15,3,0]", sep = "")
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Rigid[1]",
            "-c", "2100x1200x1200x0",
            "-s", "3x2x1x0",
            "-f", "4x4x2x1",
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[1]", "-c", "1200x1200x100", "-s", "2x1x0",
            "-f", "4x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }

        if (typeofTransform == "TRSAA") {
          itlen <- length(regIterations)
          itlenlow <- round(itlen / 2 + 0.0001)
          dlen <- itlen - itlenlow
          myconvlow <- paste(
            c(rep(2000, itlenlow), rep(0, dlen)),
            collapse = "x"
          )
          myconvhi <- paste(regIterations, collapse = "x")
          myconvhi <- paste("[", myconvhi, ",1.e-7,10]", sep = "")
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,0.3]", sep = ""),
            "-t", "Translation[1]",
            "-c", myconvlow,
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,0.3]", sep = ""),
            "-t", "Rigid[1]",
            "-c", myconvlow,
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,0.3]", sep = ""),
            "-t", "Similarity[1]",
            "-c", myconvlow,
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,0.3]", sep = ""),
            "-t", "Affine[1]",
            "-c", myconvhi,
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-x", earlyMaskOption,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,0.3]", sep = ""),
            "-t", "Affine[1]",
            "-c", myconvhi,
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }

        if (typeofTransform == "SyNabp") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste("mattes[", f, ",", m, ",1,32,regular,", samplingPercentage, "]", sep = ""),
            "-t", "Rigid[0.1]", "-c", "1000x500x250x100", "-s", "4x2x1x0",
            "-f", "8x4x2x1",
            "-x", earlyMaskOption,
            "-m", paste("mattes[", f, ",", m, ",1,32,regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.1]", "-c", "1000x500x250x100", "-s", "4x2x1x0",
            "-f", "8x4x2x1",
            "-x", earlyMaskOption,
            "-m", paste("CC[", f, ",", m, ",0.5,4]", sep = ""),
            "-t", paste("SyN[0.1,3,0]", sep = ""), "-c", "50x10x0",
            "-s", "2x1x0", "-f", "4x2x1", "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }

        if (typeofTransform == "SyNLessAggro") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", samplingPercentage, "]", sep = ""),
            "-t", "Affine[0.25]", "-c", "2100x1200x1200x100", "-s", "3x2x1x0",
            "-f", "4x2x2x1",
            "-x", earlyMaskOption,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", mysyn,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "TVMSQ") {
          if (is.na(gradStep)) gradStep <- 1.0
          tvtx <- paste("TimeVaryingVelocityField[",
            gradStep, ", 4,", flowSigma, ",0.0,", totalSigma, ",0 ]",
            sep = ""
          )
          args <- list(
            "-d", as.character(fixed@dimension), # "-r", initx,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", tvtx,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-u", "0", "-z", "0",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform == "TVMSQC") {
          if (is.na(gradStep)) gradStep <- 2.0
          tvtx <- paste("TimeVaryingVelocityField[",
            gradStep, ", 8, 1.0,0.0, 0.05,0 ]",
            sep = ""
          )
          args <- list(
            "-d", as.character(fixed@dimension), # "-r", initx,
            "-m", paste("demons[", f, ",", m, ",0.5,0]", sep = ""),
            "-m", paste("meansquares[", f, ",", m, ",1,0]", sep = ""),
            "-t", tvtx,
            "-c", "[1200x1200x100x20x0,0,5]",
            "-s", "8x6x4x2x1vox",
            "-f", "8x6x4x2x1",
            "-u", "0", "-z", "0",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }
        if (typeofTransform %in% tvTypes) {
          if (is.na(gradStep)) {
            gradStep <- 1.0
          }
          nTimePoints <- as.numeric(strsplit(strsplit(typeofTransform, "\\[")[[1]][2], "\\]")[[1]][1])
          if (is.na(gradStep)) gradStep <- 1.0
          tvtx <- paste("TimeVaryingVelocityField[",
            gradStep, ",", nTimePoints, ",", flowSigma, ",0.0,", totalSigma, ",0 ]",
            sep = ""
          )
          args <- list(
            "-d", as.character(fixed@dimension), # "-r", initx,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", tvtx,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-u", "0", "-z", "0",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }

        if (typeofTransform == "Elastic") {
          if (is.na(gradStep)) gradStep <- 0.25
          tvtx <- paste("GaussianDisplacementField[",
            gradStep, ",", flowSigma, ",", totalSigma, "]",
            sep = ""
          )
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(synMetric, "[", f, ",", m, ",1,", synSampling, "]", sep = ""),
            "-t", tvtx,
            "-c", paste("[", synits, ",1e-7,8]", collapse = ""),
            "-s", smoothingsigmas,
            "-f", shrinkfactors,
            "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }


        if (typeofTransform == "Rigid" |
          typeofTransform == "Similarity" |
          typeofTransform == "Translation" |
          typeofTransform == "Affine") {
          args <- list(
            "-d", as.character(fixed@dimension), "-r", initx,
            "-m", paste(affMetric, "[", f, ",", m, ",1,", affSampling, ",regular,", metsam, "]", sep = ""),
            "-t", paste(typeofTransform, "[0.25]", sep = ""), "-c", myiterations,
            "-s", mysAff, "-f", myfAff, "-u", "0", "-z", "1",
            "-o", paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""),
            "-x", maskOption
          )
        }

        if (grepl("antsRegistrationSyN", typeofTransform)) {
          doQuick <- FALSE
          if (grepl("Quick", typeofTransform)) {
            doQuick <- TRUE
          }

          subtypeOfTransform <- "s"
          splineDistance <- 26
          metricParameter <- 4
          if (doQuick) {
            metricParameter <- 32
          }

          if (grepl("\\[", typeofTransform) && grepl("\\]", typeofTransform)) {
            subtypeOfTransform <- strsplit(strsplit(typeofTransform, "\\[")[[1]][2], "\\]")[[1]][1]
            if (grepl(",", subtypeOfTransform)) {
              subtypeOfTransformArgs <- strsplit(subtypeOfTransform, ",")[[1]]
              subtypeOfTransform <- subtypeOfTransformArgs[1]
              if (!(subtypeOfTransform == "b" ||
                subtypeOfTransform == "br" ||
                subtypeOfTransform == "bo" ||
                subtypeOfTransform == "s" ||
                subtypeOfTransform == "sr" ||
                subtypeOfTransform == "so")) {
                stop("Extra parameters are only valid for B-spline SyN transform.")
              }
              metricParameter <- subtypeOfTransformArgs[2]
              if (length(subtypeOfTransformArgs) > 2) {
                splineDistance <- subtypeOfTransformArgs[3]
              }
            }
          }


          doRepro <- FALSE
          if (grepl("Repro", typeofTransform)) {
            doRepro <- TRUE
          }

          if (doQuick == TRUE) {
            rigidConvergence <- "[1000x500x250x0,1e-6,10]"
          } else {
            rigidConvergence <- "[1000x500x250x100,1e-6,10]"
          }
          rigidShrinkFactors <- "8x4x2x1"
          rigidSmoothingSigmas <- "3x2x1x0vox"

          if (doQuick == TRUE) {
            affineConvergence <- "[1000x500x250x0,1e-6,10]"
          } else {
            affineConvergence <- "[1000x500x250x100,1e-6,10]"
          }
          affineShrinkFactors <- "8x4x2x1"
          affineSmoothingSigmas <- "3x2x1x0vox"

          linearMetric <- "MI"
          linearMetricParameter <- "32"
          if (doRepro == TRUE) {
            linearMetric <- "GC"
            linearMetricParameter <- "1"
          }

          if (doQuick == TRUE) {
            synConvergence <- "[100x70x50x0,1e-6,10]"
            synMetric <- paste0("MI[", f, ",", m, ",1,", metricParameter, "]")
          } else {
            synConvergence <- "[100x70x50x20,1e-6,10]"
            synMetric <- paste0("CC[", f, ",", m, ",1,", metricParameter, "]")
          }
          synShrinkFactors <- "8x4x2x1"
          synSmoothingSigmas <- "3x2x1x0vox"

          tx <- "Rigid"
          if (subtypeOfTransform == "t") {
            tx <- "Translation"
          }

          if (doQuick == TRUE && doRepro == TRUE) {
            synConvergence <- "[100x70x50x0,1e-6,10]"
            synMetric <- paste0("CC[", f, ",", m, ",1", metricParameter, "]")
          }

          if (missing(randomSeed) && doRepro == TRUE) {
            randomSeed <- 1
          }

          rigidStage <- list(
            "--transform", paste0(tx, "[0.1]"),
            "--metric", paste0(linearMetric, "[", f, ",", m, ",1,", linearMetricParameter, ",regular,", samplingPercentage, "]"),
            "--convergence", rigidConvergence,
            "--shrink-factors", rigidShrinkFactors,
            "--smoothing-sigmas", rigidSmoothingSigmas
          )

          affineStage <- list(
            "--transform", "Affine[0.1]",
            "--metric", paste0(linearMetric, "[", f, ",", m, ",1,", linearMetricParameter, ",regular,", samplingPercentage, "]"),
            "--convergence", affineConvergence,
            "--shrink-factors", affineShrinkFactors,
            "--smoothing-sigmas", affineSmoothingSigmas
          )

          if (subtypeOfTransform == "sr" || subtypeOfTransform == "br") {
            if (doQuick == TRUE) {
              synConvergence <- "[50x0,1e-6,10]"
            } else {
              synConvergence <- "[50x20,1e-6,10]"
            }
            synShrinkFactors <- "2x1"
            synSmoothingSigmas <- "1x0vox"
          }

          synTransform <- "SyN[0.1,3,0]"
          if (subtypeOfTransform == "b" || subtypeOfTransform == "br" || subtypeOfTransform == "bo") {
            synTransform <- paste0("BSplineSyN[0.1,", splineDistance, ",0,3]")
          }
          synStage <- list(
            "--transform", synTransform,
            "--metric", synMetric
          )

          if (!missing(multivariateExtras)) {
            for (mm in seq.int(length(multivariateExtras)))
            {
              if (length(multivariateExtras[[mm]]) != 5) {
                stop(paste0(
                  "multivariate metric needs 5 entries: ",
                  "name of metric, fixed, moving, weight, ",
                  "samplingParam"
                ))
              }
              synStage <- lappend(synStage, list(
                "--metric", paste(
                  as.character(multivariateExtras[[mm]][[1]]), "[",
                  antsrGetPointerName(multivariateExtras[[mm]][[2]]), ",",
                  antsrGetPointerName(multivariateExtras[[mm]][[3]]), ",",
                  as.character(multivariateExtras[[mm]][[4]]), ",",
                  as.character(multivariateExtras[[mm]][[5]]), "]",
                  sep = ""
                )
              ))
            }
          }
          synStage <- lappend(synStage, list(
            "--convergence", synConvergence,
            "--shrink-factors", synShrinkFactors,
            "--smoothing-sigmas", synSmoothingSigmas
          ))

          args <- list(
            "-d", as.character(fixed@dimension),
            "-r", initx,
            "-o", paste0("[", outprefix, ",", wmo, ",", wfo, "]")
          )

          if (subtypeOfTransform == "r" || subtypeOfTransform == "t") {
            args <- lappend(args, rigidStage)
          } else if (subtypeOfTransform == "a") {
            args <- lappend(args, rigidStage)
            args <- lappend(args, affineStage)
          } else if (subtypeOfTransform == "b" || subtypeOfTransform == "s") {
            args <- lappend(args, rigidStage)
            args <- lappend(args, affineStage)
            args <- lappend(args, synStage)
          } else if (subtypeOfTransform == "br" || subtypeOfTransform == "sr") {
            args <- lappend(args, rigidStage)
            args <- lappend(args, synStage)
          } else if (subtypeOfTransform == "bo" || subtypeOfTransform == "so") {
            args <- lappend(args, synStage)
          }
          args <- lappend(args, list("-x", maskOption))

          args <- as.list(unlist(args))
        }

        if (!missing(restrictTransformation)) {
          args[[length(args) + 1]] <- "-g"
          args[[length(args) + 1]] <- paste(restrictTransformation, collapse = "x")
        }

        args[[length(args) + 1]] <- "--float"
        args[[length(args) + 1]] <- "1"
        # set the random seed
        if (missing(randomSeed)) {
          myseed <- Sys.getenv("ANTS_RANDOM_SEED")
          if (nchar(myseed) == 0) myseed <- "1234"
        }
        args[[length(args) + 1]] <- "--random-seed"
        args[[length(args) + 1]] <- "1"
        args[[length(args) + 1]] <- "--write-composite-transform"
        args[[length(args) + 1]] <- as.character(as.numeric(writeCompositeTransform))
        if (verbose) {
          args[[length(args) + 1]] <- "-v"
          args[[length(args) + 1]] <- "1"
        }

        if (printArgs) {
          cat("antsRegistration", paste(unlist(args)), "\n")
        }
        args <- .int_antsProcessArguments(c(args))
        ANTsRCore::antsRegistration(args)

        all_tx <- find_tx(outprefix)
        alltx <- all_tx$alltx
        findinv <- all_tx$findinv
        findfwd <- all_tx$findfwd
        findaff <- all_tx$findaff
        velocityfield <- all_tx$velocityfield
        rm(list = "all_tx")
        # this will make it so other file naming don't mess this up
        alltx <- alltx[findinv | findfwd | findaff]

        if (any(findinv)) {
          fwdtransforms <- rev(alltx[findfwd | findaff])
          invtransforms <- alltx[findinv | findaff]
          if (length(fwdtransforms) != length(invtransforms)) {
            message(paste0(
              "transform composite list may not be ",
              "invertible - return all transforms and ",
              "leave it to user to figure it out"
            ))
            invtransforms <- alltx
          }
        } else {
          fwdtransforms <- rev(alltx)
          invtransforms <- (alltx)
        }
        if (writeCompositeTransform) {
          fwdtransforms <- paste0(outprefix, "Composite.h5")
          invtransforms <- paste0(outprefix, "InverseComposite.h5")
        }
        if (sum(fixed - warpedmovout) == 0) { # FIXME better error catching
          stop("Registration failed. Use verbose mode to diagnose.")
        }

        if (length(velocityfield) == 0) {
          return(
            list(
              warpedmovout = antsImageClone(warpedmovout, inpixeltype),
              warpedfixout = antsImageClone(warpedfixout, inpixeltype),
              fwdtransforms = fwdtransforms,
              invtransforms = invtransforms,
              prev_transforms = pre_transform
            )
          )
        } else {
          return(
            list(
              warpedmovout = antsImageClone(warpedmovout, inpixeltype),
              warpedfixout = antsImageClone(warpedfixout, inpixeltype),
              fwdtransforms = fwdtransforms,
              invtransforms = invtransforms,
              prev_transforms = pre_transform,
              velocityfield = velocityfield
            )
          )
        }
      }
      if (!ttexists) {
        stop("Unrecognized transform type.")
      }
    }
    return(0)
  }
  args[[length(args) + 1]] <- "--float"
  args[[length(args) + 1]] <- "1"
  if (verbose) {
    args[[length(args) + 1]] <- "-v"
    args[[length(args) + 1]] <- "1"
  }

  if (printArgs) {
    cat("antsRegistration", paste(unlist(args)), "\n")
  }
  args <- .int_antsProcessArguments(c(args))
  ANTsRCore::antsRegistration(args)
}

################################
# .antsrmakeRandomString(n, length) function generates a random string random
# string of the length (length), made up of numbers, small and capital letters
# helper function
################################
.antsrmakeRandomString <- function(n = 1, mylength = 12) {
  randomString <- c(1:n) # initialize vector
  for (i in 1:n) {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS), mylength, replace = TRUE),
      collapse = ""
    )
  }
  return(randomString)
}


#' Return the antsImage pointer string
#'
#' This is a low-level function that may be useful for debugging.  It will
#' return the pointer name itself from an antsImage object.
#'
#' @param img  image whose pointer we want
#' @return string
#' @author Avants BB
#' @examples
#'
#' img <- antsImageRead(getANTsRData("r16"))
#' antsrGetPointerName(img)
#' antsrGetPointerName(antsImageClone(img))
#'
#' @export antsrGetPointerName
antsrGetPointerName <- function(img) {
  # if ( Sys.info()['sysname'] == 'Linux' ) endofpointer<-20 if (
  # Sys.info()['sysname'] == 'Darwin' ) endofpointer<-21 pname<- substr(
  # .int_antsProcessArguments( list( img ) ) , 11 , endofpointer )
  splitptrname <- strsplit(.int_antsProcessArguments(list(img)), " ")[[1]][2]
  pname <- strsplit(splitptrname, ">")
  return(pname[[1]])
}






#' ANTs template building.
#'
#' Iteratively estimate a population shape and intensity average image.  This
#' can be computationally intensive and currently is not parallelized.  Perhaps
#' better to use official \code{antsMultivariateTemplateConstruction*} in ANTs.
#' However, this code can be useful for smaller problems/populations.
#'
#' @param initialTemplate fixed image to which we register the population.
#' @param imgList moving image list from which template will be built.
#' @param typeofTransform A linear or non-linear registration type.  Mutual
#' information metric by default. See \code{antsRegistration.}
#' @param iterations should be greater than 1 less than 10.
#' @param gradientStep speed of template shape update step, less than 1.
#' @param blendingWeight parameter between zero and one, default 0.5; higher
#' values lead to sharper intensity but may also cause artifacts.
#' @param segList segmentations for each target image, this will trigger a
#' joint label fusion call for each iteration and use masks during registration.
#' @param weights numeric vector, length of number of images providing weights
#' @param verbose print diagnostic messages,
#' passed to \code{\link{antsRegistration}}
#' @param ... Additional options to pass to \code{\link{antsRegistration}}
#' @return template antsImage
#' @author Avants BB
#' @examples
#' pop <- getANTsRData("population")
#' avg <- antsAverageImages(pop) # this is in ANTsR
#' template <- buildTemplate(avg, pop, "SyN", iterations = 1)
#' @export buildTemplate
buildTemplate <- function(
    initialTemplate,
    imgList,
    typeofTransform,
    iterations = 3,
    gradientStep = 0.25,
    blendingWeight = 0.5,
    segList,
    weights,
    verbose = TRUE,
    ...) {
  template <- antsImageClone(initialTemplate)
  if (blendingWeight < 0) blendingWeight <- 0
  if (blendingWeight > 1) blendingWeight <- 1
  if (!missing(segList)) doJif <- TRUE else doJif <- FALSE
  if (missing(weights)) weights <- as.numeric(1:length(imgList))
  weights <- weights / sum(weights)
  for (i in 1:iterations) {
    if (verbose) {
      message(paste0("Iteration: ", i))
    }
    avgIlist <- list()
    avgWlist <- c()
    avgSlist <- list()
    for (k in 1:length(imgList)) {
      if (doJif & (i > 1)) {
        segreglist <- list(
          thresholdImage(segmentation, 1, Inf),
          thresholdImage(segList[[k]], 1, Inf)
        )
        w1 <- antsRegistration(
          template,
          imgList[[k]],
          typeofTransform = typeofTransform,
          verbose = verbose > 1,
          mask = segreglist,
          ...
        )
      } else {
        w1 <- antsRegistration(
          template,
          imgList[[k]],
          typeofTransform = typeofTransform,
          verbose = verbose > 1,
          ...
        )
      }
      avgIlist[[k]] <- w1$warpedmovout
      avgWlist[k] <- antsApplyTransforms(
        initialTemplate, imgList[[k]],
        w1$fwdtransforms,
        compose = w1$fwdtransforms[1]
      )
      if (doJif) {
        avgSlist[[k]] <- antsApplyTransforms(
          initialTemplate, segList[[k]],
          w1$fwdtransforms,
          interpolator = "nearestNeighbor"
        )
      }
    }
    if (verbose) {
      message("Averaging images")
    }
    template <- antsAverageImages(avgIlist, weights = weights)
    if (doJif) {
      tempmask <- thresholdImage(
        antsAverageImages(avgSlist, weights = weights),
        1 / length(avgSlist), Inf
      )
      jlf <- jointLabelFusion(template, tempmask,
        rSearch = 3,
        avgIlist, labelList = avgSlist
      )
      template <- jlf$intensity
      segmentation <- antsImageClone(jlf$segmentation, "float")
    }
    if (verbose) {
      message("Averaging warped composed transforms")
    }
    wavg <- antsAverageImages(avgWlist, weights = weights) * (-1.0 * gradientStep)
    wmag <- sqrt(mean(wavg^2))
    wavgfn <- tempfile(fileext = ".nii.gz")
    antsImageWrite(wavg, wavgfn)
    template <- antsApplyTransforms(template, template, wavgfn)
    if (doJif) {
      segmentation <- antsApplyTransforms(segmentation, segmentation, wavgfn,
        interpolator = "nearestNeighbor"
      )
    }
    if (verbose) {
      message(paste0("Blending template image", wmag))
    }
    template <- template * (1.0 - blendingWeight) +
      iMath(template, "Sharpen") * blendingWeight
  }
  if (doJif) {
    return(list(template = template, segmentation = segmentation))
  }
  return(template)
}
