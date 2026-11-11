# Coverage tests for fMRI preprocessing: motion correction, nuisance
# variables, resting/task preprocessing and fMRINormalization.

make_cov_task_bold <- function(d = c(10, 10, 8), nt = 30, tr = 2, seed = 1) {
  set.seed(seed)
  g <- as.matrix(expand.grid(1:d[1], 1:d[2], 1:d[3]))
  r <- array(sqrt(rowSums(sweep(g, 2, (d + 1) / 2)^2 / (d / 2.3)^2)), d)
  base <- array(ifelse(r < 1, 1000, 50), d)
  hrf <- hemodynamicRF(nt, onsets = seq(3, nt - 5, by = 10), durations = 4, rt = tr)
  arr <- array(0, c(d, nt))
  for (t in 1:nt) {
    arr[, , , t] <- base + rnorm(prod(d), 0, 5) + (r < 0.6) * 2000 * hrf[t]
  }
  img <- as.antsImage(arr)
  antsSetSpacing(img, c(3, 3, 3, tr))
  list(img = img, hrf = hrf, active = r < 0.6, brain = r < 1)
}

# Structural-like phantom: 3 tissue classes (csf = 1, gm = 2, wm = 3)
make_cov_struct_bold <- function(d = c(20, 20, 14), nt = 21, tr = 2, seed = 1,
                                 shift = NA) {
  set.seed(seed)
  g <- as.matrix(expand.grid(1:d[1], 1:d[2], 1:d[3]))
  r <- array(sqrt(rowSums(sweep(g, 2, (d + 1) / 2)^2 / (d / 2.3)^2)), d)
  seg <- ifelse(r < 0.45, 3, ifelse(r < 0.75, 2, ifelse(r < 1, 1, 0)))
  base <- array(c(0, 900, 600, 300)[seg + 1], d)
  arr <- array(0, c(d, nt))
  for (t in 1:nt) {
    b <- base
    if (!is.na(shift) && t == shift) b <- b[c(2:d[1], 1), , ] # 1-voxel jump
    arr[, , , t] <- b + rnorm(prod(d), 0, 10) * (b > 0)
  }
  img <- as.antsImage(arr)
  antsSetSpacing(img, c(3, 3, 3, tr))
  s <- as.antsImage(seg)
  antsSetSpacing(s, c(3, 3, 3))
  list(img = img, seg = s)
}

# antsrMotionCalculation() passes 1-based slice/direction indices to the
# 0-based ANTsRCore::ExtractSlice() (package bug), which makes it and
# fMRINormalization() unusable.  To still exercise the downstream code we
# wrap ExtractSlice so that a failing call is retried with 0-based indices.
local_fixed_extract_slice <- function(env = parent.frame()) {
  real <- ANTsRCore::ExtractSlice
  fixed <- function(image, slice, direction, collapseStrategy = 0) {
    out <- try(real(image, slice, direction, collapseStrategy), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- real(image, slice - 1, direction - 1, collapseStrategy)
    }
    out
  }
  testthat::local_mocked_bindings(
    ExtractSlice = fixed, .package = "ANTsRCore",
    .env = env
  )
}

test_that(".motion_correction argument handling and accuracy levels", {
  b <- make_cov_task_bold(nt = 8)
  mc <- ANTsR:::.motion_correction
  capture.output({
    expect_null(mc(c("a", "b")))
    expect_null(mc(list()))
    expect_null(mc(getAverageOfTimeSeries(b$img)))
    expect_null(mc(b$img, fixed = c("a", "b")))
    expect_null(mc(b$img, fixed = list()))
    expect_null(mc(b$img, fixed = b$img))
  })
  f4 <- tempfile(fileext = ".nii.gz")
  f3 <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(f3, f4)), add = TRUE)
  antsImageWrite(b$img, f4)
  avg <- getAverageOfTimeSeries(b$img)
  antsImageWrite(avg, f3)
  r1 <- mc(f4, fixed = f3, moreaccurate = 1)
  expect_equal(nrow(r1$moco_params), 8)
  expect_equal(dim(r1$moco_img), dim(b$img))
  capture.output(r2 <- mc(antsImageClone(b$img, "double"),
    fixed = antsImageClone(avg, "double"), moreaccurate = 0, txtype = "Rigid"
  ))
  expect_equal(nrow(r2$moco_params), 8)
  expect_true(is.antsImage(r2$moco_avg_img))
})

test_that("antsMotionCalculation returns parameters, FD and DVARS", {
  b <- make_cov_task_bold(nt = 8)
  m <- antsMotionCalculation(b$img, moreaccurate = 0)
  expect_equal(nrow(m$moco_params), 8)
  expect_equal(names(m$fd), c("MeanDisplacement", "MaxDisplacement"))
  expect_length(m$dvars, 8)
  expect_equal(dim(m$moco_img), dim(b$img))
})

test_that("antsrMotionCalculation errors (slice index bug) and works when patched", {
  set.seed(2)
  img3 <- makeImage(c(16, 16, 4), 0)
  base <- matrix(0, 16, 16)
  base[5:12, 5:12] <- 100
  arr <- array(0, c(16, 16, 4))
  for (t in 1:4) arr[, , t] <- base + rnorm(256)
  img3 <- as.antsImage(arr)
  expect_error(antsrMotionCalculation(img3, typeofTransform = "Rigid"), "ITK ERROR")
  local_fixed_extract_slice()
  # (motion descriptors for 2D+t data are not supported by ANTsRCore;
  # the 4D descriptor path is exercised through fMRINormalization below)
  capture.output(m <- antsrMotionCalculation(img3,
    typeofTransform = "Rigid",
    verbose = TRUE, getMotionDescriptors = FALSE
  ))
  expect_equal(nrow(m$moco_params), 4)
  expect_equal(dim(m$moco_img), dim(img3))
  m2 <- antsrMotionCalculation(img3,
    fixed = getAverageOfTimeSeries(img3),
    mask = getMask(getAverageOfTimeSeries(img3)),
    typeofTransform = "Rigid", getMotionDescriptors = FALSE
  )
  expect_true(is.na(m2$fd))
  expect_true(is.na(m2$dvars))
  expect_true(all(grepl("MOCOparam", colnames(m2$moco_params))))
})

test_that("preprocessRestingBOLD cleans BOLD with compcor, filtering and smoothing", {
  b <- make_cov_task_bold()
  p1 <- preprocessRestingBOLD(b$img,
    motionCorrectionAccuracyLevel = 0,
    numberOfCompCorComponents = 2, frequencyLowThreshold = 0.01,
    frequencyHighThreshold = 0.2, spatialSmoothingType = "gaussian",
    spatialSmoothingParameters = 1
  )
  expect_equal(dim(p1$cleanBoldImage), dim(b$img))
  expect_length(p1$FD, 30)
  expect_length(p1$DVARS, 30)
  expect_length(p1$globalSignal, 30)
  # 12 affine motion parameters + 2 compcor components
  expect_equal(ncol(p1$nuisanceVariables), 14)
  expect_true(mean(p1$DVARSpostCleaning) < mean(p1$DVARS))
  mask <- getMask(getAverageOfTimeSeries(b$img))
  p2 <- preprocessRestingBOLD(b$img,
    maskImage = mask, initialNuisanceVariables = matrix(rnorm(30), ncol = 1),
    motionCorrectionAccuracyLevel = 0, motionCorrectionIterations = 1,
    meanBoldFixedImageForMotionCorrection = getAverageOfTimeSeries(b$img),
    numberOfCompCorComponents = 0, useMotionCorrectedImage = TRUE,
    spatialSmoothingType = "perona-malik", spatialSmoothingParameters = c(2, 0.5)
  )
  expect_equal(ncol(p2$nuisanceVariables), 13)
  expect_equal(sum(p2$maskImage), sum(mask))
  capture.output(p3 <- preprocessRestingBOLD(b$img,
    doMotionCorrection = FALSE,
    useMotionCorrectedImage = TRUE, numberOfCompCorComponents = 2
  ))
  expect_true(all(p3$FD == 0))
  expect_error(preprocessRestingBOLD(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "gaussian",
    spatialSmoothingParameters = c(1, 2)
  ), "single scalar")
  expect_error(preprocessRestingBOLD(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "perona-malik",
    spatialSmoothingParameters = 1
  ), "two element")
  expect_error(preprocessRestingBOLD(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "median"
  ), "Unrecognized")
  expect_length(ANTsR:::computeDVARSspatialMap(matrix(rnorm(40), 10, 4)), 4)
})

test_that("preprocessfMRI cleans BOLD and reports motion", {
  b <- make_cov_task_bold()
  p1 <- preprocessfMRI(b$img,
    motionCorrectionAccuracyLevel = 0,
    numberOfCompCorComponents = 2, spatialSmoothingType = "perona-malik",
    spatialSmoothingParameters = c(2, 0.5), frequencyLowThreshold = 0.01,
    frequencyHighThreshold = 0.2
  )
  expect_equal(dim(p1$cleanBoldImage), dim(b$img))
  expect_equal(ncol(p1$nuisanceVariables), 14)
  expect_length(p1$FD, 30)
  p2 <- preprocessfMRI(b$img,
    maskImage = getMask(getAverageOfTimeSeries(b$img)),
    initialNuisanceVariables = matrix(rnorm(30), ncol = 1),
    motionCorrectionAccuracyLevel = 0, numberOfCompCorComponents = 0,
    useMotionCorrectedImage = TRUE, spatialSmoothingType = "gaussian",
    spatialSmoothingParameters = 1
  )
  expect_equal(ncol(p2$nuisanceVariables), 13)
  capture.output(p3 <- preprocessfMRI(b$img,
    doMotionCorrection = FALSE,
    useMotionCorrectedImage = TRUE, numberOfCompCorComponents = 2
  ))
  expect_true(all(p3$FD == 0))
  expect_error(preprocessfMRI(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "gaussian",
    spatialSmoothingParameters = c(1, 2)
  ), "single scalar")
  expect_error(preprocessfMRI(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "perona-malik",
    spatialSmoothingParameters = 1
  ), "two element")
  expect_error(preprocessfMRI(b$img,
    doMotionCorrection = FALSE,
    numberOfCompCorComponents = 2, spatialSmoothingType = "box"
  ), "Unrecognized")
})

test_that("getfMRInuisanceVariables and taskFMRI detect task activation", {
  skip_if_not_installed("magic")
  b <- make_cov_task_bold()
  capture.output({
    expect_null(getfMRInuisanceVariables())
    expect_null(getfMRInuisanceVariables(b$img, maskThresh = "a"))
    expect_null(getfMRInuisanceVariables(c("a", "b")))
    expect_null(getfMRInuisanceVariables(list()))
    expect_null(getfMRInuisanceVariables(getAverageOfTimeSeries(b$img)))
  })
  capture.output(nv <- getfMRInuisanceVariables(antsImageClone(b$img, "double"),
    maskThresh = 500, moreaccurate = 0
  ))
  nbrain <- sum(nv$mask)
  expect_equal(dim(nv$matrixTimeSeries), c(30, nbrain))
  expect_equal(colnames(nv$nuisancevariables), paste0("denoisingParams", 1:3))
  expect_length(nv$globalsignal, 30)
  capture.output(nv2 <- getfMRInuisanceVariables(b$img,
    moreaccurate = 0,
    mask = nv$mask
  ))
  expect_equal(sum(nv2$mask), nbrain)
  # taskFMRI: hrf must not carry column names (colnames bug otherwise)
  h <- matrix(b$hrf, ncol = 1)
  capture.output(expect_equal(taskFMRI(), 1))
  capture.output(tf <- taskFMRI(nv$matrixTimeSeries, h, nv,
    myformula = "denoisingParams1 + denoisingParams2"
  ))
  expect_length(tf$beta, nbrain)
  act <- nv$mask * 0
  act[nv$mask == 1] <- tf$beta
  activemask <- as.antsImage(b$active * 1)
  antsCopyImageInfo(nv$mask, activemask)
  expect_true(mean(act[activemask == 1]) > mean(act[nv$mask == 1 & activemask == 0]))
  capture.output(tf2 <- taskFMRI(nv$matrixTimeSeries, h, nv,
    myformula = "denoisingParams1",
    correctautocorr = TRUE, residualizedesignmatrix = TRUE
  ))
  expect_equal(dim(tf2$fmrimat), dim(nv$matrixTimeSeries))
  hn <- h
  colnames(hn) <- "hrf"
  capture.output(expect_error(taskFMRI(nv$matrixTimeSeries, hn, nv,
    myformula = "denoisingParams1"
  ), "dimnames"))
  # AR(2) correction helper
  set.seed(3)
  e <- as.numeric(arima.sim(list(ar = c(0.6, -0.2)), n = 100))
  ar <- ANTsR:::.arCorrection(matrix(e, ncol = 1), loops = 2)
  expect_equal(nrow(ar$outmat), 100)
  expect_true(abs(ar$arcoefs[1]) < abs(ar$initcoefs[1]))
})

test_that("fMRINormalization runs end-to-end on a small phantom (patched slice extraction)", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("pracma")
  skip_if_not_installed("mFilter")
  skip_if_not_installed("ggplot2")
  local_fixed_extract_slice()
  b <- make_cov_struct_bold(shift = 12)
  structimg <- getAverageOfTimeSeries(b$img)
  na <- array(0, dim(b$seg))
  na[5:9, 5:15, 4:10] <- 1
  na[12:16, 5:15, 4:10] <- 2
  nodes <- as.antsImage(na)
  antsCopyImageInfo(b$seg, nodes)
  expect_error(fMRINormalization(b$img, extraRuns = b$seg, steadyT = 2), "list")
  capture.output(res <- fMRINormalization(b$img,
    steadyT = 2, repeatMotionEst = 1,
    polydegree = 2, nCompCor = 2, freqLimits = c(0.01, 0.2), fdthresh = 1,
    structuralImage = structimg, structuralSeg = b$seg,
    structuralNodes = nodes
  ))
  # 21 - 1 frames kept after dropping the steady-state frame
  ntimes <- 20
  expect_equal(dim(res$fusedImg)[4], ntimes)
  expect_equal(levels(res$runID), "1")
  expect_equal(nrow(res$boldMat), ntimes)
  expect_equal(nrow(res$nuisance), ntimes)
  expect_true(all(c("compcor1", "compcor2", "globalMean", "dvars", "runs") %in%
    colnames(res$nuisance)))
  expect_length(res$FD, ntimes)
  # the shifted frame (frame 12 -> index 11 after cropping) exceeds fdthresh
  expect_true(11 %in% res$badtimes)
  expect_equal(dim(res$connMatNodes), c(2, 2))
  expect_equal(dim(res$connMatNodesPartialCorr), c(2, 2))
  # no templateMap: template-space outputs are NA (the template branch maps
  # all 264 Powers points through a slow per-voxel loop, so it is skipped)
  expect_true(all(is.na(res$powersLabels)))
  expect_true(all(is.na(res$mapsToTemplate)))
  expect_true(is.antsImage(res$seg2bold))
})
