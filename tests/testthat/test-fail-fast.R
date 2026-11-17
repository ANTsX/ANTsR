# Bad arguments must be rejected before slow work starts. Each test replaces
# the expensive step with a stub that errors if it is reached, so the test
# fails if the argument check runs afterwards (as it used to).
reached <- function(...) stop("expensive step was reached")

test_that("labelImageRegistration validates its specification before registering", {
  local_mocked_bindings(.package = "ANTsR", fitTransformToPairedPoints = reached,
    antsRegistration = reached)
  a <- array(0, c(16, 16))
  a[3:6, 3:6] <- 1
  a[9:12, 3:6] <- 2
  a[3:6, 9:12] <- 3
  img <- as.antsImage(a)
  expect_error(labelImageRegistration(img, img, typeOfDeformableTransform = "antsRegistrationSyNQuick[xx]"),
    "Only 'so' or 'bo'")
  expect_error(labelImageRegistration(img, img, typeOfDeformableTransform = list("SyN", 0.2)),
    "Incorrect specification")
  expect_error(labelImageRegistration(img, img, typeOfDeformableTransform = c("a", "b")),
    "Incorrect specification")
  expect_error(labelImageRegistration(img, img, initialTransforms = tempfile(fileext = ".mat")),
    "does not exist")
})

test_that("preprocessfMRI / preprocessRestingBOLD check smoothing before motion correction", {
  local_mocked_bindings(.package = "ANTsR", .motion_correction = reached,
    timeseries2matrix = reached, getAverageOfTimeSeries = reached)
  bold <- makeImage(c(4, 4, 4, 6), rnorm(384))
  for (f in list(preprocessfMRI, preprocessRestingBOLD)) {
    expect_error(f(bold, spatialSmoothingType = "box"), "Unrecognized smoothing option")
    expect_error(f(bold, spatialSmoothingType = "gaussian", spatialSmoothingParameters = c(1, 2)),
      "single scalar parameter")
    expect_error(f(bold, spatialSmoothingType = "perona-malik", spatialSmoothingParameters = 1),
      "two element vector")
  }
  expect_true(ANTsR:::.checkSpatialSmoothing("none", 0))
  expect_true(ANTsR:::.checkSpatialSmoothing("gaussian", 2))
  expect_true(ANTsR:::.checkSpatialSmoothing("perona-malik", c(1, 2)))
})

test_that("multiResRandomForestRegression checks for randomForest first", {
  local_mocked_bindings(.package = "ANTsR", usePkg = function(...) FALSE,
    getMultiResFeatureMatrix = reached)
  lm <- makeImage(c(8, 8), 0)
  lm[2:7, 2:7] <- 1
  x <- list(list(makeImage(c(8, 8), rnorm(64))), list(makeImage(c(8, 8), rnorm(64))))
  expect_error(multiResRandomForestRegression(1:2, x, list(lm, lm), rad = c(1, 1)),
    "install the randomForest package")
})

test_that("filterfMRIforNetworkAnalysis checks labels/mask and nuisancein before filtering", {
  local_mocked_bindings(.package = "ANTsR", frequencyFilterfMRI = reached)
  mat <- matrix(rnorm(40 * 64), 40, 64)
  mask <- makeImage(c(4, 4, 4), 1)
  lab <- makeImage(c(4, 4, 4), rep(1:2, 32))
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, labels = lab), "mask is required")
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = data.frame(a = rnorm(10))), "10 rows but there are 40 time points")
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = c(NA, rnorm(39))), "nuisancein must not contain NA")
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = data.frame(a = letters[1:40 %% 26 + 1])), "nuisancein must be numeric")
})

test_that("functionalLungSegmentation checks clusterCenters before bias correction", {
  local_mocked_bindings(.package = "ANTsR", n4BiasFieldCorrection = reached, atropos = reached)
  img <- makeImage(c(8, 8, 8), runif(512))
  mask <- makeImage(c(8, 8, 8), 1)
  expect_error(functionalLungSegmentation(img, mask, numberOfClusters = 3, clusterCenters = c(1, 2),
    verbose = FALSE), "numberOfClusters should match")
})

test_that("fMRINormalization checks extraRuns before any processing", {
  for (p in c("ggplot2", "igraph", "pracma", "mFilter")) skip_if_not_installed(p)
  local_mocked_bindings(.package = "ANTsR", timeseries2matrix = reached,
    antsrMotionCalculation = reached, getANTsRData = reached)
  img <- makeImage(c(4, 4, 4, 6), rnorm(384))
  expect_error(fMRINormalization(img, extraRuns = list("not an image")),
    "extraRuns must be a list of antsImages")
})

test_that("aslAveraging rejects invalid or vector methods with a clear error", {
  local_mocked_bindings(.package = "ANTsR", timeseries2matrix = reached, getMask = reached)
  asl <- makeImage(c(4, 4, 4, 6), rnorm(384))
  expect_error(aslAveraging(asl, method = "median"), "Unrecognized method")
  expect_error(aslAveraging(asl, method = c("regression", "bayesian")), "Unrecognized method")
})

test_that("generateAifMaskImage and compcor check their indices before timeseries2matrix", {
  local_mocked_bindings(.package = "ANTsR", timeseries2matrix = reached)
  p4 <- makeImage(c(4, 4, 4, 10), runif(640))
  voi <- makeImage(c(4, 4, 4), 1)
  expect_error(generateAifMaskImage(p4, voi, index = c(NA, NA)), "index must not contain NA")
  expect_error(compcor(p4, ncompcor = 2, mask = voi, highvarmatinds = c(NA, NA)),
    "highvarmatinds must not contain NA")
})

test_that("plot.antsImage checks domainImageMap before resampling overlays", {
  local_mocked_bindings(.package = "ANTsR", resampleImageToTarget = reached)
  x <- makeImage(c(8, 8), rnorm(64))
  y <- makeImage(c(4, 4), 1, spacing = c(2, 2))   # different space: would be resampled
  expect_error(plot(x, y, domainImageMap = list(x)), "Should be length 2")
  expect_error(plot(x, y, domainImageMap = list("a", "b")), "first entry list should be antsImage")
})

test_that("antsAverageImages checks weights before reading images", {
  local_mocked_bindings(.package = "ANTsR", antsImageRead = reached)
  fns <- c(tempfile(fileext = ".nii.gz"), tempfile(fileext = ".nii.gz"))
  on.exit(unlink(fns), add = TRUE)
  for (f in fns) antsImageWrite(makeImage(c(4, 4), 1), f)
  expect_error(antsAverageImages(fns, weights = 1, verbose = FALSE), "length\\( weights \\)")
})
