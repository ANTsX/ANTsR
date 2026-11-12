# Arguments whose "not supplied" default is NA used to be tested with
# if (is.na(arg)), which errors for vectors/matrices/data frames on
# R >= 4.2 (and silently used the first element before); image-typed
# arguments given as arrays were silently ignored.

test_that(".isNA1 is TRUE only for a single atomic NA", {
  expect_true(ANTsR:::.isNA1(NA))
  expect_true(ANTsR:::.isNA1(NA_real_))
  expect_false(ANTsR:::.isNA1(c(NA, NA)))
  expect_false(ANTsR:::.isNA1(1:3))
  expect_false(ANTsR:::.isNA1(matrix(NA, 2, 2)))
  expect_false(ANTsR:::.isNA1(data.frame(a = NA)))
  expect_false(ANTsR:::.isNA1(list(NA)))
  expect_false(ANTsR:::.isNA1(makeImage(c(2, 2), 0)))
  expect_false(ANTsR:::.isNA1(NULL))
})

test_that("antsrMetricCreate uses array masks instead of ignoring them", {
  fi <- antsImageRead(getANTsRData("r16"))
  mi <- antsImageRead(getANTsRData("r64"))
  mask <- getMask(fi)
  none <- antsrMetricGetValue(antsrMetricCreate(fi, mi, type = "MeanSquares"))
  img <- antsrMetricGetValue(antsrMetricCreate(fi, mi, type = "MeanSquares",
    fixed.mask = mask, moving.mask = mask))
  arr <- antsrMetricGetValue(antsrMetricCreate(fi, mi, type = "MeanSquares",
    fixed.mask = as.array(mask), moving.mask = as.array(mask)))
  expect_equal(arr, img, tolerance = 1e-6)
  # the mask must actually change the metric (here by about 2x)
  expect_gt(abs(arr - none) / abs(none), 0.1)
  expect_error(antsrMetricCreate(fi, mi, fixed.mask = list(1)), "fixed.mask must be")
  expect_error(antsrMetricCreate(fi, mi, moving.mask = list(1)), "moving.mask must be")
})

test_that("subgradientL1Regression accepts an initial betas vector", {
  set.seed(1)
  x <- matrix(rnorm(200), 20, 10)
  y <- x[, 1] * 2 + rnorm(20, sd = 0.1)
  set.seed(2)
  r0 <- subgradientL1Regression(y, x, nits = 3)
  set.seed(2)
  r1 <- subgradientL1Regression(y, x, betas = rep(0, 10), nits = 3)
  expect_equal(r1, r0)
  expect_no_error(subgradientL1Regression(y, x, betas = rep(0.5, 10), nits = 3))
})

test_that("generateAifMaskImage accepts several voxel indices", {
  set.seed(1)
  p4 <- makeImage(c(4, 4, 4, 10), runif(640))
  voi <- makeImage(c(4, 4, 4), 1)
  a <- generateAifMaskImage(p4, voi, index = c(1, 2))
  expect_equal(sum(as.array(a$aifMaskImage)), 2)
})

test_that("filterfMRIforNetworkAnalysis accepts a nuisance data frame", {
  set.seed(1)
  mat <- matrix(rnorm(40 * 64), 40, 64)
  mask <- makeImage(c(4, 4, 4), 1)
  lab <- makeImage(c(4, 4, 4), rep(1:2, 32))
  ff <- filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = data.frame(a = rnorm(40), b = rnorm(40)))
  # nuisance columns are used for the correlations, then dropped again
  expect_equal(dim(ff$corrmat), c(2, 2))
  expect_equal(dim(ff$partialcorrmat), c(2, 2))
  # without glasso the glasso matrix is the partial correlation matrix
  expect_equal(ff$glassocormat, ff$partialcorrmat)
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = data.frame(a = rnorm(10))), "10 rows but there are 40 time points")
})

test_that("filterfMRIforNetworkAnalysis keeps the glasso estimate with nuisance variables", {
  skip_if_not_installed("glasso")
  set.seed(3)
  mat <- matrix(rnorm(60 * 64), 60, 64)
  mask <- makeImage(c(4, 4, 4), 1)
  lab <- makeImage(c(4, 4, 4), rep(1:4, 16))
  nz <- data.frame(a = rnorm(60), b = rnorm(60))
  ff <- filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = nz, useglasso = 0.5)
  expect_equal(dim(ff$glassocormat), c(4, 4))
  # glasso with a large penalty zeroes partial correlations; this used to be
  # overwritten by the (dense) partial correlation matrix
  expect_false(isTRUE(all.equal(ff$glassocormat, ff$partialcorrmat)))
  expect_true(any(ff$glassocormat[upper.tri(ff$glassocormat)] == 0))
})

test_that("plotPrettyGraph accepts per-vertex sizes", {
  skip_if_not_installed("sna")
  set.seed(1)
  A <- matrix(runif(16), 4)
  A <- (A + t(A)) / 2
  diag(A) <- 0
  fn <- tempfile(fileext = ".png")
  on.exit(unlink(fn), add = TRUE)
  suppressMessages(plotPrettyGraph(A, 1:4, pngfn = fn, vertexSize = c(1, 2, 3, 4)))
  expect_true(file.exists(fn))
})

test_that("compcor accepts a vector of high-variance column indices", {
  set.seed(1)
  mat <- matrix(rnorm(40 * 64), 40, 64)
  expect_equal(dim(compcor(mat, ncompcor = 2, highvarmatinds = 1:10)), c(40, 2))
  expect_equal(compcor(mat, ncompcor = 2, highvarmatinds = 1:10, returnhighvarmatinds = TRUE), 1:10)
})

test_that("timeserieswindow2matrix accepts a 4D spacing vector", {
  set.seed(1)
  img4 <- makeImage(c(4, 4, 4, 12), rnorm(768))
  m3 <- makeImage(c(4, 4, 4), 1)
  tsm <- timeseries2matrix(img4, m3)
  expect_no_error(timeserieswindow2matrix(tsm, m3, c(3, 6), 2, spacing = c(1, 1, 1, 2)))
  expect_error(timeserieswindow2matrix(tsm, m3, c(3, 6), 2, spacing = c(1, 1)))
})

test_that("reorientImage gets past a vector axis2", {
  r16 <- antsImageRead(getANTsRData("r16"))
  # it then fails because reorientImageR never writes its transform file
  # (a separate bug); before, it failed on if (is.na(axis2)) itself
  err <- tryCatch(reorientImage(r16, c(1, 0), axis2 = c(0, 1)), error = conditionMessage)
  expect_false(grepl("length > 1", err))
})

test_that("quantifySNPs uses a trait vector and checks lengths", {
  set.seed(1)
  snps <- matrix(sample(0:2, 200, TRUE), 20, 10)
  tr <- rnorm(20)
  q <- suppressMessages(capture.output(r <- quantifySNPs(snps, freqthresh = 0, traitvecin = tr)))
  # each genotype is replaced by the mean trait of subjects with it
  j <- which(colSums(snps == 0) > 0)[1]
  expect_equal(unname(r[snps[, j] == 0, j][1]), mean(tr[snps[, j] == 0]))
  train <- rep(c(TRUE, FALSE), 10)
  q <- capture.output(r2 <- quantifySNPs(snps, freqthresh = 0, traitvecin = tr, trainvec = train))
  expect_equal(unname(r2[snps[, j] == 0, j][1]), mean(tr[snps[, j] == 0 & train]))
  expect_error(quantifySNPs(snps, traitvecin = tr[1:5]), "one value per row")
  expect_error(quantifySNPs(snps, traitvecin = tr, trainvec = train[1:5]), "one value per row")
})

test_that("getMultiResFeatureMatrix accepts an array mask and ignores a global testmat", {
  set.seed(1)
  img <- makeImage(c(8, 8), rnorm(64))
  lm <- makeImage(c(8, 8), 0)
  lm[2:7, 2:7] <- 1
  ref <- getMultiResFeatureMatrix(list(img), lm, rad = c(1, 1), nsamples = 5)
  expect_equal(getMultiResFeatureMatrix(list(img), as.array(lm), rad = c(1, 1), nsamples = 5), ref)
  expect_error(getMultiResFeatureMatrix(list(img), "mask", rad = c(1, 1)), "labelmask must be")
  # a list of masks: one antsImage or array per feature image
  expect_equal(getMultiResFeatureMatrix(list(img), list(as.array(lm)), rad = c(1, 1), nsamples = 5), ref)
  expect_error(getMultiResFeatureMatrix(list(img), list("a"), rad = c(1, 1)), "one antsImage")
  expect_error(getMultiResFeatureMatrix(list(img), list(lm, lm), rad = c(1, 1)), "one antsImage")
  # an object called testmat in the global environment used to be picked up
  assign("testmat", matrix(999, 2, 2), envir = globalenv())
  on.exit(rm("testmat", envir = globalenv()), add = TRUE)
  expect_equal(getMultiResFeatureMatrix(list(img), lm, rad = c(1, 1), nsamples = 5), ref)
})

test_that("antsSet* reject NA/NaN and non-positive spacing", {
  img <- makeImage(c(4, 4), 0)
  expect_error(antsSetSpacing(img, c(NA_real_, 1)), "finite and positive")
  expect_error(antsSetSpacing(img, c(NaN, 1)), "finite and positive")
  expect_error(antsSetSpacing(img, c(0, 1)), "finite and positive")
  expect_error(antsSetSpacing(img, c(-1, 1)), "finite and positive")
  expect_error(antsSetSpacing(img, c("1", "1")), "numeric")
  expect_error(spacing(img) <- c(NA_real_, NA_real_), "finite and positive")
  antsSetSpacing(img, c(2, 0.5))
  expect_equal(antsGetSpacing(img), c(2, 0.5))
  expect_error(antsSetOrigin(img, c(NA_real_, 0)), "origin must be finite")
  expect_error(antsSetOrigin(img, c(1, 2, 3)), "origin must be of same dimensions")
  antsSetOrigin(img, c(-3, 4))
  expect_equal(antsGetOrigin(img), c(-3, 4))
  expect_error(antsSetDirection(img, matrix(NA_real_, 2, 2)), "direction must be finite")
  expect_error(antsSetDirection(img, c(1, 0, 0, 1)), "numeric matrix")
  antsSetDirection(img, diag(2))
  expect_equal(antsGetDirection(img), diag(2))
})

test_that("vectors of NAs are rejected instead of flowing into results", {
  set.seed(1)
  x <- matrix(rnorm(200), 20, 10)
  y <- x[, 1] + rnorm(20)
  expect_error(subgradientL1Regression(y, x, betas = rep(NA_real_, 10)), "betas must not contain NA")
  expect_error(subgradientL1Regression(y, x, betas = rep(0, 3)), "one value per column")
  expect_error(subgradientL1Regression(y, x, sparval = c(NA, NA)), "single number")

  p4 <- makeImage(c(4, 4, 4, 10), runif(640))
  voi <- makeImage(c(4, 4, 4), 1)
  # used to return an empty AIF mask
  expect_error(generateAifMaskImage(p4, voi, index = c(NA, NA)), "index must not contain NA")

  mat <- matrix(rnorm(40 * 64), 40, 64)
  mask <- makeImage(c(4, 4, 4), 1)
  lab <- makeImage(c(4, 4, 4), rep(1:2, 32))
  # used to return NA correlation matrices
  expect_error(filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab,
    nuisancein = data.frame(a = c(NA, rnorm(39)))), "nuisancein must not contain NA")
  expect_error(compcor(mat, ncompcor = 2, highvarmatinds = c(NA, NA)), "highvarmatinds must not contain NA")

  img4 <- makeImage(c(4, 4, 4, 12), rnorm(768))
  m3 <- makeImage(c(4, 4, 4), 1)
  tsm <- timeseries2matrix(img4, m3)
  for (sp in list(rep(NA_real_, 4), c(NA_real_, NA_real_, 3, 5), c(1, 1, 1, 0), c(-1, 1, 1, 1))) {
    expect_error(timeserieswindow2matrix(tsm, m3, c(3, 6), 2, spacing = sp),
      "4 finite, positive numbers")
  }

  r16 <- antsImageRead(getANTsRData("r16"))
  expect_error(reorientImage(r16, c(1, 0), axis2 = c(NA_real_, NA_real_)), "axis2 must not contain NA")

  snps <- matrix(sample(0:2, 200, TRUE), 20, 10)
  # used to turn every SNP value into NA
  expect_error(quantifySNPs(snps, traitvecin = rep(NA, 20)), "traitvecin must not contain NA")
  expect_error(quantifySNPs(snps, traitvecin = rnorm(20), trainvec = rep(NA, 20)), "trainvec must not contain NA")

  fi <- antsImageRead(getANTsRData("r16"))
  mi <- antsImageRead(getANTsRData("r64"))
  expect_error(antsrMetricCreate(fi, mi, fixed.mask = array(NA_real_, dim(fi))), "fixed.mask must not contain NA")

  img <- makeImage(c(8, 8), rnorm(64))
  expect_error(getMultiResFeatureMatrix(list(img), array(NA_real_, c(8, 8)), rad = c(1, 1)), "labelmask must not contain NA")
  expect_error(getMultiResFeatureMatrix(list(img), list(array(NA_real_, c(8, 8))), rad = c(1, 1)), "labelmask must not contain NA")
})

test_that("plotPrettyGraph rejects NA vertex sizes", {
  skip_if_not_installed("sna")
  A <- matrix(runif(16), 4)
  A <- (A + t(A)) / 2
  diag(A) <- 0
  expect_error(plotPrettyGraph(A, 1:4, pngfn = tempfile(fileext = ".png"),
    vertexSize = rep(NA, 4)), "vertexSize must not contain NA")
})

test_that("antsSetDirection error message matches its check", {
  img <- makeImage(c(4, 4), 0)
  expect_error(antsSetDirection(img, array(1, c(2, 2, 2))), "numeric matrix")
  expect_error(antsSetDirection(img, matrix("a", 2, 2)), "numeric matrix")
})
