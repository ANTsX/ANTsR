# For each optional argument changed in this PR, check that every kind of
# input takes the intended branch: the NA default, a supplied value (scalar,
# vector, matrix, array, image), NA-containing vectors, and wrong types.

test_that(".isNA1 routes only a single atomic NA to the default", {
  for (v in list(NA, NA_real_, NA_integer_, NA_character_, factor(NA))) {
    expect_true(ANTsR:::.isNA1(v))
  }
  for (v in list(c(NA, NA), c(NA, 1), 0, 1:3, "a", matrix(NA, 1, 1) + 1:2,
    data.frame(a = NA), list(NA), NULL, makeImage(c(2, 2), 0))) {
    expect_false(ANTsR:::.isNA1(v))
  }
})

test_that("antsrMetricCreate mask routing", {
  fi <- antsImageRead(getANTsRData("r16"))
  mi <- antsImageRead(getANTsRData("r64"))
  mask <- getMask(fi)
  val <- function(...) antsrMetricGetValue(antsrMetricCreate(fi, mi, type = "MeanSquares", ...))
  none <- val()
  img <- val(fixed.mask = mask)
  fn <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(fn), add = TRUE)
  antsImageWrite(mask, fn)
  expect_gt(abs(img - none) / abs(none), 0.1)                # image: used
  expect_equal(val(fixed.mask = as.array(mask)), img, tolerance = 1e-6) # array: converted
  expect_equal(val(fixed.mask = fn), img, tolerance = 1e-6)             # filename: read
  expect_equal(val(moving.mask = as.array(mask)), val(moving.mask = mask), tolerance = 1e-6)
  a <- as.array(mask)
  a[1] <- NA
  expect_error(val(fixed.mask = a), "fixed.mask must not contain NA")
  expect_error(val(fixed.mask = array(NA_real_, dim(fi))), "fixed.mask must not contain NA")
  expect_error(val(fixed.mask = list(mask)), "fixed.mask must be")
  expect_error(val(fixed.mask = as.numeric(as.array(mask))), "fixed.mask must be")
})

test_that("getMultiResFeatureMatrix labelmask routing", {
  set.seed(1)
  img <- makeImage(c(8, 8), rnorm(64))
  lm <- makeImage(c(8, 8), 0)
  lm[2:7, 2:7] <- 1
  f <- function(m) getMultiResFeatureMatrix(list(img), m, rad = c(1, 1), nsamples = 5)
  ref <- f(lm)
  expect_equal(f(as.array(lm)), ref)
  expect_equal(f(list(lm)), ref)
  expect_equal(f(list(as.array(lm))), ref)
  a <- as.array(lm)
  a[1] <- NA
  expect_error(f(a), "labelmask must not contain NA")
  expect_error(f(list(a)), "labelmask must not contain NA")
  expect_error(f(list(lm, lm)), "one antsImage")
  expect_error(f(list("a")), "one antsImage")
  expect_error(f("mask"), "labelmask must be")
  expect_error(f(as.numeric(as.array(lm))), "labelmask must be")
})

test_that("getMultiResFeatureMatrix array masks take each feature image's geometry", {
  set.seed(1)
  img1 <- makeImage(c(8, 8), rnorm(64))
  img2 <- makeImage(c(8, 8), rnorm(64), spacing = c(2, 0.5), origin = c(10, -4))
  m <- array(0, c(8, 8))
  m[2:7, 2:7] <- 1
  f <- function(masks) getMultiResFeatureMatrix(list(img1, img2), masks, rad = c(1, 1), nsamples = 5)
  # record the geometry of every (image, mask) pair actually used; voxel
  # values alone can't show this, since the images have the same size
  pairs <- list()
  orig <- getNeighborhoodInMask
  local_mocked_bindings(.package = "ANTsR", getNeighborhoodInMask = function(image, mask, ...) {
    pairs[[length(pairs) + 1]] <<- list(
      image = list(antsGetSpacing(image), antsGetOrigin(image)),
      mask = list(antsGetSpacing(mask), antsGetOrigin(mask))
    )
    orig(image, mask, ...)
  })
  same_space <- function() all(vapply(pairs, function(p) isTRUE(all.equal(p$image, p$mask)), TRUE))
  # reference: masks built explicitly in each feature image's space
  ref <- f(list(as.antsImage(m, reference = img1), as.antsImage(m, reference = img2)))
  expect_true(same_space())
  # a list of arrays: the second mask used to get img1's geometry
  pairs <- list()
  expect_equal(f(list(m, m)), ref)
  expect_true(same_space())
  # one array for all features: converted against each feature image
  pairs <- list()
  expect_equal(f(m), ref)
  expect_true(same_space())
  expect_true(length(pairs) >= 2)
})

test_that("getMultiResFeatureMatrix validates x and mask sizes per feature", {
  set.seed(1)
  img <- makeImage(c(8, 8), rnorm(64))
  lm <- makeImage(c(8, 8), 0)
  lm[2:7, 2:7] <- 1
  f <- function(x, m) getMultiResFeatureMatrix(x, m, rad = c(1, 1), nsamples = 5)
  ref <- f(list(img), lm)
  expect_equal(f(img, lm), ref)                         # a single image: wrapped
  expect_error(f(list(img, "notimage"), list(lm, lm)), "list of antsImages")
  expect_error(f(list(), lm), "list of antsImages")
  # masks select voxels of their feature image: sizes must match
  expect_error(f(list(img), makeImage(c(6, 6), 1)), "has dimensions 6x6 but the feature image has 8x8")
  expect_error(f(list(img), array(1, c(6, 6))), "has dimensions 6x6")
  img2 <- makeImage(c(6, 6), rnorm(36))
  expect_error(f(list(img, img2), list(lm, lm)), "feature 2 has dimensions 8x8 but the feature image has 6x6")
  expect_error(f(list(img, img2), as.array(lm)), "feature 2")
})

test_that("multiResRandomForestRegression needs one y, x and mask per subject", {
  set.seed(1)
  subj <- function() list(makeImage(c(8, 8), rnorm(64)))
  lm <- makeImage(c(8, 8), 0)
  lm[2:7, 2:7] <- 1
  x <- list(subj(), subj(), subj())
  expect_error(multiResRandomForestRegression(1:3, x, list(lm, lm), rad = c(1, 1)),
    "one entry per subject")
  expect_error(multiResRandomForestRegression(1:2, x, list(lm, lm, lm), rad = c(1, 1)),
    "one entry per subject")
})

test_that("antsrMetricCreate array masks must be on the image grid", {
  fi <- makeImage(c(8, 8), rnorm(64))
  mi <- makeImage(c(8, 8), rnorm(64))
  expect_error(antsrMetricCreate(fi, mi, fixed.mask = array(1, c(4, 4))),
    "fixed.mask array must have the same dimensions as fixed")
  expect_error(antsrMetricCreate(fi, mi, moving.mask = array(1, c(4, 4))),
    "moving.mask array must have the same dimensions as moving")
  # an antsImage mask may be on another grid (ITK maps physical points)
  expect_no_error(antsrMetricCreate(fi, mi, fixed.mask = makeImage(c(4, 4), 1, spacing = c(2, 2))))
})

test_that("subgradientL1Regression betas/sparval routing", {
  set.seed(1)
  x <- matrix(rnorm(200), 20, 10)
  y <- x[, 1] * 2 + rnorm(20, sd = 0.1)
  run <- function(...) { set.seed(2); subgradientL1Regression(y, x, nits = 3, ...) }
  expect_equal(run(betas = NA), run(betas = rep(0, 10)))    # NA: default zeros
  expect_false(isTRUE(all.equal(run(betas = rep(1, 10))$betas, run()$betas)))  # vector: used
  expect_equal(length(run(betas = matrix(0, 10, 1))$betas), 10)
  expect_error(run(betas = rep(NA_real_, 10)), "betas must not contain NA")
  expect_error(run(betas = c(NA, rep(0, 9))), "betas must not contain NA")
  expect_error(run(betas = rep(0, 3)), "one value per column")
  expect_no_error(run(sparval = NA))                           # NA: no sparsification
  expect_no_error(run(sparval = 0.5))                          # scalar: used
  expect_error(run(sparval = c(0.1, 0.2)), "single number")
  expect_error(run(sparval = c(NA, NA)), "single number")
  expect_error(run(sparval = "a"), "single number")
})

test_that("generateAifMaskImage index routing", {
  set.seed(1)
  p4 <- makeImage(c(4, 4, 4, 10), runif(640))
  voi <- makeImage(c(4, 4, 4), 1)
  aif <- function(...) sum(as.array(generateAifMaskImage(p4, voi, ...)$aifMaskImage))
  # NA: automatic selection, which finds no AIF voxel in pure noise
  expect_warning(auto <- generateAifMaskImage(p4, voi), "No voxels survived")
  expect_null(auto)
  expect_equal(aif(index = 3), 1)           # scalar: that voxel
  expect_equal(aif(index = c(1, 2)), 2)     # vector: those voxels
  expect_error(aif(index = c(NA, NA)), "index must not contain NA")
  expect_error(aif(index = c(1, NA)), "index must not contain NA")
})

test_that("filterfMRIforNetworkAnalysis nuisancein routing", {
  set.seed(1)
  mat <- matrix(rnorm(40 * 64), 40, 64)
  mask <- makeImage(c(4, 4, 4), 1)
  lab <- makeImage(c(4, 4, 4), rep(1:2, 32))
  run <- function(nz) filterfMRIforNetworkAnalysis(mat, tr = 2, mask = mask, labels = lab, nuisancein = nz)
  nz <- data.frame(a = rnorm(40), b = rnorm(40))
  base <- run(NA)                                            # NA: no nuisance
  withdf <- run(nz)                                          # data frame: used
  expect_false(isTRUE(all.equal(withdf$partialcorrmat, base$partialcorrmat)))
  expect_equal(run(as.matrix(nz))$partialcorrmat, withdf$partialcorrmat)  # matrix == df
  expect_equal(dimnames(withdf$partialcorrmat), dimnames(base$partialcorrmat))
  expect_error(run(data.frame(a = letters[1:40 %% 26 + 1])), "nuisancein must be numeric")
  expect_equal(dim(run(nz$a)$partialcorrmat), c(2, 2))       # single vector column
  expect_error(run(data.frame(a = rep(NA_real_, 40))), "nuisancein must not contain NA")
  expect_error(run(c(NA, rnorm(39))), "nuisancein must not contain NA")
  expect_error(run(nz[1:10, ]), "10 rows but there are 40 time points")
})

test_that("compcor highvarmatinds routing", {
  set.seed(1)
  mat <- matrix(rnorm(40 * 64), 40, 64)
  auto <- compcor(mat, ncompcor = 2, returnhighvarmatinds = TRUE)   # NA: computed
  expect_true(length(auto) > 0)
  expect_equal(compcor(mat, ncompcor = 2, highvarmatinds = 5, returnhighvarmatinds = TRUE), 5)
  expect_equal(compcor(mat, ncompcor = 2, highvarmatinds = 1:10, returnhighvarmatinds = TRUE), 1:10)
  expect_error(compcor(mat, ncompcor = 2, highvarmatinds = c(NA, NA)), "highvarmatinds must not contain NA")
  expect_error(compcor(mat, ncompcor = 2, highvarmatinds = c(1, NA)), "highvarmatinds must not contain NA")
})

test_that("plotPrettyGraph vertexSize routing", {
  skip_if_not_installed("sna")
  set.seed(1)
  A <- matrix(runif(16), 4)
  A <- (A + t(A)) / 2
  diag(A) <- 0
  plot_ok <- function(vs) {
    fn <- tempfile(fileext = ".png")
    on.exit(unlink(fn))
    suppressMessages(plotPrettyGraph(A, 1:4, pngfn = fn, vertexSize = vs))
    file.exists(fn)
  }
  expect_true(plot_ok(NA))            # NA: scaled default
  expect_true(plot_ok(2))             # scalar
  expect_true(plot_ok(1:4))           # vector
  expect_error(plot_ok(rep(NA, 4)), "vertexSize must not contain NA")
  expect_error(plot_ok(c(1, NA, 1, 1)), "vertexSize must not contain NA")
})

test_that("timeserieswindow2matrix spacing routing", {
  set.seed(1)
  img4 <- makeImage(c(4, 4, 4, 12), rnorm(768))
  m3 <- makeImage(c(4, 4, 4), 1)
  tsm <- timeseries2matrix(img4, m3)
  tw <- function(sp) timeserieswindow2matrix(tsm, m3, c(3, 6), 2, spacing = sp)
  expect_no_error(tw(NA))                  # NA: unit spacing
  expect_no_error(tw(c(1, 1, 1, 2)))       # vector: used
  for (sp in list(rep(NA_real_, 4), c(NA, NA, 3, 5), c(1, 1, 1, NaN), c(1, 1, 1, 0),
    c(-1, 1, 1, 1), c(1, 1), rep("1", 4), 2)) {
    expect_error(tw(sp), "4 finite, positive numbers")
  }
})

test_that("reorientImage axis2 routing", {
  r16 <- antsImageRead(getANTsRData("r16"))
  msg <- function(...) tryCatch({ reorientImage(r16, c(1, 0), ...); "" }, error = conditionMessage)
  # NA and a vector both get past the axis2 handling (reorientImageR then
  # fails on its unwritten transform file, a separate bug)
  expect_false(grepl("axis2|length > 1", msg()))
  expect_false(grepl("axis2|length > 1", msg(axis2 = c(0, 1))))
  expect_match(msg(axis2 = c(NA_real_, NA_real_)), "axis2 must not contain NA")
  expect_match(msg(axis2 = c(0, NA)), "axis2 must not contain NA")
})

test_that("quantifySNPs traitvecin/trainvec routing", {
  set.seed(1)
  snps <- matrix(sample(0:2, 200, TRUE), 20, 10)
  tr <- rnorm(20)
  train <- rep(c(TRUE, FALSE), 10)
  q <- function(...) { capture.output(r <- quantifySNPs(snps, freqthresh = 0, ...)); r }
  j <- 1
  g0 <- snps[, j] == 0
  expect_equal(unname(q()[g0, j][1]), mean(g0))                         # NA: frequencies
  expect_equal(unname(q(traitvecin = tr)[g0, j][1]), mean(tr[g0]))      # trait means
  expect_equal(unname(q(traitvecin = tr, trainvec = train)[g0, j][1]), mean(tr[g0 & train]))
  expect_error(q(trainvec = train), "trainvec requires traitvecin")     # used to be ignored
  expect_error(q(traitvecin = rep(NA, 20)), "traitvecin must not contain NA")
  expect_error(q(traitvecin = c(NA, tr[-1])), "traitvecin must not contain NA")
  expect_error(q(traitvecin = tr, trainvec = rep(NA, 20)), "trainvec must not contain NA")
  expect_error(q(traitvecin = tr[1:5]), "one value per row")
  expect_error(q(traitvecin = tr, trainvec = train[1:5]), "one value per row")
})
