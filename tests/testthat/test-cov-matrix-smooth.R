# Coverage tests for smoothing / sparse-regression helpers in multiscaleSVDxpts.R

make_lowrank <- function(n = 30, p = 20, k = 2, noise = 0.05, seed = 1) {
  set.seed(seed)
  u <- matrix(rnorm(n * k), n, k)
  v <- matrix(rnorm(p * k), p, k)
  u %*% t(v) + matrix(rnorm(n * p, sd = noise), n, p)
}

test_that("sparseDistanceMatrix covers metrics, packages and sinkhorn", {
  set.seed(2)
  x <- matrix(rnorm(15 * 12), 15, 12)
  for (km in c("euclidean", "correlation", "covariance")) {
    d <- sparseDistanceMatrix(x, k = 3, kmetric = km, ncores = 1)
    expect_s4_class(d, "Matrix")
    expect_equal(dim(d), c(12, 12))
  }
  dg <- sparseDistanceMatrix(x, k = 3, kmetric = "gaussian", sigma = 1, ncores = 1)
  expect_equal(as.numeric(Matrix::diag(dg)), rep(1, 12))
  dsk <- sparseDistanceMatrix(x, k = 3, kmetric = "correlation", sinkhorn = TRUE,
    r = -Inf, ncores = 1)
  expect_equal(dim(dsk), c(12, 12))
  dfnn <- sparseDistanceMatrix(x, k = 3, kPackage = "FNN")
  expect_equal(dim(dfnn), c(12, 12))
  expect_true(all(dfnn@x >= 0))
  expect_error(sparseDistanceMatrix(x, k = 3, kmetric = "gaussian"), "sigma")
  xna <- x
  xna[1, 1] <- NA
  expect_error(sparseDistanceMatrix(xna, k = 3), "NA")
})

test_that("sparseDistanceMatrixXY gives ncol(y) by ncol(x) sparse matrices", {
  set.seed(3)
  x <- matrix(rnorm(15 * 10), 15, 10)
  y <- matrix(rnorm(15 * 8), 15, 8)
  for (km in c("euclidean", "correlation", "covariance")) {
    # note: for correlation/covariance the default r = Inf zeroes everything
    d <- sparseDistanceMatrixXY(x, y, k = 3, kmetric = km, ncores = 1,
      r = if (km == "euclidean") Inf else -Inf)
    expect_equal(dim(d), c(8, 10))
    # each row of y has at most k neighbours
    expect_true(all(Matrix::rowSums(d != 0) <= 3))
    expect_gt(sum(d != 0), 0)
  }
  dg <- sparseDistanceMatrixXY(x, y, k = 3, kmetric = "gaussian", sigma = 2,
    kPackage = "FNN")
  expect_equal(dim(dg), c(8, 10))
  expect_true(all(dg@x > 0 & dg@x <= 1))
  # with identical inputs the nearest neighbour of each column is itself
  dd <- sparseDistanceMatrixXY(x, x, k = 2, kPackage = "FNN")
  expect_true(all(abs(Matrix::diag(dd)) < 1e-8))
  expect_error(sparseDistanceMatrixXY(x, y, k = 3, kmetric = "gaussian"), "sigma")
  yna <- y
  yna[2, 2] <- NA
  expect_error(sparseDistanceMatrixXY(x, yna, k = 3), "NA")
  xna <- x
  xna[2, 2] <- NA
  expect_error(sparseDistanceMatrixXY(xna, y, k = 3), "NA")
})

test_that("multiscaleSVD returns eigenvalue spectra across scales", {
  set.seed(4)
  # points near a 2-d plane embedded in 8 dims
  n <- 60
  base <- matrix(runif(n * 2), n, 2)
  x <- cbind(base, matrix(rnorm(n * 6, sd = 0.01), n, 6))
  r <- c(0.4, 0.6, 0.8, 1.0, 1.2, 1.5)
  pdf(NULL)
  res <- suppressWarnings(multiscaleSVD(x, r, locn = 5, nev = 7,
    verbose = TRUE, plot = 1))
  dev.off()
  expect_named(res, c("dim", "noiseCutoffs", "evalClustering", "evalsVsScale"))
  expect_equal(dim(res$evalsVsScale), c(length(r), 7))
  expect_equal(colnames(res$evalsVsScale)[1], "EV1")
  # leading eigenvalue grows with radius
  expect_gt(res$evalsVsScale[length(r), 1], res$evalsVsScale[1, 1])
  expect_length(res$dim, 2)
  res2 <- suppressWarnings(multiscaleSVD(x, c(1.5, 1.6, 1.7, 1.8), locn = 3,
    nev = 7, knn = 40))
  expect_equal(res2$dim[1], 4)
})

test_that("knnSmoothImage smooths an image within a mask", {
  # sparseDistanceMatrix reads the thread count from this variable
  old <- Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS", unset = NA)
  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "1")
  on.exit(if (is.na(old)) Sys.unsetenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS") else
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = old), add = TRUE)
  img <- makeImage(c(8, 8), 0)
  mask <- makeImage(c(8, 8), 1)
  set.seed(5)
  img[mask == 1] <- rnorm(64)
  expect_identical(knnSmoothImage(img, mask, radius = 0), img)
  sm <- knnSmoothImage(img, mask, radius = 1, spatialSigma = 2, iterations = 2)
  expect_true(is.antsImage(sm))
  expect_lt(var(sm[mask == 1]), var(img[mask == 1]))
  jm <- knnSmoothImage(img, mask, radius = 1, spatialSigma = 2, returnMatrix = TRUE)
  expect_equal(dim(jm), c(64, 64))
  # constant image: intensity range zero branch
  cimg <- makeImage(c(8, 8), 3)
  cs <- knnSmoothImage(cimg, mask, radius = 1, spatialSigma = 2)
  expect_equal(mean(cs[mask == 1]), 3, tolerance = 1e-4)
})

test_that("smoothMatrixPrediction fits a sparse basis to low-rank data", {
  set.seed(6)
  n <- 30
  p <- 15
  bdf <- data.frame(a = rnorm(n), b = rnorm(n))
  x <- cbind(bdf$a, bdf$b) %*% matrix(rnorm(2 * p), 2, p) +
    matrix(rnorm(n * p, sd = 0.1), n, p)
  sm <- diag(p)
  for (pos in c("positive", "negative", "either")) {
    res <- smoothMatrixPrediction(x, bdf, iterations = 3, gamma = 1e-3,
      sparsenessQuantile = 0.5, positivity = pos, smoothingMatrix = sm)
    expect_equal(dim(res$v), c(p, 2))
    expect_equal(colnames(res$v), c("a", "b"))
    expect_length(res$intercept, n)
    expect_true(mean(res$v == 0) >= 0.3)
  }
  # unequal group sizes -> non-constant weights (the weight column ends up
  # in the model matrix, so constant weights would yield NaN; see report)
  rm1 <- c(rep(1:5, each = 2), rep(6:9, each = 5))
  res2 <- smoothMatrixPrediction(x, bdf, iterations = 3, gamma = 1e-3,
    positivity = TRUE, smoothingMatrix = sm, repeatedMeasures = rm1,
    verbose = TRUE)
  # random intercepts are constant within subject
  expect_equal(res2$intercept[1], res2$intercept[2])
  expect_equal(res2$intercept[11], res2$intercept[15])
  res2b <- smoothMatrixPrediction(x, bdf, iterations = 2, gamma = 1e-3,
    positivity = "either", smoothingMatrix = sm, repeatedMeasures = rm1,
    rowWeights = runif(n))
  expect_equal(nrow(res2b$v), p)
  # LRR reduces the rows of u but not of x (bug: non-conformable)
  expect_error(smoothMatrixPrediction(x, bdf, iterations = 2, gamma = 1e-3,
    positivity = FALSE, smoothingMatrix = sm, LRR = 2), "non-conformable")
  expect_error(smoothMatrixPrediction(x, bdf, positivity = "bogus"), "positivity")
  expect_message(out <- smoothMatrixPrediction(x[1:5, ], bdf, positivity = "either"),
    "inconsistent")
  expect_true(is.na(out))
  expect_message(out <- smoothMatrixPrediction(x, positivity = "either"), "needs input")
})

test_that("smoothRegression and smoothMultiRegression return sparse loadings", {
  set.seed(7)
  n <- 30
  p <- 12
  x <- matrix(rnorm(n * p), n, p)
  y <- x[, 1] * 2 + x[, 2] + rnorm(n, sd = 0.1)
  res <- smoothRegression(x, y, iterations = 6, sparsenessQuantile = 0.5,
    positivity = "either", nv = 2, verbose = TRUE)
  expect_equal(dim(res$v), c(p, 2))
  expect_true(any(res$v == 0))
  proj <- x %*% res$v[, 1]
  expect_gt(abs(cor(proj, y)), 0.5)
  ep <- data.frame(age = rnorm(n), sex = rnorm(n))
  res2 <- smoothRegression(x, y, iterations = 2, positivity = TRUE,
    smoothingMatrix = diag(p), nv = 2, extraPredictors = ep)
  expect_equal(nrow(res2$v), p)
  expect_equal(nrow(res2$fullv), p + 2)
  expect_error(smoothRegression(x, y, positivity = "maybe"), "positivity")
  expect_message(o <- smoothRegression(x, positivity = "either"), "needs input")
  ym <- cbind(y, x[, 3] + rnorm(n, sd = 0.1), rnorm(n))
  out <- capture.output(res3 <- smoothMultiRegression(x, ym, iterations = 1,
    positivity = "either", nv = 2))
  expect_equal(dim(res3$vx), c(p, 2))
  expect_equal(dim(res3$vy), c(3, 2))
  expect_error(smoothMultiRegression(x, ym, positivity = "x"), "positivity")
})

test_that("jointSmoothMatrixReconstruction reconstructs paired modalities", {
  set.seed(8)
  n <- 25
  z <- matrix(rnorm(n * 2), n, 2)
  x1 <- z %*% matrix(rnorm(2 * 10), 2, 10) + matrix(rnorm(n * 10, sd = 0.1), n, 10)
  x2 <- z %*% matrix(rnorm(2 * 8), 2, 8) + matrix(rnorm(n * 8, sd = 0.1), n, 8)
  xl <- list(x1, x2)
  params <- rbind(c(1, 2, 1), c(2, 1, 1))
  res <- jointSmoothMatrixReconstruction(xl, 2, params, iterations = 2,
    subIterations = 2, gamma = 1e-4, sparsenessQuantile = 0.25,
    positivity = "either", smoothingMatrix = list(NA, NA), verbose = TRUE,
    doOrth = TRUE)
  expect_named(res, c("u", "v", "intercepts"))
  expect_length(res$v, 2)
  expect_equal(dim(res$v[[1]]), c(8, 2))
  expect_equal(dim(res$v[[2]]), c(10, 2))
  expect_equal(dim(res$u[[1]]), c(n, 2))
  # with smoothing matrices, positivity and repeated measures
  rm1 <- rep(1:5, each = 5)
  res2 <- jointSmoothMatrixReconstruction(xl, 2, params, iterations = 2,
    subIterations = 3, gamma = 1e-4, positivity = TRUE,
    smoothingMatrix = list(diag(8), diag(10)), repeatedMeasures = rm1,
    rowWeights = rep(1, n))
  expect_equal(res2$intercepts[[1]][1], res2$intercepts[[1]][2])
  # a one-directional parameter row indexes ulist[[m2]] out of range (bug)
  expect_error(jointSmoothMatrixReconstruction(xl, 2, rbind(c(1, 2, 1)),
    iterations = 1, subIterations = 1, positivity = "negative",
    smoothingMatrix = list(NA)), "subscript out of bounds")
  expect_error(jointSmoothMatrixReconstruction(xl, 2, params, positivity = "no"),
    "positivity")
})

test_that("orthogonalizeAndQSparsifyOld and rankBasedMatrixSegmentation sparsify", {
  set.seed(9)
  v <- matrix(rnorm(20 * 3), 20, 3)
  f <- ANTsR:::orthogonalizeAndQSparsifyOld
  expect_identical(f(v, sparsenessQuantile = 0), v)
  for (pos in c("positive", "negative", "either")) {
    o <- f(v, 0.5, positivity = pos)
    expect_equal(dim(o), dim(v))
    expect_true(all(colSums(o == 0) >= 9))
  }
  o2 <- f(v, 0.5, positivity = "positive", softThresholding = TRUE, unitNorm = TRUE)
  expect_equal(sqrt(colSums(o2^2)), rep(1, 3), tolerance = 1e-8)
  o3 <- f(v, 0.5, sparsenessAlg = "orthorank")
  expect_equal(dim(o3), dim(v))
  o4 <- f(v, 0.5, positivity = "either", sparsenessAlg = "basic")
  expect_equal(dim(o4), dim(v))
  # basic rank segmentation keeps the top entries per row
  m <- rbind(c(3, -1, 2, 0), c(-5, -4, 1, 0), c(0, 0, 0, 0))
  r1 <- rankBasedMatrixSegmentation(m, 0.5, basic = TRUE, positivity = "positive")
  expect_equal(r1[1, ], c(3, 0, 2, 0))
  expect_equal(r1[2, ], c(-5, -4, 0, 0))
  expect_equal(r1[3, ], rep(0, 4))
  r2 <- rankBasedMatrixSegmentation(m, 0.5, basic = TRUE, positivity = "either",
    transpose = TRUE)
  expect_equal(dim(r2), dim(m))
  r3 <- rankBasedMatrixSegmentation(m, 0.5, basic = FALSE)
  # at most one nonzero per column after indicator optimisation
  expect_true(all(colSums(r3 != 0) <= 1))
  oi <- optimize_indicator_matrix(m, verbose = TRUE, max_iter = 3)
  expect_true(all(colSums(oi != 0) <= 1))
})

test_that("smoothAppGradCCA covers svd and stochastic initializations", {
  set.seed(10)
  n <- 30
  z <- rnorm(n)
  x <- cbind(z, matrix(rnorm(n * 9), n, 9))
  y <- cbind(z + rnorm(n, sd = 0.1), matrix(rnorm(n * 7), n, 7))
  r1 <- smoothAppGradCCA(x, y, k = 2, iterations = 3, initialization = "svd",
    positivity = TRUE, verbose = TRUE)
  expect_equal(dim(r1$phix), c(10, 2))
  expect_equal(dim(r1$phiy), c(8, 2))
  r2 <- smoothAppGradCCA(x, y, k = 2, iterations = 3, stochastic = 20,
    positivity = FALSE)
  expect_equal(dim(r2$phiy), c(8, 2))
  expect_error(smoothAppGradCCA(x[1:5, ], y), "nrow")
  expect_error(smoothAppGradCCA(x, y, positivity = "bad"), "positivity")
})

test_that("mapAsymVar builds asymmetry columns", {
  df <- data.frame(vol_left = c(3, 5), vol_right = c(1, 6), thk_left = c(1, 2),
    other = 1:2)
  out <- mapAsymVar(df, c("vol_left", "thk_left"))
  expect_true("vol_Asym" %in% colnames(out))
  expect_equal(out$vol_Asym, c(2, 1))
})
