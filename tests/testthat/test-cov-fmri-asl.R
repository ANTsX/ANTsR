# Coverage tests for ASL / perfusion functions using small synthetic data.

make_cov_asl <- function(d = c(10, 10, 6), nt = 40, base = 1000, perf = 30,
                         seed = 1, halfperf = FALSE) {
  set.seed(seed)
  arr <- array(0, c(d, nt))
  ctr <- (d + 1) / 2
  g <- as.matrix(expand.grid(1:d[1], 1:d[2], 1:d[3]))
  r <- sqrt(rowSums(sweep(g, 2, ctr)^2 / (d / 2.5)^2))
  inside <- array(r < 1, d)
  perfregion <- inside
  if (halfperf) perfregion[1:floor(d[1] / 2), , ] <- FALSE
  for (t in 1:nt) {
    v <- array(rnorm(prod(d), 0, 5), d)
    v[inside] <- v[inside] + base
    if (t %% 2 == 0) v[perfregion] <- v[perfregion] + perf # even = control
    v[!inside] <- v[!inside] + 10
    arr[, , , t] <- v
  }
  img <- as.antsImage(arr)
  antsSetSpacing(img, c(2, 2, 3, 3))
  mask <- as.antsImage(inside * 1)
  antsSetSpacing(mask, c(2, 2, 3))
  list(img = img, mask = mask, inside = inside)
}

test_that("aslPerfusion estimates positive perfusion with denoiser + robust", {
  a <- make_cov_asl(halfperf = TRUE)
  moco <- ANTsR:::.motion_correction(a$img, moreaccurate = 0)
  expect_equal(dim(moco$moco_params), c(40, 14))
  expect_equal(names(moco$moco_params)[1:3], c("MetricPre", "MetricPost", "MOCOparam1"))
  out <- NULL
  capture.output(suppressWarnings(
    out <- aslPerfusion(a$img,
      mask = a$mask, moco_results = moco, skip = 1,
      ncompcor = 2, dorobust = 0.9, useDenoiser = 2:3, verbose = TRUE
    )
  ))
  expect_true(is.antsImage(out$perfusion))
  expect_equal(dim(out$perfusion), dim(a$mask))
  expect_true(mean(out$perfusion[a$mask == 1]) > 0)
  expect_equal(dim(out$aslTimeSeries), c(40, sum(a$inside)))
  expect_length(out$xideal, 40)
  expect_true(all(c("denoisingParams1", "denoisingParams2") %in%
    colnames(out$nuisancevariables)))
  expect_true(mean(out$m0[a$mask == 1]) > mean(out$m1[a$mask == 1]))
  expect_length(out$regweights, 40)
})

test_that("aslPerfusion non-robust path, argument checks", {
  a <- make_cov_asl(nt = 20, seed = 2)
  capture.output(expect_null(aslPerfusion()))
  capture.output(expect_null(aslPerfusion(a$img, maskThresh = "a")))
  expect_error(aslPerfusion(list(1)), "must be a filename")
  expect_error(aslPerfusion(c("a", "b")), "only one filename")
  expect_error(aslPerfusion(a$mask), "dimension")
  # integer pixel type gets cloned to float; checkmeansignal too high -> NA
  moco <- ANTsR:::.motion_correction(a$img, moreaccurate = 0)
  capture.output(expect_warning(
    res <- aslPerfusion(antsImageClone(a$img, "double"),
      moco_results = moco,
      mask = a$mask, checkmeansignal = 1e6, dorobust = 0
    ), "likely bad"
  ))
  expect_true(is.na(res))
  out <- NULL
  capture.output(suppressWarnings(
    out <- aslPerfusion(a$img,
      moco_results = moco, dorobust = 0,
      checkmeansignal = 0, maskThresh = 0.5
    )
  ))
  expect_true(is.antsImage(out$perfusion))
  expect_true(all(out$regweights == 1))
  expect_true(is.na(out$indstozero))
})

test_that("aslPerfusion with a file name and m0 supplied, internal motion correction", {
  a <- make_cov_asl(nt = 20, seed = 3)
  f <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(f))
  antsImageWrite(a$img, f)
  m0 <- getAverageOfTimeSeries(a$img)
  # supplying an m0 image errors at `if (is.na(m0))` (package bug)
  expect_error(capture.output(
    aslPerfusion(f, mask = a$mask, m0 = m0, dorobust = 0, moreaccurate = 0)
  ), "interpretable as logical")
})

test_that("perfusionregression covers argument checks, robust and bayesian paths", {
  a <- make_cov_asl(nt = 30, seed = 4)
  mat <- timeseries2matrix(a$img, a$mask)
  xideal <- rep(c(-0.5, 0.5), 15)
  capture.output(expect_null(perfusionregression()))
  capture.output(expect_null(perfusionregression(a$mask, mat)))
  r0 <- perfusionregression(a$mask, mat, xideal, nuis = NA, dorobust = 0)
  expect_true(mean(r0$cbfi[a$mask == 1]) > 20)
  expect_equal(r0$regweights, rep(1, 30))
  set.seed(5)
  nuis <- matrix(rnorm(60), ncol = 2)
  capture.output(
    r1 <- perfusionregression(a$mask, mat, xideal,
      nuis = nuis, dorobust = 0.5,
      skip = 1, useBayesian = 1
    )
  )
  expect_true(is.antsImage(r1$cbfi))
  expect_length(r1$regweights, 30)
  expect_true(all(is.finite(r1$cbfi[a$mask == 1])))
  capture.output(
    r2 <- perfusionregression(a$mask, mat, -xideal,
      nuis = NA, dorobust = 1, skip = 5,
      selectionValsForRegweights = runif(ncol(mat))
    )
  )
  expect_true(mean(r2$cbfi[a$mask == 1]) > 0)
})

test_that("aslCensoring finds outliers with outlier, robust and scor methods", {
  a <- make_cov_asl(nt = 20, seed = 6)
  arr <- as.array(a$img)
  arr[, , , 7] <- arr[, , , 7] + 400 * a$inside # corrupt one frame
  bad <- as.antsImage(arr)
  antsCopyImageInfo(a$img, bad)
  res <- aslCensoring(bad, mask = a$mask)
  expect_true(7 %in% res$which.outliers)
  expect_equal(dim(res$asl.inlier)[4], 20 - length(res$which.outliers))
  res2 <- aslCensoring(bad, mask = a$mask, reject.pairs = TRUE)
  expect_true(all(c(7, 8) %in% res2$which.outliers))
  expect_equal(length(res2$which.outliers) %% 2, 0)
  # no mask -> computed from the data
  res3 <- aslCensoring(a$img, method = "outlier")
  expect_true(is.antsImage(res3$asl.inlier))
  # robust method with nuisance variables
  set.seed(7)
  capture.output(res4 <- aslCensoring(a$img,
    mask = a$mask, method = "robust",
    nuis = matrix(rnorm(40), ncol = 2), skip = 1, robthresh = 0.9
  ))
  expect_true(is.numeric(res4$which.outliers) || length(res4$which.outliers) == 0)
  expect_equal(dim(res4$asl.inlier)[4], 20 - length(res4$which.outliers))
  # scor method
  capture.output(res5 <- aslCensoring(a$img, mask = a$mask, method = "scor"))
  expect_true(length(res5$which.outliers) >= 1)
  expect_equal(dim(res5$asl.inlier)[4], 20 - length(res5$which.outliers))
})

test_that("aslAveraging regression, subtraction, bayesian and errors", {
  a <- make_cov_asl(nt = 20, seed = 8)
  expect_warning(avg <- aslAveraging(a$img, mask = a$mask), "tag-control")
  expect_true(mean(avg[a$mask == 1]) > 20)
  tc <- rep(c(-0.5, 0.5), 10)
  set.seed(9)
  avg2 <- aslAveraging(a$img, tc = tc, nuisance = rnorm(20))
  expect_equal(dim(avg2), dim(a$mask))
  avg3 <- aslAveraging(a$img, method = "simpleSubtraction")
  expect_true(is.antsImage(avg3))
  expect_true(mean(avg3) >= 0)
  expect_error(aslAveraging(a$img, tc = tc, method = "nonsense"), "Unrecognized")
  # bayesian: 4-class segmentation inside mask + tissue probability list
  seg <- a$mask * 0
  segarr <- as.array(seg)
  segarr[a$inside] <- rep(1:4, length.out = sum(a$inside))
  seg <- as.antsImage(segarr)
  antsCopyImageInfo(a$mask, seg)
  tissues <- lapply(1:4, function(i) {
    timg <- thresholdImage(seg, i, i) * 0.7 + 0.1
    timg * a$mask
  })
  set.seed(10)
  bavg <- aslAveraging(a$img,
    tc = tc, method = "bayesian",
    nuisance = matrix(rnorm(40), ncol = 2), segmentation = seg,
    tissuelist = tissues
  )
  expect_true(is.antsImage(bavg))
  expect_true(mean(bavg[a$mask == 1]) > 0)
})

test_that("quantifyCBF for pcasl and pasl, 3D and 4D", {
  a <- make_cov_asl(nt = 10, seed = 11)
  m0 <- getAverageOfTimeSeries(a$img)
  expect_error(quantifyCBF(m0, a$mask, list()), "sequence")
  expect_error(quantifyCBF(m0, a$mask, list(sequence = "casl")), "Only pcasl")
  expect_error(quantifyCBF(m0, a$mask, list(sequence = "pcasl")), "M0")
  perf3 <- a$mask * 30
  q1 <- quantifyCBF(perf3, a$mask, list(sequence = "pcasl", m0 = m0))
  expect_true(all(q1$meancbf[a$mask == 1] > 0))
  expect_true(all(q1$meancbf[a$mask == 0] == 0))
  q2 <- quantifyCBF(perf3, a$mask, list(
    sequence = "pcasl", m0 = m0, lambda = 0.9,
    alpha = 0.7, T1blood = 0.6, omega = 1.2, slicetime = 0.04, tau = 1.8
  ), M0val = 1000)
  expect_true(all(is.finite(as.array(q2$meancbf))))
  # 4D perfusion time series
  expect_true(all(is.na(q1$kmeancbf)))
  # 4D perfusion: getAverageOfTimeSeries() is called on a plain array
  # (package bug), so the time-series path errors
  perf4 <- a$img * 0 + 30
  expect_error(
    quantifyCBF(perf4, a$mask, list(sequence = "pcasl", m0 = m0)),
    "not antsImage"
  )
  capture.output(q4 <- quantifyCBF(perf3, a$mask, list(
    sequence = "pasl", m0 = m0,
    TI1 = 700, TI2 = 1700, lambda = 0.9, alpha = 0.95, T1blood = 1150,
    slicetime = 45, A = 1.06, T2wm = 40, T2b = 80, TE = 20, delaytime = 800
  )))
  expect_true(all(q4$meancbf[a$mask == 1] > 0))
  capture.output(expect_error(
    quantifyCBF(perf4, a$mask, list(sequence = "pasl", m0 = m0)),
    "not antsImage"
  ))
})

test_that("aslDenoiseR polynomial, compcor, loess variants", {
  set.seed(12)
  nt <- 40
  nv <- 80
  tc <- rep(c(-0.5, 0.5), nt / 2)
  mat <- matrix(rnorm(nt * nv), nt, nv)
  mat[, 1:40] <- mat[, 1:40] + outer(tc, rep(3, 40))
  d1 <- aslDenoiseR(mat, tc,
    polydegree = 3, maxnoisepreds = 1:3,
    selectionthresh = 0.3, verbose = TRUE, scalemat = TRUE
  )
  expect_true(d1$n %in% 1:3)
  expect_length(d1$R2final, nv)
  expect_length(d1$noisepool, nv)
  d2 <- aslDenoiseR(mat, tc,
    covariates = rnorm(nt), polydegree = 2,
    maxnoisepreds = 2, usecompcor = TRUE, selectionthresh = 0.3
  )
  expect_equal(d2$n, 2)
  expect_equal(nrow(d2$noiseu), nt)
  d3 <- aslDenoiseR(mat, tc,
    covariates = rnorm(nt), maxnoisepreds = 1:2,
    selectionthresh = 0.3, crossvalidationgroups = rep(1:4, each = 10)
  )
  expect_true(is.list(d3))
  capture.output(d4 <- aslDenoiseR(mat, tc, selectionthresh = 1e-6))
  expect_true(is.na(d4))
})

test_that("getASLNoisePredictors returns requested number of predictors", {
  set.seed(13)
  nt <- 40
  tc <- rep(c(-0.5, 0.5), nt / 2)
  mat <- matrix(rnorm(nt * 60), nt, 60)
  mat[, 1:30] <- mat[, 1:30] + outer(tc, rep(3, 30))
  n1 <- getASLNoisePredictors(mat, tc, noisefrac = 0.3, npreds = 3)
  expect_equal(dim(n1), c(nt, 3))
  n2 <- getASLNoisePredictors(mat, tc,
    noisefrac = 0.3, polydegree = 2, npreds = 2,
    method = "compcor", covariates = rnorm(nt)
  )
  expect_equal(dim(n2), c(nt, 2))
  expect_error(getASLNoisePredictors(mat, tc, noisefrac = 1e-6), "pvalthresh")
})

test_that("bayesianlm with and without priors, vector and matrix response", {
  set.seed(14)
  n <- 50
  X <- cbind(1, rnorm(n), rnorm(n))
  beta <- c(1, 2, -1)
  y <- as.numeric(X %*% beta + rnorm(n, sd = 0.1))
  b0 <- bayesianlm(X, y)
  expect_equal(as.numeric(b0$beta), c(2, -1), tolerance = 0.1)
  expect_length(b0$beta.pval, 2)
  b1 <- bayesianlm(X, y,
    priorMean = c(0, 0, 0), priorPrecision = diag(3) * 1e6,
    includeIntercept = TRUE, regweights = rep(1, n)
  )
  expect_true(all(abs(b1$beta) < 0.1))
  Y <- cbind(y, 2 * y)
  b2 <- bayesianlm(X, Y, priorMean = matrix(0, 3, 2), regweights = rep(1, n))
  expect_equal(dim(b2$beta), c(2, 2))
  X2 <- X[, 1:2]
  b3 <- bayesianlm(X2, y)
  expect_length(b3$beta, 1)
})

test_that("bayesianCBF fails because of undefined 'seg' (package bug)", {
  a <- make_cov_asl(nt = 10, seed = 15)
  tissues <- list(a$mask, a$mask, a$mask, a$mask)
  expect_error(bayesianCBF(a$img, a$mask, tissues), "seg")
})
