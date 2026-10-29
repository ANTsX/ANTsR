context("stats / fMRI value fixes")

test_that("quantifyCBF handles 4D input and uses the PASL T2 correction", {
  mask <- makeImage(c(4, 4, 3), 1)
  m0 <- makeImage(c(4, 4, 3), 100)
  perf4 <- makeImage(c(4, 4, 3, 5), 1)
  pc <- quantifyCBF(perf4, mask, list(sequence = "pcasl", m0 = m0))
  expect_s4_class(pc$meancbf, "antsImage")
  expect_equal(dim(pc$meancbf), c(4, 4, 3))
  expect_equal(dim(pc$timecbf), c(4, 4, 3, 5))

  perf3 <- makeImage(c(4, 4, 3), 1)
  pa <- suppressWarnings(capture.output(
    res <- quantifyCBF(perf3, mask, list(sequence = "pasl", m0 = m0))
  ))
  # default parameters: TI = 45 * slice + 800 + 700, T1b = 1150, alpha = 0.95
  TI <- 45 * 1 + 800 + 700
  Aprim <- 1.06 * exp((1 / 40 - 1 / 80) * 20)
  expected <- 3000 * 1000 / (Aprim * 100 * exp(-TI / 1150) * 700 * 0.95)
  expect_equal(as.array(res$meancbf)[1, 1, 1], expected, tolerance = 1e-5)

  pa4 <- suppressWarnings(capture.output(
    res4 <- quantifyCBF(perf4, mask, list(sequence = "pasl", m0 = m0))
  ))
  expect_equal(as.array(res4$meancbf), as.array(res$meancbf), tolerance = 1e-5)
})

test_that("resels gives Worsley counts in 2D and 3D", {
  expect_equal(resels(makeImage(c(10, 10), 1), c(1, 1)), c(1, 18, 81, 0))
  expect_equal(
    resels(makeImage(c(10, 10, 10), 1), c(1, 1, 1)),
    c(1, 27, 243, 729)
  )
})

test_that("estSmooth runs on 2D and 3D masks", {
  set.seed(1)
  m3 <- makeImage(c(10, 10, 10), 1)
  es <- estSmooth(matrix(rnorm(8 * 1000), nrow = 8), m3, rdf = 6, verbose = FALSE)
  expect_length(es$fwhm, 3)
  expect_true(all(is.finite(es$fwhm)))
  m2 <- makeImage(c(10, 10), 1)
  es2 <- estSmooth(matrix(rnorm(8 * 100), nrow = 8), m2, rdf = 6, verbose = FALSE)
  expect_length(es2$fwhm, 2)
  expect_true(all(is.finite(es2$fwhm)))
  # determinant is non-negative (Cauchy-Schwarz), so no RPV is clipped
  expect_true(all(as.array(es2$RPVImg) >= 0))
})

make_stat_image <- function() {
  set.seed(2)
  arr <- array(rnorm(1000, sd = 0.3), c(10, 10, 10))
  arr[7:8, 7:8, 7:8] <- 5 # second cluster in voxel order, label 2
  arr[2:3, 2:3, 2:3] <- 6
  arr[2, 2, 2] <- 8
  as.antsImage(arr)
}

test_that("statFieldThresh FDR methods run and use consistent df", {
  timg <- make_stat_image()
  res <- resels(makeImage(c(10, 10, 10), 1), c(1, 1, 1))
  for (tt in c("pRFT", "cFDR", "pFDR")) {
    u <- statFieldThresh(timg, .05, 2, 1, c(1, 1, 1), res, c(1, 20), "T", tt)
    expect_true(is.finite(u))
    expect_true(u > 0)
  }
  # verbose = TRUE draws the progress bar without error
  expect_output(
    statFieldThresh(timg, .05, 2, 1, c(1, 1, 1), res, c(1, 20), "T", "cFDR",
      verbose = TRUE
    )
  )
  expect_error(
    statFieldThresh(timg, .05, 2, 1, c(1, 1, 1), res, c(1, 20), "T", "pFDR",
      pp = 1e-300
    ),
    "survive"
  )
})

test_that("euler Gaussian rho_0 is the normal tail probability", {
  expect_equal(euler(3, c(1, 50), "Z")[1], 1 - pnorm(3))
})

test_that("rftResults rows match centroids and zero clusters is handled", {
  timg <- make_stat_image()
  res <- resels(makeImage(c(10, 10, 10), 1), c(1, 1, 1))
  r <- rftResults(timg, res, c(1, 1, 1), c(1, 20), "T", threshType = 4, k = 2)
  cs <- r$ClusterStats
  ps <- r$PeakStats
  expect_equal(nrow(cs), 2)
  # cluster with peak 8 sits at indices 2:3 -> physical 1:2 -> centroid 1.5
  peak8 <- which(ps[, "MaxStat"] == 8)
  expect_equal(as.numeric(cs[peak8, c("xc", "yc", "zc")]), c(1.5, 1.5, 1.5))
  peak5 <- which(ps[, "MaxStat"] == 5)
  expect_equal(as.numeric(cs[peak5, c("xc", "yc", "zc")]), c(6.5, 6.5, 6.5))
  expect_equal(
    as.numeric(ps[, "Z"]),
    round(qnorm(pt(as.numeric(ps[, "MaxStat"]), 20)), 4),
    tolerance = 1e-3
  )
  expect_warning(
    r0 <- rftResults(timg, res, c(1, 1, 1), c(1, 20), "T", threshType = 100),
    "No clusters"
  )
  expect_equal(r0$LabeledClusters, 0)

  # 2D
  set.seed(3)
  a2 <- matrix(rnorm(400, sd = 0.3), 20)
  a2[3:5, 3:5] <- 6
  img2 <- as.antsImage(a2)
  res2 <- resels(makeImage(c(20, 20), 1), c(1, 1))
  r2 <- rftResults(img2, res2, c(1, 1), c(1, 20), "T", threshType = 4)
  expect_equal(nrow(r2$ClusterStats), 1)
})

test_that("hemodynamicRF mean argument centres the response", {
  h1 <- hemodynamicRF(60, onsets = c(1, 30), durations = 10, rt = 2, mean = TRUE)
  h0 <- hemodynamicRF(60, onsets = c(1, 30), durations = 10, rt = 2, mean = FALSE)
  expect_equal(mean(h1), 0, tolerance = 1e-12)
  expect_equal(sum(h0), 1, tolerance = 1e-12)
  expect_equal(c(h1), c(h0) - mean(h0))
})

test_that("temporalwhiten works for any AR order", {
  skip_if_not_installed("magic")
  set.seed(1)
  m <- replicate(4, as.numeric(arima.sim(list(ar = 0.5), 40)))
  w1 <- temporalwhiten(m, 1)
  expect_false(anyNA(w1))
  arv <- ar(m[, 1], FALSE, 1)$ar
  expect_equal(w1[, 1], magic::shift(m[, 1], 1) * arv[1])
  expect_false(anyNA(temporalwhiten(m, 3)))
})

test_that("aslCensoring scor flags the outlying frame", {
  set.seed(1)
  nt <- 16
  base <- array(rnorm(5^3, 50, 10), c(5, 5, 5))
  arr <- array(0, c(5, 5, 5, nt))
  for (t in 1:nt) arr[, , , t] <- base + rnorm(125, sd = 2)
  arr[, , , 7] <- base * 4
  asl <- as.antsImage(arr)
  mask <- makeImage(c(5, 5, 5), 1)
  capture.output(r <- aslCensoring(asl, mask, method = "scor"))
  expect_true(7 %in% r$which.outliers)
  expect_true(length(r$which.outliers) < nt / 2)
})

test_that("generateAifMaskImage picks highest-scale voxel when all survive", {
  set.seed(1)
  nt <- 40
  tp <- seq_len(nt)
  scales <- c(30, 60, 45)
  mat <- sapply(scales, function(s) 100 * exp(-s * dnorm(tp, 20, 3)))
  arr <- array(0, c(3, 1, 1, nt))
  for (v in 1:3) arr[v, 1, 1, ] <- mat[, v]
  perf <- as.antsImage(arr)
  voi <- makeImage(c(3, 1, 1), 1)
  r <- generateAifMaskImage(perf, voi)
  expect_equal(nrow(r$fittingResults), 3)
  expect_equal(r$fittingResults$Index[1], 2)
  expect_equal(as.numeric(as.array(r$aifMaskImage)), c(0, 1, 0))
})

test_that("spatialbayesianlm works with a single predictor", {
  set.seed(1)
  mask <- makeImage(c(5, 5, 5), 1)
  x <- rnorm(20)
  ymat <- outer(x, rep(1, 125)) + matrix(rnorm(20 * 125), 20)
  r <- spatialbayesianlm(lm(ymat ~ x), ymat, mask)
  expect_length(r, 2)
  expect_s4_class(r[[1]], "antsImage")
})

test_that("compcor accepts index vectors and ncompcor = 1", {
  set.seed(1)
  mat <- matrix(rnorm(5000), ncol = 50)
  inds <- compcor(mat, returnhighvarmatinds = TRUE)
  expect_equal(dim(compcor(mat, ncompcor = 1)), c(100, 1))
  expect_equal(dim(compcor(mat, 2, highvarmatinds = inds)), c(100, 2))
})

test_that("taskFMRI names, autocorrelation and full final model", {
  skip_if_not_installed("magic")
  set.seed(1)
  nt <- 60
  hrf <- hemodynamicRF(scans = nt, onsets = c(1, 21, 41), durations = 10, rt = 2)
  nuis <- matrix(rnorm(nt * 6), nt)
  colnames(nuis) <- c(
    "motion1", "motion2", "motion3",
    "compcorr1", "compcorr2", "compcorr3"
  )
  myvars <- list(nuisancevariables = nuis, globalsignal = rnorm(nt))
  mat <- cbind(
    as.numeric(hrf) * 50 + nuis[, 1] * 3 + rnorm(nt),
    rnorm(nt)
  )
  capture.output(r1 <- taskFMRI(mat, hrf, myvars))
  # the final model includes the nuisance terms in myformula
  df <- data.frame(hrf = as.numeric(hrf), globalsignal = myvars$globalsignal, nuis)
  full <- lm(mat[, 1] ~ hrf + globalsignal + motion1 + motion2 + motion3 +
    compcorr1 + compcorr2 + compcorr3, data = df)
  expect_equal(r1$beta[1], coefficients(summary(full))["hrf", 3])

  hrfn <- hrf
  colnames(hrfn) <- "hrf"
  capture.output(r2 <- taskFMRI(mat, hrfn, myvars))
  expect_equal(r2$beta, r1$beta)
  capture.output(r3 <- taskFMRI(mat, hrf, myvars, correctautocorr = TRUE))
  expect_length(r3$beta, 2)
  expect_true(all(is.finite(r3$beta)))
})

test_that("networkEiganat accepts an antsImage mask", {
  set.seed(1)
  mask <- makeImage(c(10, 10, 10), 1)
  mat <- matrix(rnorm(20 * 1000), 20)
  r <- networkEiganat(mat,
    sparseness = c(1, 1), nvecs = 2, its = 2,
    mask = mask, verbose = FALSE
  )
  expect_length(r$eigenanatomyimages, 2)
  expect_s4_class(r$eigenanatomyimages[[1]], "antsImage")
})

test_that("combineNuisancePredictors ordered selection stays within maxpreds", {
  set.seed(1)
  n <- 40
  noise <- matrix(rnorm(n * 6), n)
  target <- rnorm(n)
  inmat <- outer(target, rnorm(30)) +
    noise[, 1:2] %*% matrix(rnorm(60, sd = 3), 2) +
    matrix(rnorm(n * 30), n)
  r <- combineNuisancePredictors(inmat, target, noise,
    maxpreds = 3,
    ordered = TRUE, k = 4
  )
  expect_true(is.matrix(r))
  expect_false(anyNA(r))
  expect_lte(ncol(r), 3)
  expect_equal(r, noise[, seq_len(ncol(r)), drop = FALSE])
})
