context("fMRI / ASL fixes")

# small synthetic tag/control ASL series: label effect in half of the voxels
make_test_asl <- function(nt = 40, sz = 6) {
  set.seed(1)
  dims <- c(sz, sz, sz, nt)
  asl <- makeImage(dims, rnorm(prod(dims)) + 500)
  aslmean <- getAverageOfTimeSeries(asl)
  aslmask <- getMask(aslmean, 0.001, Inf)
  aslmat <- timeseries2matrix(asl, aslmask)
  sigvox <- seq(1, ncol(aslmat), by = 2)
  ctl <- seq(2, nt, by = 2)
  aslmat[ctl, sigvox] <- aslmat[ctl, sigvox] + 5
  asl <- matrix2timeseries(asl, aslmask, aslmat)
  list(asl = asl, mask = aslmask, mean = aslmean)
}

# stand-in for .motion_correction (registration is slow / flaky on tiny data)
fake_moco <- function(img, ...) {
  list(
    moco_img = img,
    moco_params = as.data.frame(
      matrix(rnorm(dim(img)[4] * 14, sd = 0.01), ncol = 14)
    ),
    moco_avg_img = getAverageOfTimeSeries(img)
  )
}

test_that("corw uses the weights", {
  set.seed(2)
  mat <- matrix(rnorm(200), nrow = 20)
  w <- runif(nrow(mat))
  cw <- corw(mat, weights = w)
  c1 <- corw(mat, weights = rep(1, nrow(mat)))
  expect_false(isTRUE(all.equal(cw, c1)))
  # unit weights reproduce the unweighted absolute correlation
  expect_equal(c1[1, 2], abs(cor(mat[, 1], mat[, 2])))
})

test_that("perfusionregression robust path handles NA selection values", {
  skip_if_not_installed("robustbase")
  set.seed(3)
  nt <- 40
  nv <- 50
  xideal <- rep(c(0, 1), nt / 2) - 0.5
  mat <- matrix(rnorm(nt * nv), nt) + outer(xideal, rep(2, nv))
  mask <- makeImage(c(5, 5, 2), 1)
  nuis <- matrix(rnorm(nt * 2), nt)
  # NA selection values used to give if (NA) errors; mask is smaller than
  # skip^2 voxels, which used to overrun the visited voxel indices
  capture.output({
    res <- perfusionregression(mask, mat, xideal, nuis,
      dorobust = 0.92, skip = 20, selectionValsForRegweights = NA
    )
  })
  expect_true(is.antsImage(res$cbfi))
  expect_length(res$regweights, nt)
  expect_true(any(res$regweights != 1))
  expect_true(abs(mean(res$cbfi[mask == 1]) - 2) < 0.5)
})

test_that("aslPerfusion default robust path and supplied M0", {
  skip_if_not_installed("robustbase")
  dat <- make_test_asl()
  moco <- fake_moco(dat$asl)
  capture.output({
    expect_warning(
      p <- aslPerfusion(dat$asl, moco_results = moco, mask = dat$mask),
      "Estimating m0"
    )
  })
  expect_true(is.antsImage(p$perfusion))
  expect_true(is.antsImage(p$m0))
  expect_true(is.antsImage(p$m1))

  m0 <- dat$mean
  capture.output({
    p2 <- aslPerfusion(dat$asl, moco_results = moco, mask = dat$mask, m0 = m0)
  })
  expect_equal(as.array(p2$m0), as.array(m0))
  expect_true(is.antsImage(p2$m1))
})

test_that("bayesianCBF runs", {
  skip_if_not_installed("robustbase")
  dat <- make_test_asl()
  seg <- dat$mean * 0
  seg[dat$mask == 1] <- sample(1:4, sum(dat$mask), replace = TRUE)
  tissues <- lapply(1:4, function(i) thresholdImage(seg, i, i))
  local_mocked_bindings(.motion_correction = fake_moco, .package = "ANTsR")
  # bayesianCBF calls aslPerfusion with verbose = 1, which plots
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  capture.output({
    res <- suppressWarnings(
      bayesianCBF(dat$asl, seg, tissues,
        useDataDrivenMask = 0, denoisingComponents = 1:2
      )
    )
  })
  expect_true(is.antsImage(res$bcbf))
  expect_equal(ncol(res$posteriorBetaSummary), sum(dat$mask))
})

test_that("filterfMRIforNetworkAnalysis band is in Hz", {
  skip_if_not_installed("mFilter")
  skip_if_not_installed("magic")
  tr <- 2
  nt <- 400
  tt <- (seq_len(nt) - 1) * tr
  # 0.015 Hz is inside [0.01, 0.1] Hz, 0.15 Hz is outside
  inband <- sin(2 * pi * 0.015 * tt)
  outband <- sin(2 * pi * 0.15 * tt)
  mat <- cbind(inband, outband, inband, outband)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  res <- filterfMRIforNetworkAnalysis(mat,
    tr = tr, freqLo = 0.01, freqHi = 0.1,
    cbfnetwork = "BOLD"
  )
  fts <- as.matrix(res$filteredTimeSeries)
  ratio <- apply(fts, 2, var) / apply(mat, 2, var)
  expect_true(all(ratio[c(1, 3)] > 0.8))
  expect_true(all(ratio[c(2, 4)] < 0.1))
})

test_that("filterfMRIforNetworkAnalysis accepts matrix nuisancein", {
  skip_if_not_installed("mFilter")
  skip_if_not_installed("magic")
  set.seed(4)
  mask <- makeImage(c(4, 4, 4), 1)
  labels <- makeImage(c(4, 4, 4), rep(1:4, each = 16))
  nt <- 100
  mat <- matrix(rnorm(nt * sum(mask)), nrow = nt)
  nuis <- matrix(rnorm(nt * 2), nrow = nt)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  res <- filterfMRIforNetworkAnalysis(mat,
    tr = 2, cbfnetwork = "BOLD",
    mask = mask, labels = labels, nuisancein = nuis
  )
  expect_equal(dim(res$corrmat), c(4, 4))
  expect_equal(dim(res$partialcorrmat), c(4, 4))
  expect_equal(dim(res$glassocormat), c(4, 4))
})

test_that("filterfMRIforNetworkAnalysis returns the glasso estimate", {
  skip_if_not_installed("mFilter")
  skip_if_not_installed("magic")
  skip_if_not_installed("glasso")
  set.seed(5)
  mask <- makeImage(c(4, 4, 4), 1)
  labels <- makeImage(c(4, 4, 4), rep(1:4, each = 16))
  nt <- 100
  mat <- matrix(rnorm(nt * sum(mask)), nrow = nt)
  nuis <- matrix(rnorm(nt * 2), nrow = nt)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  res <- filterfMRIforNetworkAnalysis(mat,
    tr = 2, cbfnetwork = "BOLD",
    mask = mask, labels = labels, nuisancein = nuis, useglasso = 0.5
  )
  expect_equal(dim(res$glassocormat), c(4, 4))
  expect_false(isTRUE(all.equal(res$glassocormat, res$partialcorrmat)))
})
