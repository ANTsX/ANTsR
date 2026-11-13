# Coverage tests for assorted fMRI helpers: matrices <-> time series, HRF,
# compcor, points images, perfusion SVD, spatial bayesian lm, ICA, vwnrfs.

test_that("timeseries2matrix and matrix2timeseries round trip", {
  set.seed(1)
  img <- makeImage(c(5, 5, 4, 6), rnorm(5 * 5 * 4 * 6))
  mask <- makeImage(c(5, 5, 4), 0)
  mask[2:4, 2:4, 2:3] <- 1
  mat <- timeseries2matrix(img, mask)
  expect_equal(dim(mat), c(6, 18))
  back <- matrix2timeseries(img, mask, mat)
  expect_equal(dim(back), dim(img))
  expect_equal(timeseries2matrix(back, mask), mat, tolerance = 1e-5)
  # outside the mask the image is zero
  expect_equal(sum(abs(as.array(back)[1, 1, 1, ])), 0)
  # array mask also accepted
  mat2 <- timeseries2matrix(img, as.array(mask))
  expect_equal(dim(mat2), dim(mat))
})

test_that("timeserieswindow2matrix extracts event windows", {
  set.seed(2)
  mask <- makeImage(c(4, 4, 3), 0)
  mask[2:3, 2:3, 2] <- 1
  nv <- sum(mask)
  tsmat <- matrix(rnorm(30 * nv), 30, nv)
  res <- timeserieswindow2matrix(tsmat, mask, c(2, 10, 20), timewindow = 5)
  expect_equal(dim(res$eventmatrix), c(3, nv * 5))
  expect_equal(dim(res$mask4d), c(4, 4, 3, 5))
  expect_equal(res$eventmatrix[1, 1:nv], tsmat[2, ])
  res2 <- timeserieswindow2matrix(tsmat, mask, c(1, 28),
    timewindow = 5, zeropadvalue = 1
  )
  expect_equal(dim(res2$mask4d), c(4, 4, 3, 7))
  expect_equal(sum(res2$eventmatrix[2, ]), 0) # window runs past the end
  # a length-4 spacing hits `if (!is.na(spacing))` (package bug)
  expect_error(timeserieswindow2matrix(tsmat, mask, 2,
    timewindow = 5, spacing = c(1, 1, 1, 2)
  ), "length > 1")
  capture.output(expect_true(is.na(timeserieswindow2matrix(1:5, mask, 1, 2))))
  capture.output(expect_true(is.na(timeserieswindow2matrix(tsmat, as.antsImage(matrix(1, 2, 2)), 1, 2))))
  capture.output(r3 <- timeserieswindow2matrix(tsmat, mask, c(-1, 2), timewindow = 5))
  expect_true(is.antsImage(r3))
  capture.output(r4 <- timeserieswindow2matrix(tsmat, mask, c(2, 40), timewindow = 5))
  expect_true(is.antsImage(r4))
  capture.output(r5 <- timeserieswindow2matrix(tsmat[, 1:2], mask, 2, timewindow = 5))
  expect_true(is.antsImage(r5))
})

test_that("hemodynamicRF builds normalized regressors", {
  h <- hemodynamicRF(scans = 40, onsets = c(5, 20), durations = 4, rt = 2)
  expect_equal(dim(h), c(40, 1))
  expect_equal(sum(h), 1, tolerance = 1e-8)
  expect_true(min(h) >= 0)
  h2 <- hemodynamicRF(scans = 20, times = c(4, 20), durations = 2, rt = 2, mean = FALSE)
  expect_equal(nrow(h2), 20)
  expect_error(
    hemodynamicRF(scans = 40, onsets = c(5, 20), durations = c(1, 2, 3)),
    "does not match"
  )
})

test_that("compcor returns requested components for matrices and images", {
  set.seed(3)
  img <- makeImage(c(6, 6, 4, 20), rnorm(6 * 6 * 4 * 20) + 100)
  mask <- getAverageOfTimeSeries(img) * 0 + 1
  capture.output(expect_null(compcor(img)))
  capture.output(expect_equal(compcor(), 1))
  cc <- compcor(img, ncompcor = 3, mask = mask)
  expect_equal(dim(cc), c(20, 3))
  expect_equal(colnames(cc), paste0("compcorr", 1:3))
  mat <- timeseries2matrix(img, mask)
  v <- compcor(mat, ncompcor = 2, returnv = TRUE)
  expect_equal(colnames(v), c("compcorr1", "compcorr2"))
  inds <- compcor(mat, returnhighvarmatinds = TRUE)
  expect_true(all(inds <= ncol(mat)))
  cc2 <- compcor(mat, ncompcor = 2, highvarmatinds = inds[1], scale = FALSE)
  expect_equal(nrow(cc2), 20)
  # passing several indices hits `if (is.na(highvarmatinds))` (package bug)
  expect_error(compcor(mat, ncompcor = 2, highvarmatinds = inds), "length > 1")
})

test_that("makePointsImage and makePowersPointsImage label points", {
  mask2 <- makeImage(c(20, 20), 1)
  pts2 <- matrix(c(5, 5, 14, 14), ncol = 2, byrow = TRUE)
  p2 <- makePointsImage(pts2, mask2, radius = 2)
  expect_equal(sort(unique(as.numeric(p2[p2 > 0]))), c(1, 2))
  mask3 <- makeImage(c(12, 12, 12), 1)
  pts3 <- data.frame(x = c(3, 8), y = c(3, 8), z = c(3, 8))
  p3 <- makePointsImage(pts3, mask3, radius = 1)
  expect_equal(sort(unique(as.numeric(p3[p3 > 0]))), c(1, 2))
  expect_error(makePointsImage(pts2, mask3), "dimensionality")
  pw <- data.frame(x = c(4, 8), y = c(4, 8), z = c(4, 8), ROI = c(7, 9))
  pp <- makePowersPointsImage(pw, mask3, radius = 1)
  expect_true(all(c(7, 9) %in% as.numeric(pp[pp > 0])))
})

test_that("bloodPerfusionSVD estimates CBF/CBV/MTT with an AIF", {
  set.seed(4)
  nt <- 30
  tt <- 1:nt
  d <- c(6, 6, 2)
  arr <- array(0, c(d, nt))
  conc_aif <- 20 * dnorm(tt, 12, 2.5)
  for (i in 1:d[1]) for (j in 1:d[2]) for (k in 1:d[3]) {
    if (i == 3 && j == 3 && k == 1) {
      cc <- conc_aif + 0.05 # positive baseline offset relative to frame 1
      cc[1] <- 0
    } else {
      cc <- 0.3 * conc_aif[pmax(1, tt - 2)] + 0.01 * rnorm(nt)^2
      cc[1] <- 0
    }
    arr[i, j, k, ] <- 1000 * exp(-cc)
  }
  img <- as.antsImage(arr)
  voi <- makeImage(d, 1)
  aif <- generateAifMaskImage(img, voi)
  expect_equal(sum(aif$aifMaskImage), 1)
  expect_true(nrow(aif$fittingResults) >= 1)
  aif2 <- generateAifMaskImage(img, voi, index = 5)
  expect_equal(sum(aif2$aifMaskImage), 1)
  res <- bloodPerfusionSVD(img, voi, aif$aifMaskImage)
  expect_equal(dim(res$cbfImage), d)
  expect_true(all(as.array(res$cbfImage) >= 0))
  expect_length(res$aifSignal, nt)
  res2 <- bloodPerfusionSVD(img, voi)
  expect_true(is.antsImage(res2$mttImage))
  expect_error(bloodPerfusionSVD(), "perfusion image")
  expect_error(bloodPerfusionSVD(img), "VOI mask")
  expect_error(ANTsR:::deconvolutionSVD(matrix(1:4, 2)), "vector")
  # no voxel looks like an AIF -> warning, NULL
  flat <- makeImage(c(d, nt), 1000)
  expect_warning(expect_null(generateAifMaskImage(flat, voi)), "No voxels")
})

test_that("spatialbayesianlm smooths coefficients over the mask", {
  set.seed(5)
  mask <- makeImage(c(6, 6), 0)
  mask[2:5, 2:5] <- 1
  nv <- sum(mask)
  n <- 30
  x <- rnorm(n)
  z <- rnorm(n)
  ymat <- outer(x, rep(2, nv)) + outer(z, rep(-1, nv)) +
    matrix(rnorm(n * nv, sd = 0.5), n, nv)
  mylm <- lm(ymat ~ x + z)
  out <- spatialbayesianlm(mylm, ymat, mask, smth = 1)
  expect_length(out, 3)
  expect_true(abs(mean(out[[1]][mask == 1]) - 2) < 0.5)
  out2 <- spatialbayesianlm(mylm, ymat, mask,
    regweights = matrix(1, n, nv), nhood = c(1, 1)
  )
  expect_length(out2, 3)
  out3 <- spatialbayesianlm(mylm, ymat, mask, regweights = rep(1, n))
  expect_length(out3, 3)
  out4 <- spatialbayesianlm(mylm, ymat, mask, regweights = matrix(1, 2, 2))
  expect_length(out4, 3)
  capture.output(expect_true(is.na(spatialbayesianlm(mylm, ymat[, -1], mask))))
})

test_that("antsSpatialICAfMRI and timeseriesN3 on small images", {
  skip_if_not_installed("fastICA")
  set.seed(6)
  dims <- c(6, 6, 6, 10)
  nvox <- prod(dims)
  bl <- list(
    makeImage(dims, rnorm(nvox) + 500),
    makeImage(dims, rnorm(nvox) + 500)
  )
  maskImage <- getAverageOfTimeSeries(bl[[1]]) * 0 + 1
  expect_error(antsSpatialICAfMRI(bl), "mask")
  ica <- antsSpatialICAfMRI(bl, maskImage, numberOfICAComponents = 2, verbose = TRUE)
  expect_length(ica$componentImages, 2)
  ica1 <- antsSpatialICAfMRI(bl[1], maskImage,
    numberOfICAComponents = 2,
    normalizeComponentImages = FALSE
  )
  expect_length(ica1$componentImages, 2)
  bold <- makeImage(c(6, 6, 6, 2), rnorm(6^3 * 2) + 100)
  mask <- getMask(getAverageOfTimeSeries(bold))
  n3 <- timeseriesN3(bold, mask, c(2, 1))
  expect_equal(dim(n3), dim(bold))
})

test_that("vwnrfs trains and vwnrfs.predict predicts on small 2D images", {
  skip_if_not_installed("randomForest")
  # note: these functions call gc() many times, so keep the data tiny
  set.seed(7)
  mask <- makeImage(c(10, 10), 0)
  mask[3:6, 3:6] <- 1
  mask[5, 5:6] <- 2
  scl <- 0.33
  ilist <- list()
  lablist <- list()
  masklist <- list()
  for (i in 1:3) {
    img <- antsImageClone(mask)
    imgb <- antsImageClone(mask)
    limg <- antsImageClone(mask)
    img[3:6, 3:6] <- rnorm(16) * scl + (i %% 4)
    imgb[3:6, 3:6] <- rnorm(16) * scl + (i %% 4)
    limg[3:6, 3:6] <- (i %% 4) + 1
    ilist[[i]] <- list(img, imgb)
    lablist[[i]] <- limg
    masklist[[i]] <- mask
  }
  rfm <- vwnrfs(lablist, ilist, masklist, rad = c(1, 1), ntrees = 20, asFactors = TRUE)
  expect_s3_class(rfm$rfm, "randomForest")
  expect_true(is.factor(rfm$tv))
  expect_equal(rfm$rfm$type, "classification")
  capture.output(pr <- vwnrfs.predict(rfm$rfm, ilist, mask, rad = c(1, 1), asFactors = TRUE))
  expect_length(pr$seg, 3)
  expect_length(pr$probs, 3)
  expect_true(is.antsImage(pr$seg[[1]]))
  smask <- splitMask(mask, n = 3)
  expect_equal(sum(smask > 0), sum(mask >= 0.5))
  expect_true(max(smask) >= 2)
  expect_equal(max(splitMask(mask, voxchunk = 1000)), 1)
  expect_error(splitMask(mask), "required")
  expect_error(splitMask(list(mask), n = 2), "single antsImage")
})
