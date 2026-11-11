test_that("resampleImageToTarget interpolators and checks", {
  set.seed(1)
  img <- makeImage(c(20, 20), runif(400))
  tgt <- makeImage(c(10, 10), 0, spacing = c(2, 2))
  for (it in list("linear", "nearestNeighbor", 2, "bSpline", "genericLabel")) {
    r <- resampleImageToTarget(img, tgt, interpType = it)
    expect_equal(dim(r), c(10, 10))
    expect_equal(antsGetSpacing(r), c(2, 2))
  }
  invisible(capture.output(r <- resampleImageToTarget(img, tgt, verbose = TRUE)))
  expect_equal(dim(r), c(10, 10))
  # constant image stays constant
  expect_equal(unique(c(as.array(resampleImageToTarget(img * 0 + 3, tgt)))), 3)
  expect_error(resampleImageToTarget(img), "missing")
  expect_error(resampleImageToTarget(mergeChannels(list(img, img)), tgt), "channel")
  expect_error(resampleImageToTarget(makeImage(c(4, 4, 4, 3), 1), makeImage(c(4, 4, 4), 1)), "imagetype 3")
  ts <- resampleImageToTarget(makeImage(c(4, 4, 4, 3), 1), makeImage(c(4, 4, 4), 1), imagetype = 3)
  expect_equal(dim(ts), c(4, 4, 4, 3))
})

test_that("resampleImage by voxels and spacing", {
  set.seed(2)
  img <- makeImage(c(20, 20), runif(400))
  for (it in 0:4) {
    r <- resampleImage(img, c(10, 10), TRUE, it)
    expect_equal(dim(r), c(10, 10))
    expect_equal(antsGetSpacing(r), c(2, 2))
  }
  r2 <- resampleImage(img, c(2, 2), FALSE, "Linear")
  expect_equal(dim(r2), c(10, 10))
})

test_that("multi-resolution random forest features and models", {
  skip_if_not_installed("randomForest")
  set.seed(3)
  mask <- makeImage(c(30, 30), 0)
  mask[8:20, 8:20] <- 1
  mask[10:14, 15:20] <- 2
  ilist <- list()
  masklist <- list()
  yvec <- rep(0, 6)
  for (i in 1:6) {
    im <- antsImageClone(mask)
    imb <- antsImageClone(mask)
    im[3:6, 3:6] <- rnorm(16, 1) * i * 0.3
    imb[3:6, 3:6] <- rnorm(16, 1) * i * .3
    ilist[[i]] <- list(im, imb)
    yvec[i] <- i^2
    masklist[[i]] <- antsImageClone(mask)
  }
  fm <- getMultiResFeatureMatrix(ilist[[1]], masklist[[1]], rad = c(1, 1), multiResSchedule = c(2, 0))
  # 2 features x 9 neighbours x 2 scales x 2 labels
  expect_equal(dim(fm), c(10, 72))
  fm2 <- getMultiResFeatureMatrix(ilist[[1]], list(mask, thresholdImage(mask, 2, 2)),
    rad = c(1, 1), multiResSchedule = c(1, 0), nsamples = 5
  )
  expect_equal(nrow(fm2), 5)
  rfm <- multiResRandomForestRegression(yvec, ilist, masklist,
    rad = c(1, 1),
    multiResSchedule = c(2, 0), ntrees = 20
  )
  expect_s3_class(rfm, "randomForest")
  expect_length(predict(rfm, newdata = fm), 10)
  rfm2 <- multiResRandomForestRegression(factor(rep(1:2, 3)), ilist, masklist,
    rad = c(1, 1),
    multiResSchedule = c(1, 0), nsamples = 5, ntrees = 20
  )
  expect_equal(rfm2$type, "classification")
})

test_that("kellyKapowski estimates cortical thickness", {
  seg <- makeImage(c(20, 20), 0)
  seg[3:18, 3:18] <- 2
  seg[6:15, 6:15] <- 3
  gm <- thresholdImage(seg, 2, 2)
  wm <- thresholdImage(seg, 3, 3)
  kk <- kellyKapowski(s = seg, g = gm, w = wm, its = 5, r = 0.5, m = 1)
  expect_equal(dim(kk), c(20, 20))
  expect_true(all(as.array(kk)[as.array(wm) == 1] == 0))
  expect_gt(max(kk), 0)
  expect_output(expect_null(kellyKapowski(s = NULL, g = gm, w = wm)), "Input error")
})

test_that("ripmmarc patch bases and population variant", {
  set.seed(4)
  img <- resampleImage(antsImageRead(getANTsRData("r16")), c(32, 32), TRUE, 0)
  msk <- getMask(img)
  rp <- ripmmarc(img, msk, patchRadius = 1, patchSamples = 20, patchVarEx = 2)
  expect_true(all(c("canonicalFrame", "basisMat", "imagePatchMat", "evecCoeffs", "recon") %in% names(rp)))
  expect_equal(dim(rp$recon), c(32, 32))
  bimg <- ripmmarcBasisImage(rp$canonicalFrame, rp$basisMat[1, ])
  expect_equal(dim(bimg), dim(rp$canonicalFrame))
  expect_error(ripmmarcBasisImage(rp$canonicalFrame, 1:3), "Size of mask")
  rp2 <- ripmmarc(img, msk,
    patchRadius = 1, patchSamples = 20, patchVarEx = 2,
    evecBasis = rp$basisMat, canonicalFrame = rp$canonicalFrame,
    rotationInvariant = FALSE, regressProjections = FALSE, meanCenter = FALSE
  )
  expect_equal(dim(rp2$basisMat), dim(rp$basisMat))
  pop <- list(img, img * 2, img + 1)
  rpp <- ripmmarcPop(pop, list(msk, msk, msk), patchRadius = 1, patchSamples = 20, seed = 1)
  expect_named(rpp, c("canonicalFrame", "basisMat", "basisEigenvalues"))
  rpp2 <- ripmmarcPop(pop, msk, patchRadius = 1, patchSamples = 20, meanCenter = FALSE, seed = 1)
  expect_equal(ncol(rpp2$basisMat), ncol(rpp$basisMat))
})

test_that("sparseRegression returns eigenanatomy images", {
  set.seed(5)
  nsubj <- 20
  input <- t(replicate(nsubj, rnorm(125)))
  outcome <- seq(1, 5, length.out = nsubj)
  input[, 40:60] <- 30 + outcome + rnorm(21 * 20, sd = 2)
  mymask <- makeImage(c(5, 5, 5), 1)
  sr <- suppressWarnings(sparseRegression(input, data.frame(outcome = outcome), "outcome", mymask,
    sparseness = 0.1, nvecs = 2, its = 2, cthresh = 10
  ))
  expect_named(sr, c("projections", "eigenanatomyimages", "umatrix"))
  expect_equal(nrow(sr$projections), nsubj)
  expect_s4_class(sr$eigenanatomyimages[[1]], "antsImage")
  expect_error(sparseRegression(), "image matrix")
  expect_error(sparseRegression(input), "demographics")
  expect_error(sparseRegression(input, data.frame(outcome = outcome)), "outcome")
})
