test_that("whiten returns matrices of the expected shape", {
  set.seed(1)
  mat <- matrix(rnorm(300), ncol = 50)
  w <- whiten(mat)
  expect_equal(dim(w), dim(mat))
  w2 <- whiten(mat, 2, TRUE)
  expect_equal(dim(w2), c(2, 50))
  expect_true(all(is.finite(w2)))
})

test_that("weingartenImageCurvature options", {
  set.seed(2)
  img <- makeImage(c(10, 10, 10), 0)
  img[3:8, 3:8, 3:8] <- 1
  img <- smoothImage(img, 1)
  for (o in c("mean", "gaussian", "characterize")) {
    k <- weingartenImageCurvature(img, opt = o)
    expect_s4_class(k, "antsImage")
    expect_equal(dim(k), c(10, 10, 10))
  }
  kl <- weingartenImageCurvature(img, labeled = TRUE)
  expect_equal(dim(kl), dim(img))
  k2 <- weingartenImageCurvature(makeImage(c(10, 10), 1))
  expect_s4_class(k2, "antsImage")
})

test_that("subgradientL1Regression recovers a sparse predictor", {
  set.seed(3)
  x <- replicate(20, rnorm(40))
  y <- x[, 1] * 2 + rnorm(40, sd = 0.1)
  s <- subgradientL1Regression(y, x, percentvals = 0.1, nits = 5)
  expect_named(s, c("betas", "deltmag", "resultcorr"))
  expect_length(s$betas, 20)
  expect_length(s$deltmag, 5)
  expect_true(all(is.finite(s$betas)))
  expect_true(s$resultcorr > -1 && s$resultcorr <= 1)
})

test_that("split and merge N-D images", {
  set.seed(4)
  m4 <- makeImage(c(4, 4, 4, 3), rnorm(192))
  l <- splitNDImageToList(m4)
  expect_length(l, 3)
  expect_equal(dim(l[[1]]), c(4, 4, 4))
  expect_equal(as.array(l[[2]]), as.array(m4)[, , , 2])
  mm <- mergeListToNDImage(m4, l)
  expect_equal(as.array(mm), as.array(m4))
  m3 <- makeImage(c(5, 5, 3), rnorm(75))
  l3 <- splitNDImageToList(m3)
  expect_length(l3, 3)
  expect_equal(dim(l3[[1]]), c(5, 5))
  mm3 <- mergeListToNDImage(m3, l3)
  expect_equal(as.array(mm3), as.array(m3), tolerance = 1e-6)
})

test_that("splitData partitions rows", {
  set.seed(5)
  d <- data.frame(value = 1:30)
  o <- splitData(d, 2 / 3)
  expect_length(o$data.in, 20)
  expect_length(o$data.out, 10)
  expect_equal(sort(c(o$data.in, o$data.out)), 1:30)
  o2 <- splitData(d, 2 / 3, return.rows = TRUE)
  expect_equal(sort(c(o2$rows.in, o2$rows.out)), 1:30)
  o3 <- splitData(d, 3)
  expect_named(o3, c("fold1", "fold2", "fold3"))
})

test_that("simulateBiasField in 2D and 3D", {
  set.seed(6)
  bf <- simulateBiasField(makeImage(c(20, 20), 1),
    numberOfPoints = 5,
    numberOfFittingLevels = 2, meshSize = 4
  )
  expect_equal(dim(bf), c(20, 20))
  expect_gt(sd(as.array(bf)), 0)
  bf3 <- simulateBiasField(makeImage(c(8, 8, 8), 1),
    numberOfPoints = 5,
    numberOfFittingLevels = 2, meshSize = 2, sdBiasField = 2
  )
  expect_equal(dim(bf3), c(8, 8, 8))
})

test_that("renormalizeProbabilityImages sums to one", {
  set.seed(7)
  mat <- t(matrix(runif(400 * 3), ncol = 3))
  mask <- makeImage(c(20, 20), 1)
  plist <- matrixToImages(mat, mask)
  invisible(capture.output(rp <- renormalizeProbabilityImages(plist, mask)))
  s <- as.array(rp[[1]] + rp[[2]] + rp[[3]])
  expect_equal(mean(s), 1, tolerance = 1e-5)
  invisible(capture.output(rp2 <- renormalizeProbabilityImages(plist, mask, 2)))
  expect_length(rp2, 3)
})

test_that("regressionMatchImage matches a linear intensity transform", {
  set.seed(8)
  src <- makeImage(c(20, 20), runif(400))
  ref <- src * 3 + 2
  rm <- regressionMatchImage(src, ref)
  expect_equal(as.array(rm), as.array(ref), tolerance = 1e-4)
  rm2 <- regressionMatchImage(src, ref, mask = makeImage(c(20, 20), 1), polyOrder = 2, truncate = FALSE)
  expect_equal(as.array(rm2), as.array(ref), tolerance = 1e-3)
  expect_error(regressionMatchImage(makeImage(c(10, 10), 1), ref))
})

test_that("quantifySNPs and .binarizeSNPs", {
  mat <- matrix(c(0, 1, 2, 0, 0, 1, 2, 2, 2), ncol = 3)
  invisible(capture.output(q <- quantifySNPs(mat, freqthresh = 0)))
  expect_equal(dim(q), c(3, 3))
  expect_equal(q[, 3], c(1, 1, 1))
  invisible(capture.output(q2 <- quantifySNPs(mat, freqthresh = 0, shiftit = TRUE, replaceWithF = FALSE)))
  expect_equal(nrow(q2), 3)
  b <- ANTsR:::.binarizeSNPs(mat)
  expect_equal(dim(b), c(3, 6))
  expect_equal(b[, 1], as.numeric(mat[, 1] != 2))
  expect_equal(b[, 2], as.numeric(mat[, 1] != 1))
  expect_output(expect_equal(ANTsR:::.binarizeSNPs(), 1), "Usage")
  # package bug: data.frame input fails in the column-interleaving assignment
  expect_error(ANTsR:::.binarizeSNPs(data.frame(a = c(0, 1, 2), b = c(2, 2, 1))), "subscripts")
})

test_that("partialVolumeCorrection recovers grey matter activity", {
  pg <- matrix(seq(0.1, 1, by = 0.1), nrow = 2)
  pw <- 1 - pg
  pv <- partialVolumeCorrection(pg * 10 + pw * 4, pg, pw)
  expect_equal(pv, matrix(10, 2, 5), tolerance = 1e-6)
  pvi <- partialVolumeCorrection(as.antsImage(pg * 10 + pw * 4), as.antsImage(pg),
    as.antsImage(pw),
    mask = as.antsImage(pg * 0 + 1)
  )
  expect_s4_class(pvi, "antsImage")
  expect_equal(as.array(pvi), matrix(10, 2, 5), tolerance = 1e-4)
})

test_that("one-hot encoding round trips", {
  set.seed(9)
  seg <- array(sample(0:2, 64, TRUE), c(8, 8))
  oh <- segmentationToOneHot(seg)
  expect_equal(dim(oh), c(8, 8, 3))
  expect_true(all(apply(oh, c(1, 2), sum) == 1))
  expect_equal(oh[, , 2], (seg == 1) * 1)
  oh2 <- segmentationToOneHot(seg, channelFirstOrdering = TRUE)
  expect_equal(dim(oh2), c(3, 8, 8))
  oh3 <- segmentationToOneHot(seg, segmentationLabels = c(0, 1, 2, 3))
  expect_equal(dim(oh3), c(8, 8, 4))
  expect_equal(sum(oh3[, , 4]), 0)
  dom <- as.antsImage(seg * 1.0)
  inv <- oneHotToSegmentation(oh, dom)
  expect_length(inv, 3)
  expect_equal(as.array(inv[[3]]), (seg == 2) * 1)
  inv2 <- oneHotToSegmentation(oh2, dom, channelFirstOrdering = TRUE)
  expect_equal(as.array(inv2[[1]]), (seg == 0) * 1)
  seg3 <- array(sample(0:2, 64, TRUE), c(4, 4, 4))
  expect_equal(dim(segmentationToOneHot(seg3)), c(4, 4, 4, 3))
  o3 <- segmentationToOneHot(seg3, channelFirstOrdering = TRUE)
  expect_equal(dim(o3), c(3, 4, 4, 4))
  inv3 <- oneHotToSegmentation(o3, as.antsImage(seg3 * 1.0), channelFirstOrdering = TRUE)
  expect_equal(as.array(inv3[[2]]), (seg3 == 1) * 1)
  inv4 <- oneHotToSegmentation(segmentationToOneHot(seg3), as.antsImage(seg3 * 1.0))
  expect_equal(as.array(inv4[[3]]), (seg3 == 2) * 1)
})
