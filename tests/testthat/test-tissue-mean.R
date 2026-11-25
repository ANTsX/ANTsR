test_that(".tissueMean handles zero, one, and many voxels", {
  m <- matrix(1:12, nrow = 3)
  expect_equal(ANTsR:::.tissueMean(m, 1:2, "wm"), rowMeans(m[, 1:2]))
  expect_equal(ANTsR:::.tissueMean(m, 3, "wm"), m[, 3])
  expect_warning(out <- ANTsR:::.tissueMean(m, integer(0), "wm"), "No wm voxels")
  expect_equal(out, rep(NA_real_, 3))
})

test_that(".compcorMask falls back to un-eroded white matter and flags empty masks", {
  seg <- makeImage(c(10, 10), 0)
  # one-voxel-thick white matter line: erosion removes all of it
  seg[5, 2:9] <- 3
  m <- ANTsR:::.compcorMask(seg)
  expect_true(is.antsImage(m))
  expect_equal(sum(m), 8)
  # CSF plus a white-matter block that survives erosion
  seg2 <- makeImage(c(10, 10), 0)
  seg2[2, 2] <- 1
  seg2[4:8, 4:8] <- 3
  m2 <- ANTsR:::.compcorMask(seg2)
  expect_equal(sum(m2), 1 + sum(iMath(thresholdImage(seg2, 3, 3), "ME", 1)))
  # neither CSF nor white matter
  seg3 <- makeImage(c(10, 10), 0)
  seg3[3:6, 3:6] <- 2
  expect_null(ANTsR:::.compcorMask(seg3))
})

test_that("compcor handles one component and too few voxels", {
  set.seed(1)
  mat <- matrix(rnorm(20 * 40), 20)
  one <- compcor(mat, 1)
  expect_equal(dim(one), c(20L, 1L))
  expect_equal(colnames(one), "compcorr1")
  expect_equal(dim(compcor(mat, 1, returnv = TRUE))[2], 1L)
  expect_warning(single <- compcor(matrix(rnorm(20), 20), 2), "Only 1 CompCor")
  expect_equal(dim(single), c(20L, 1L))
  expect_error(compcor(matrix(numeric(0), 20, 0), 2), "at least one voxel")
  # never more components than selected high-variance voxels: extra
  # columns of svd()$u would be arbitrary null-space vectors
  nhv <- length(compcor(mat, 3, returnhighvarmatinds = TRUE))
  k <- suppressWarnings(compcor(mat, 3))
  expect_equal(ncol(k), min(3L, nhv))
  big <- matrix(rnorm(50 * 2000), 50)
  expect_equal(dim(compcor(big, 4)), c(50L, 4L))
})
