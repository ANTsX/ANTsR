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
  # a constant voxel has rank 0 after centering: no spurious component
  # (previously c(1, 0, ..., 0), a first-frame indicator)
  expect_warning(none <- compcor(matrix(3, 20, 1), 1), "Only 0 CompCor")
  expect_equal(dim(none), c(20L, 0L))
  # duplicated voxels only add one component
  dup <- matrix(rnorm(20), 20, 3)
  expect_warning(d1 <- compcor(dup, 3, variance_extreme = 1), "Only 1 CompCor")
  expect_equal(ncol(d1), 1L)
  expect_error(compcor(matrix(numeric(0), 20, 0), 2), "at least one voxel")
  # never more components than selected high-variance voxels: extra
  # columns of svd()$u would be arbitrary null-space vectors
  nhv <- length(compcor(mat, 3, returnhighvarmatinds = TRUE))
  k <- suppressWarnings(compcor(mat, 3))
  expect_equal(ncol(k), min(3L, nhv))
  big <- matrix(rnorm(50 * 2000), 50)
  expect_equal(dim(compcor(big, 4)), c(50L, 4L))
})

test_that("aslDenoiseR uses the CompCor components that are available", {
  set.seed(2)
  n <- 20
  mat <- matrix(rnorm(n * 30), n)
  target <- rep(c(0, 1), n / 2)
  # compcor() returning fewer columns than max(maxnoisepreds) (rank-limited
  # noise pool) used to fail with "subscript out of bounds"
  local_mocked_bindings(
    compcor = function(fmri, ncompcor, ...) {
      out <- matrix(rnorm(nrow(fmri)), ncol = 1)
      colnames(out) <- "compcorr1"
      out
    },
    .package = "ANTsR"
  )
  res <- suppressWarnings(aslDenoiseR(mat, target,
    maxnoisepreds = 1:3, polydegree = 1, crossvalidationgroups = 2,
    selectionthresh = 0.99, usecompcor = TRUE
  ))
  expect_true(is.list(res))
  expect_equal(NCOL(res$noiseu), 1)
})
