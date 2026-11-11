eanat_blob_data <- function() {
  set.seed(2)
  g <- expand.grid(1:10, 1:10)
  blob <- function(cx, cy) exp(-((g[, 1] - cx)^2 + (g[, 2] - cy)^2) / 6)
  V <- cbind(blob(3, 3), blob(8, 7))
  U <- matrix(rnorm(60), 30, 2)
  list(X = U %*% t(V) + matrix(rnorm(3000, sd = 0.05), 30, 100),
    mask = array(1, c(10, 10)))
}

test_that("networkEiganat accepts array and antsImage masks identically", {
  d <- eanat_blob_data()
  set.seed(5)
  ra <- networkEiganat(d$X, nvecs = 2, its = 3, mask = d$mask)
  set.seed(5)
  ri <- networkEiganat(d$X, nvecs = 2, its = 3, mask = as.antsImage(d$mask))
  expect_equal(ra$v, ri$v)
  expect_true(all(is.finite(ra$v)))
  expect_length(ra$eigenanatomyimages, 2)
  expect_s4_class(ra$eigenanatomyimages[[1]], "antsImage")
  expect_equal(dim(ra$eigenanatomyimages[[1]]), c(10, 10))
  # without a mask there are no component images
  expect_length(networkEiganat(d$X, nvecs = 2, its = 2)$eigenanatomyimages, 0)
})

test_that("networkEiganat rejects invalid masks", {
  d <- eanat_blob_data()
  expect_error(networkEiganat(d$X, nvecs = 2, its = 1, mask = "mask"),
    "NA, an antsImage or an array")
  expect_error(networkEiganat(d$X, nvecs = 2, its = 1, mask = array(1, c(5, 5))),
    "25 voxels > 0.5 but Xin has 100 columns")
})

test_that(".eanatsparsifyv uses clustval and never returns an all-zero component", {
  mask <- as.antsImage(array(1, c(10, 10)))
  v <- array(0, c(10, 10))
  v[4:6, 4:6] <- 1                       # one 9-voxel component
  v <- matrix(c(v) + seq(0, 0.01, length.out = 100), ncol = 1)
  plain <- ANTsR:::.eanatsparsifyv(v, -0.1, NA)
  # the default 1000-voxel minimum removes everything on a 100-voxel mask:
  # keep the plain L0-sparse component instead of zeros
  expect_equal(ANTsR:::.eanatsparsifyv(v, -0.1, mask), plain)
  expect_equal(ANTsR:::.eanatsparsifyv(v, -0.1, mask, clustval = 0), plain)
  # a small clustval keeps the smoothed cluster
  small <- ANTsR:::.eanatsparsifyv(v, -0.1, mask, clustval = 3)
  expect_true(any(small != 0))
  expect_false(isTRUE(all.equal(small, plain)))
})
