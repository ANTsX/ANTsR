translation3d <- function() {
  tx <- createAntsrTransform(type = "Euler3DTransform", translation = c(1, 2, 3))
  f <- tempfile(fileext = ".mat")
  writeAntsrTransform(tx, f)
  f
}

test_that("antsApplyTransformsToPoints rejects too few coordinate columns", {
  f <- translation3d()
  pts <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_error(
    antsApplyTransformsToPoints(3, pts, f),
    "2 column\\(s\\) but a 3D transform"
  )
})

test_that("antsApplyTransformsToPoints errors instead of returning the input on native failure", {
  # a corrupt transform file crashes the native reader, so simulate the
  # native failure status (1) that it returns for bad inputs
  f <- translation3d()
  pts <- data.frame(x = c(1, 2), y = c(3, 4), z = c(5, 6))
  local_mocked_bindings(
    AntsApplyTransformsToPoints = function(...) 1L,
    .package = "ANTsRCore"
  )
  expect_error(antsApplyTransformsToPoints(3, pts, f), "failed \\(status 1\\)")
})

test_that("antsApplyTransformsToPoints still transforms valid points", {
  f <- translation3d()
  pts <- data.frame(x = c(1, 2), y = c(3, 4), z = c(5, 6), label = c(7, 8))
  out <- antsApplyTransformsToPoints(3, pts, f)
  expect_equal(out$x, pts$x + 1)
  expect_equal(out$y, pts$y + 2)
  expect_equal(out$z, pts$z + 3)
  expect_equal(out$label, pts$label)
})

test_that("antsApplyTransformsToPoints supports double precision", {
  f <- translation3d()
  pts <- data.frame(x = c(123456.789012, 2), y = c(1, 2), z = c(1, 2))
  dbl <- antsApplyTransformsToPoints(3, pts, f, precision = "double")
  expect_equal(dbl$x[1], 123457.789012, tolerance = 1e-12)
  flt <- antsApplyTransformsToPoints(3, pts, f)
  # single precision cannot hold the fractional digits
  expect_gt(abs(flt$x[1] - 123457.789012), 1e-5)
  img <- antsApplyTransformsToPoints(
    3, as.antsImage(data.matrix(pts), pixeltype = "double"), f,
    precision = "double"
  )
  expect_equal(img@pixeltype, "double")
  expect_equal(as.array(img)[1, 1], 123457.789012, tolerance = 1e-12)
})

test_that("partialVolumeCorrection never returns Inf for a zero denominator", {
  expect_warning(
    out <- partialVolumeCorrection(10, 0, 1, proportion = 0),
    "cannot be recovered"
  )
  expect_true(is.na(out))
  # ordinary voxels are unchanged by the guard
  expect_equal(partialVolumeCorrection(10, 0.5, 0.5), 10 / (0.5 + 0.4 * 0.5))
  expect_equal(
    partialVolumeCorrection(10, 0.5, 0.5, proportion = 0),
    10 / 0.5
  )
  # little tissue: returned uncorrected, as before
  expect_equal(partialVolumeCorrection(10, 0.1, 0.1), 10)
  expect_error(partialVolumeCorrection(10, 0.5, 0.5, proportion = -1), "non-negative")
})

test_that("partialVolumeCorrection guards antsImage inputs", {
  img <- as.antsImage(matrix(10, 2, 2))
  gm <- as.antsImage(matrix(c(0, 0.5, 0.5, 1), 2, 2))
  wm <- as.antsImage(matrix(c(1, 0.5, 0.5, 0), 2, 2))
  expect_warning(
    out <- partialVolumeCorrection(img, gm, wm, proportion = 0),
    "1 voxel"
  )
  vals <- as.array(out)
  expect_false(any(is.infinite(vals)))
  expect_true(is.na(vals[1, 1]))
  expect_equal(vals[2, 2], 10)
})

test_that("a double antsImage of points keeps double precision by default", {
  tx <- createAntsrTransform(dimension = 2, translation = c(10, 0))
  f <- tempfile(fileext = ".mat")
  on.exit(unlink(f))
  writeAntsrTransform(tx, f)
  pts <- as.antsImage(matrix(c(123456.789, 1), 1), pixeltype = "double")
  out <- antsApplyTransformsToPoints(2, pts, f)
  expect_equal(out@pixeltype, "double")
  expect_equal(as.array(out)[1, 1], 123456.789 + 10, tolerance = 1e-9)
  fl <- antsApplyTransformsToPoints(2, pts, f, precision = "float")
  expect_equal(fl@pixeltype, "float")
})

test_that("partialVolumeCorrection keeps missing tissue values as NA", {
  out <- partialVolumeCorrection(c(10, 10), c(NA, 0.5), c(0.5, 0.5))
  expect_true(is.na(out[1]))
  expect_equal(out[2], 10 / (0.5 + 0.4 * 0.5))
  expect_error(partialVolumeCorrection(10, 0.5, 0.5, proportion = Inf), "finite")
})
