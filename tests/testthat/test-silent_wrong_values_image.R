context("Functions that silently returned wrong values (image/registration)")

.small_reg_images <- function() {
  f <- resampleImage(ri(1), c(32, 32), useVoxels = TRUE, interpType = 0)
  m <- resampleImage(ri(2), c(32, 32), useVoxels = TRUE, interpType = 0)
  list(f = f, m = m)
}

test_that("antsRegistration linear transforms honor samplingPercentage", {
  imgs <- .small_reg_images()
  for (tx in c("Rigid", "Affine", "Similarity", "Translation", "DenseRigid")) {
    out <- capture.output(suppressMessages(
      antsRegistration(imgs$f, imgs$m, tx,
        samplingPercentage = 0.9,
        affIterations = c(2, 1), printArgs = TRUE
      )
    ))
    out <- paste(out, collapse = " ")
    expect_true(grepl("regular,0.9]", out, fixed = TRUE), info = tx)
    expect_false(grepl("regular,0.2]", out, fixed = TRUE), info = tx)
  }
  # DenseRigid keeps its denser default when samplingPercentage is not given
  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "DenseRigid",
      affIterations = c(2, 1), printArgs = TRUE
    )
  ))
  expect_true(grepl("regular,0.8]", paste(out, collapse = " "), fixed = TRUE))
})

test_that("antsRegistrationSyN metric/transform strings are well formed", {
  imgs <- .small_reg_images()
  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuickRepro[bo]",
      printArgs = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  expect_true(grepl("CC\\[[^],]+,[^],]+,1,32\\]", out))
  expect_false(grepl(",132]", out, fixed = TRUE))

  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]",
      printArgs = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  expect_true(grepl("--transform Affine[0.1] ", out, fixed = TRUE))
})

test_that("maskImage binarizes with a scalar level", {
  img <- as.antsImage(matrix(c(5, 7, 9, 11), 2, 2))
  seg <- as.antsImage(matrix(c(1, 1, 2, 0), 2, 2))
  out <- as.array(maskImage(img, seg, 1, binarize = TRUE))
  expect_equal(out, matrix(c(1, 1, 0, 0), 2, 2))
  out <- as.array(maskImage(img, seg, 1, binarize = FALSE))
  expect_equal(out, matrix(c(5, 7, 0, 0), 2, 2))
})

test_that("round(img, digits) honors digits; cumulative Math functions work", {
  vals <- c(1.234, 2.345, 3.456, 4.567)
  img <- as.antsImage(matrix(vals, 2, 2))
  expect_equal(c(as.array(round(img, 2))), round(vals, 2), tolerance = 1e-6)
  expect_equal(c(as.array(round(img))), round(vals))
  expect_equal(cumsum(img), cumsum(vals), tolerance = 1e-6)
  expect_equal(cumprod(img), cumprod(vals), tolerance = 1e-5)
  expect_equal(cummax(img), cummax(vals), tolerance = 1e-6)
})

test_that("image-array operations keep array values for integer images", {
  mask <- as.antsImage(matrix(c(1, 0, 1, 1), 2, 2), pixeltype = "unsigned char")
  half <- array(0.5, c(2, 2))
  expect_equal(as.array(mask * half), matrix(c(0.5, 0, 0.5, 0.5), 2, 2))
  expect_equal(as.array(half * mask), matrix(c(0.5, 0, 0.5, 0.5), 2, 2))
  expect_equal(as.array(mask + half), matrix(c(1.5, 0.5, 1.5, 1.5), 2, 2))
  # consistent with the image-image case
  expect_equal(
    as.array(mask * half),
    as.array(mask * as.antsImage(half))
  )
  expect_true(all(as.array(mask == array(1.4, c(2, 2))) == 0))
  expect_true(all(as.array(array(1.4, c(2, 2)) == mask) == 0))
  expect_equal(
    as.array(mask < array(1.4, c(2, 2))),
    matrix(1, 2, 2)
  )
  expect_equal(
    as.array(mask > array(0.5, c(2, 2))),
    matrix(c(1, 0, 1, 1), 2, 2)
  )
  expect_equal(
    as.array(mask & array(0.5, c(2, 2))),
    matrix(c(1, 0, 1, 1), 2, 2)
  )
  # double images stay double
  dimg <- as.antsImage(matrix(1, 2, 2), pixeltype = "double")
  expect_equal(pixeltype(dimg + half), "double")
})

test_that("unary minus does not wrap unsigned images", {
  mask <- as.antsImage(matrix(c(1, 0, 1, 1), 2, 2), pixeltype = "unsigned char")
  expect_equal(as.array(-mask), matrix(c(-1, 0, -1, -1), 2, 2))
  img <- as.antsImage(matrix(c(1.5, -2, 3, 4), 2, 2))
  expect_equal(as.array(-img), -matrix(c(1.5, -2, 3, 4), 2, 2))
})
