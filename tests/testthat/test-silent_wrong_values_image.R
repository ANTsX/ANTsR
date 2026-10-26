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
