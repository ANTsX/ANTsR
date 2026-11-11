test_that("imagesToMatrix reads images under a mask and rejects mismatches", {
  d <- file.path(tempdir(), "test-imagesToMatrix")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  set.seed(1)
  f <- vapply(1:3, function(i) {
    fn <- file.path(d, paste0("img", i, ".mha"))
    antsImageWrite(makeImage(c(8, 8), rnorm(64) + i), fn)
    fn
  }, "")
  mask <- makeImage(c(8, 8), 0)
  mask[3:6, 3:6] <- 1
  mat <- imagesToMatrix(f, mask)
  expect_equal(dim(mat), c(3, 16))
  expect_equal(mat[2, ], as.numeric(antsImageRead(f[2])[mask == 1]),
    tolerance = 1e-6)

  # a differently sized image used to crash R; now it is an R error
  big <- file.path(d, "big.mha")
  antsImageWrite(makeImage(c(16, 16), 1), big)
  expect_error(imagesToMatrix(c(f, big), mask), "16x16 but the mask has 8x8")
  expect_error(imagesToMatrix(file.path(d, "nope.mha"), mask), "does not exist")
})
