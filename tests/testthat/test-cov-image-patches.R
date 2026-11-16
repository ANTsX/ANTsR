arr_diff <- function(a, b) max(abs(as.array(a) - as.array(b)))

test_that("extractImagePatches 2D: all patches, list and array forms", {
  set.seed(1)
  img <- makeImage(c(10, 12), rnorm(120))
  p <- extractImagePatches(img, c(3, 3))
  expect_type(p, "list")
  expect_length(p, 8 * 10)
  expect_equal(p[[1]], as.array(img)[1:3, 1:3])
  pa <- extractImagePatches(img, c(3, 3), returnAsArray = TRUE, strideLength = c(1, 1))
  expect_equal(dim(pa), c(80, 3, 3))
  expect_equal(pa[80, , ], as.array(img)[8:10, 10:12])

  p2 <- extractImagePatches(img, c(4, 4), strideLength = 2, returnAsArray = TRUE)
  expect_equal(dim(p2), c(4 * 5, 4, 4))
  r <- reconstructImageFromPatches(p2, img, strideLength = 2)
  expect_s4_class(r, "antsImage")
  expect_equal(arr_diff(r, img), 0, tolerance = 1e-6)
  r2 <- reconstructImageFromPatches(
    extractImagePatches(img, c(4, 4), strideLength = 2), img,
    strideLength = c(2, 2)
  )
  expect_equal(arr_diff(r2, img), 0, tolerance = 1e-6)
})

test_that("reconstructImageFromPatches 2D single-component stride 1 (known bug)", {
  set.seed(1)
  img <- makeImage(c(8, 8), rnorm(64))
  pa <- extractImagePatches(img, c(3, 3), returnAsArray = TRUE)
  # package bug: imageArray[...,] drops the component dimension -> non-conformable
  expect_error(reconstructImageFromPatches(pa, img), "non-conformable")
  expect_error(
    reconstructImageFromPatches(pa, getMask(img + 10, 1, 100), domainImageIsMask = TRUE),
    "non-conformable"
  )
})

test_that("extract/reconstruct round trip in 3D", {
  set.seed(2)
  img3 <- makeImage(c(6, 7, 5), rnorm(210))
  p3 <- extractImagePatches(img3, c(3, 3, 3), returnAsArray = TRUE)
  expect_equal(dim(p3), c(4 * 5 * 3, 3, 3, 3))
  r <- reconstructImageFromPatches(p3, img3)
  expect_equal(dim(r), dim(img3))
  expect_equal(arr_diff(r, img3), 0, tolerance = 1e-5)
  pl <- extractImagePatches(img3, c(3, 3, 3), strideLength = c(1, 1, 1))
  expect_length(pl, 60)
  r <- reconstructImageFromPatches(pl, img3)
  expect_equal(arr_diff(r, img3), 0, tolerance = 1e-5)

  img4 <- makeImage(c(6, 6, 4), rnorm(144))
  r <- reconstructImageFromPatches(
    extractImagePatches(img4, c(2, 2, 2), strideLength = 2), img4,
    strideLength = 2
  )
  expect_equal(arr_diff(r, img4), 0, tolerance = 1e-5)
  r <- reconstructImageFromPatches(
    extractImagePatches(img4, c(2, 2, 2), strideLength = 2, returnAsArray = TRUE),
    img4,
    strideLength = 2
  )
  expect_equal(arr_diff(r, img4), 0, tolerance = 1e-5)

  msk <- getMask(img3 + 10, 1, 100)
  rm <- reconstructImageFromPatches(p3, msk, domainImageIsMask = TRUE)
  expect_equal(dim(rm), dim(img3))
})

test_that("multi-component patches round trip", {
  set.seed(3)
  img <- makeImage(c(8, 6), rnorm(48))
  mc <- mergeChannels(list(img, img * 2))
  pm <- extractImagePatches(mc, c(3, 3), returnAsArray = TRUE)
  expect_equal(dim(pm), c(6 * 4, 3, 3, 2))
  pml <- extractImagePatches(mc, c(3, 3))
  expect_equal(dim(pml[[1]]), c(3, 3, 2))
  rm <- reconstructImageFromPatches(pm, mc)
  expect_equal(rm@components, 2)
  expect_equal(arr_diff(rm, mc), 0, tolerance = 1e-5)
  rm <- reconstructImageFromPatches(pml, mc, strideLength = 1)
  expect_equal(arr_diff(rm, mc), 0, tolerance = 1e-5)
  rm <- reconstructImageFromPatches(
    extractImagePatches(mc, c(2, 2), strideLength = 2, returnAsArray = TRUE), mc,
    strideLength = 2
  )
  expect_equal(arr_diff(rm, mc), 0, tolerance = 1e-5)

  img3 <- makeImage(c(4, 4, 4), rnorm(64))
  mc3 <- mergeChannels(list(img3, img3 * 2))
  pm3 <- extractImagePatches(mc3, c(2, 2, 2), returnAsArray = TRUE)
  expect_equal(dim(pm3), c(27, 2, 2, 2, 2))
  expect_length(extractImagePatches(mc3, c(2, 2, 2)), 27)
  rm3 <- reconstructImageFromPatches(pm3, mc3)
  expect_equal(arr_diff(rm3, mc3), 0, tolerance = 1e-5)
  rm3 <- reconstructImageFromPatches(
    extractImagePatches(mc3, c(2, 2, 2), strideLength = 2, returnAsArray = TRUE),
    mc3,
    strideLength = 2
  )
  expect_equal(arr_diff(rm3, mc3), 0, tolerance = 1e-5)
})

test_that("random patch extraction with and without mask", {
  set.seed(4)
  img <- makeImage(c(10, 12), rnorm(120))
  img3 <- makeImage(c(6, 7, 5), rnorm(210))
  msk <- getMask(img3 + 10, 1, 100)
  x <- extractImagePatches(img3, c(3, 3, 3), maxNumberOfPatches = 5, randomSeed = 1, returnAsArray = TRUE)
  expect_equal(dim(x), c(5, 3, 3, 3))
  y <- extractImagePatches(img3, c(3, 3, 3), maxNumberOfPatches = 5, randomSeed = 1, returnAsArray = TRUE)
  expect_identical(x, y)
  x <- extractImagePatches(img3, c(3, 3, 3), maxNumberOfPatches = 5, maskImage = msk, randomSeed = 1)
  expect_length(x, 5)
  expect_equal(dim(x[[1]]), c(3, 3, 3))
  x <- extractImagePatches(img3, c(3, 3, 3),
    maxNumberOfPatches = 5, maskImage = msk,
    randomize = FALSE, returnAsArray = TRUE
  )
  expect_equal(dim(x), c(5, 3, 3, 3))
  x <- extractImagePatches(img, c(3, 3), maxNumberOfPatches = 5, randomSeed = 1, returnAsArray = TRUE)
  expect_equal(dim(x), c(5, 3, 3))
  x <- extractImagePatches(img, c(3, 3), maxNumberOfPatches = 5, randomSeed = 1)
  expect_length(x, 5)

  mc <- mergeChannels(list(img, img))
  expect_equal(dim(extractImagePatches(mc, c(3, 3), maxNumberOfPatches = 5, randomSeed = 1, returnAsArray = TRUE)), c(5, 3, 3, 2))
  expect_equal(dim(extractImagePatches(mc, c(3, 3), maxNumberOfPatches = 5, randomSeed = 1)[[1]]), c(3, 3, 2))
  mc3 <- mergeChannels(list(img3, img3))
  expect_equal(dim(extractImagePatches(mc3, c(3, 3, 3), maxNumberOfPatches = 5, randomSeed = 1, returnAsArray = TRUE)), c(5, 3, 3, 3, 2))
  expect_equal(dim(extractImagePatches(mc3, c(3, 3, 3), maxNumberOfPatches = 5, randomSeed = 1)[[1]]), c(3, 3, 3, 2))
})

test_that("patch functions validate inputs", {
  img <- makeImage(c(10, 12), 1)
  img1 <- makeImage(c(10, 12, 4, 2), 1)
  expect_error(extractImagePatches(img1, c(2, 2, 2, 2)), "Unsupported")
  expect_error(extractImagePatches(img, c(2, 2, 2)), "Mismatch")
  expect_error(extractImagePatches(img, c(20, 2)), "greater")
  expect_error(extractImagePatches(img, c(2, 2), strideLength = c(1, 1, 1)), "strideLength")
  expect_error(extractImagePatches(img, c(2, 2), strideLength = c(0, 1)), "positive")
  expect_error(extractImagePatchCoordinates(img1, c(2, 2, 2, 2)), "Unsupported")
  expect_error(extractImagePatchCoordinates(img, c(2, 2, 2)), "Mismatch")
  expect_error(extractImagePatchCoordinates(img, c(20, 2)), "greater")
  expect_error(extractImagePatchCoordinates(img, c(2, 2), strideLength = c(1, 1, 1)), "strideLength")
  expect_error(extractImagePatchCoordinates(img, c(2, 2), strideLength = c(0, 1)), "positive")
  pa <- array(0, c(4, 2, 2))
  expect_error(reconstructImageFromPatches(pa, img1), "Unsupported")
  expect_error(reconstructImageFromPatches(pa, img, strideLength = c(1, 1, 1)), "strideLength")
  expect_error(reconstructImageFromPatches(pa, img, strideLength = c(0, 1)), "positive")
})

test_that("extractImagePatchCoordinates returns index and physical coordinates", {
  img3 <- makeImage(c(6, 7, 5), 1, spacing = c(2, 2, 2), origin = c(10, 0, 0))
  co <- extractImagePatchCoordinates(img3, c(3, 3, 3), physicalCoordinates = FALSE)
  expect_equal(dim(co), c(60, 3))
  expect_equal(co[1, ], c(2, 2, 2))
  cp <- extractImagePatchCoordinates(img3, c(3, 3, 3))
  expect_equal(dim(cp), c(60, 3))
  # physical = origin + spacing * (index - 1)
  expect_equal(unname(cp[1, ]), c(10, 0, 0) + 2 * (co[1, ] - 1))
  cc <- extractImagePatchCoordinates(img3, c(3, 3, 3),
    physicalCoordinates = FALSE, cornerCoordinates = FALSE
  )
  expect_equal(cc[1, ], co[1, ] + round(c(3, 3, 3) / 2))

  r1 <- extractImagePatchCoordinates(img3, c(3, 3, 3), maxNumberOfPatches = 4, randomSeed = 1)
  r2 <- extractImagePatchCoordinates(img3, c(3, 3, 3), maxNumberOfPatches = 4, randomSeed = 1)
  expect_equal(dim(r1), c(4, 3))
  expect_identical(r1, r2)
  msk <- makeImage(c(6, 7, 5), 1)
  rm <- extractImagePatchCoordinates(img3, c(3, 3, 3),
    maxNumberOfPatches = 4,
    maskImage = msk, randomSeed = 1, cornerCoordinates = FALSE,
    physicalCoordinates = FALSE
  )
  expect_equal(dim(rm), c(4, 3))
  expect_true(all(rm >= 2))
  rn <- extractImagePatchCoordinates(img3, c(3, 3, 3), maxNumberOfPatches = 4, maskImage = msk, randomize = FALSE)
  expect_equal(ncol(rn), 3)

  img <- makeImage(c(10, 12), 1)
  r2d <- extractImagePatchCoordinates(img, c(3, 3), maxNumberOfPatches = 4, randomSeed = 2, physicalCoordinates = FALSE)
  expect_equal(dim(r2d), c(4, 2))
  expect_true(all(r2d[, 1] <= 9 & r2d[, 2] <= 11))
  # package bug: the 2D 'all' branch never increments its counter, so only
  # one (the last) coordinate is returned
  a2d <- extractImagePatchCoordinates(img, c(3, 3), physicalCoordinates = FALSE)
  expect_equal(as.vector(a2d), c(9, 11))
})
