test_that("round() with negative digits promotes instead of wrapping", {
  vals <- c(255, 254, 3, 0)
  img <- as.antsImage(array(vals, c(2, 2)),
    spacing = c(2, 3), origin = c(1, -1), pixeltype = "unsigned char"
  )
  res <- round(img, digits = -1)
  expect_equal(as.numeric(as.array(res)), round(vals, -1))
  expect_equal(res@pixeltype, "float")
  expect_equal(antsGetSpacing(res), c(2, 3))
  expect_equal(antsGetOrigin(res), c(1, -1))

  # values that still fit keep the input type
  small <- as.antsImage(array(c(14, 26, 3, 0), c(2, 2)),
    pixeltype = "unsigned char"
  )
  res2 <- round(small, digits = -1)
  expect_equal(res2@pixeltype, "unsigned char")
  expect_equal(as.numeric(as.array(res2)), c(10, 30, 0, 0))

  # very large unsigned int values go to double, keeping exact integers
  big <- as.antsImage(array(2^32 - 1, c(2, 2)), pixeltype = "unsigned int")
  res3 <- round(big, digits = -1)
  expect_equal(res3@pixeltype, "double")
  expect_equal(as.numeric(as.array(res3))[1], round(2^32 - 1, -1))
})

test_that("round() with digits = 0 matches base R on half-integers", {
  vals <- c(0.5, 1.5, 2.5, -0.5)
  img <- as.antsImage(array(vals, c(2, 2)))
  expect_equal(as.numeric(as.array(round(img))), round(vals))
})

test_that("image-array comparisons are exact for large integer labels", {
  img <- as.antsImage(array(16777216, c(2, 2)), pixeltype = "unsigned int")
  arr <- array(16777217, c(2, 2))
  same <- array(16777216, c(2, 2))
  expect_true(all(as.array(img == arr) == 0))
  expect_true(all(as.array(arr == img) == 0))
  expect_true(all(as.array(img != arr) == 1))
  expect_true(all(as.array(arr != img) == 1))
  expect_true(all(as.array(img < arr) == 1))
  expect_true(all(as.array(arr > img) == 1))
  expect_true(all(as.array(img <= arr) == 1))
  expect_true(all(as.array(img > arr) == 0))
  expect_true(all(as.array(img >= arr) == 0))
  expect_true(all(as.array(arr >= img) == 1))
  expect_true(all(as.array(img == same) == 1))
  # image-image comparison is unchanged
  expect_true(all(as.array(
    img == as.antsImage(arr, pixeltype = "unsigned int")
  ) == 0))
})

test_that("fractional arrays still compare correctly against integer masks", {
  mask <- as.antsImage(array(c(0, 1, 1, 0), c(2, 2)),
    pixeltype = "unsigned char"
  )
  arr <- array(c(0.5, 0.5, 1, 0), c(2, 2))
  expect_equal(as.numeric(as.array(mask == arr)), c(0, 0, 1, 1))
  expect_equal(as.numeric(as.array(mask > arr)), c(0, 1, 0, 0))
})

test_that("float images still equal their own values as arrays", {
  img <- as.antsImage(array(c(0.1, 0.2, 1 / 3, 7), c(2, 2)))
  expect_true(all(as.array(img == as.array(img)) == 1))
  expect_true(all(as.array(as.array(img) == img) == 1))
})
