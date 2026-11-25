test_that("composeDisplacementFields gives w(x) + d(x + w(x))", {
  n <- 20
  xs <- matrix(rep(0:(n - 1), n), n, n)
  # d(x) = (0.1 x, 0) varies along x; w(x) = (2, 0) is a constant shift,
  # so the two composition orders differ by 0.2
  darr <- array(0, c(2, n, n))
  darr[1, , ] <- 0.1 * xs
  warr <- array(0, c(2, n, n))
  warr[1, , ] <- 2
  d <- as.antsImage(darr, components = TRUE)
  w <- as.antsImage(warr, components = TRUE)

  before <- list.files(tempdir())
  u <- composeDisplacementFields(d, w)
  expect_length(setdiff(list.files(tempdir()), before), 0)

  expect_true(is.antsImage(u))
  expect_equal(u@components, 2L)
  expect_equal(dim(u), dim(d))
  ux <- as.array(splitChannels(u)[[1]])
  uy <- as.array(splitChannels(u)[[2]])
  inner <- 3:(n - 4) # away from the boundary
  expect_equal(ux[inner, inner], (2 + 0.1 * (xs + 2))[inner, inner],
    tolerance = 1e-5
  )
  expect_equal(max(abs(uy)), 0, tolerance = 1e-6)
})

test_that("composing zero fields gives a zero field", {
  field <- as.antsImage(array(0, c(2, 8, 8)), components = TRUE)
  u <- composeDisplacementFields(field, field)
  expect_equal(dim(u), c(8, 8))
  expect_equal(max(abs(as.array(u))), 0)
})
