context("randomlyTransformImageData")

img <- antsImageRead(getANTsRData("r16"))

test_that("deformation transforms actually deform the image (#442)", {
  # sdAffine = 0 makes the linear part the identity, so only the random
  # deformation can change the image
  for (tt in c("deformation", "affineAndDeformation")) {
    set.seed(1)
    sim <- randomlyTransformImageData(img, list(list(img)),
      numberOfSimulations = 1, transformType = tt, sdAffine = 0,
      numberOfRandomPoints = 100)
    out <- sim$simulatedImages[[1]][[1]]
    expect_gt(max(abs(as.array(out) - as.array(img))), 1, label = tt)
  }
})

test_that("linear transform types match ANTsPy's parameterisation", {
  linear_parameters <- function(tt) {
    set.seed(2)
    sim <- randomlyTransformImageData(img, list(list(img)),
      numberOfSimulations = 1, transformType = tt, sdAffine = 0.2)
    tx <- sim$simulatedTransforms[[1]]
    # the composite holds a single affine transform; read it back as a matrix
    # one point at a time: point matrices are transposed until #446 is merged
    pts <- list(c(0, 0), c(1, 0), c(0, 1))
    mapped <- do.call(rbind, lapply(pts, function(p) applyAntsrTransformToPoint(tx, p)))
    list(A = t(mapped[2:3, ] - rbind(mapped[1, ], mapped[1, ])),
      offset = mapped[1, ])
  }
  # translation: identity matrix
  expect_equal(linear_parameters("translation")$A, diag(2), tolerance = 1e-6)
  # rotation: a proper rotation, and only the translation entries are left
  # unperturbed (the last matrix entry used to be zeroed as well)
  rot <- linear_parameters("rotation")$A
  expect_equal(crossprod(rot), diag(2), tolerance = 1e-5)
  expect_equal(det(rot), 1, tolerance = 1e-5)
  # rigid: proper rotation
  rig <- linear_parameters("rigid")$A
  expect_equal(crossprod(rig), diag(2), tolerance = 1e-5)
  expect_equal(det(rig), 1, tolerance = 1e-5)
  # scaleShear: symmetric positive definite
  ss <- linear_parameters("scaleShear")$A
  expect_equal(ss, t(ss), tolerance = 1e-5)
  expect_true(all(eigen(ss, symmetric = TRUE)$values > 0))
})
