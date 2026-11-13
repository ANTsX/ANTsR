# Coverage tests for landmark-based transform fitting.

lm_points <- function(n = 12, seed = 1) {
  set.seed(seed)
  matrix(runif(n * 2, 4, 12), ncol = 2)
}


# composeDisplacementFields() currently fails (it passes antsImage objects as
# the transform list and reads a file name antsApplyTransforms never writes),
# which breaks the "diffeo" and "syn" branches below.  Those tests first
# document the failure and then substitute a working composition so the
# fitting logic itself can be exercised.
working_compose <- function(displacementField, warpingField) {
  f1 <- tempfile(fileext = ".nii.gz")
  f2 <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(f1, f2)))
  antsImageWrite(displacementField, f1)
  antsImageWrite(warpingField, f2)
  ref <- makeImage(dim(displacementField), 0,
    spacing = antsGetSpacing(displacementField),
    origin = antsGetOrigin(displacementField),
    direction = antsGetDirection(displacementField))
  out <- antsApplyTransforms(ref, ref, c(f1, f2), compose = tempfile())
  field <- antsImageRead(out)
  unlink(out)
  field
}

test_that("composeDisplacementFields fails on antsImage fields (known bug)", {
  domain <- makeImage(c(8, 8), 0)
  field <- as.antsImage(array(0, c(2, 8, 8)), components = TRUE)
  expect_error(suppressWarnings(composeDisplacementFields(field, field)))
  expect_error(suppressWarnings(fitTransformToPairedPoints(lm_points(6), lm_points(6, 2),
    "diffeo", domainImage = domain, numberOfCompositions = 1)))
})

test_that("fitTransformToPairedPoints recovers known linear transforms", {
  fixed <- lm_points()
  theta <- pi / 9
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2)
  shift <- c(1.5, -0.5)
  center <- colMeans(fixed)
  moving <- sweep(sweep(fixed, 2, center) %*% t(R), 2, center + shift, "+")

  for (type in c("rigid", "affine", "similarity", "Rigid")) {
    xfrm <- fitTransformToPairedPoints(moving, fixed, transformType = type,
                                       regularization = 0)
    expect_s4_class(xfrm, "antsrTransform")
    mapped <- applyAntsrTransformToPoint(xfrm, fixed)
    expect_equal(mapped, moving, tolerance = 1e-4, ignore_attr = TRUE, info = type)
  }

  # similarity recovers an isotropic scale
  moving2 <- sweep(sweep(fixed, 2, center) * 1.3, 2, center, "+")
  xs <- fitTransformToPairedPoints(moving2, fixed, "similarity", regularization = 0)
  expect_equal(applyAntsrTransformToPoint(xs, fixed), moving2, tolerance = 0.05,
               ignore_attr = TRUE)

  # reflection forces the determinant fix in the Kabsch step
  refl <- fixed
  refl[, 1] <- -refl[, 1]
  xr <- fitTransformToPairedPoints(refl, fixed, "rigid", regularization = 0)
  A <- matrix(getAntsrTransformParameters(xr)[1:4], 2)
  expect_equal(det(A), 1, tolerance = 1e-6)

  # regularization is clamped to [0, 1]
  x1 <- fitTransformToPairedPoints(moving, fixed, "affine", regularization = 5)
  x2 <- fitTransformToPairedPoints(moving, fixed, "affine", regularization = 1)
  expect_equal(getAntsrTransformParameters(x1), getAntsrTransformParameters(x2))
  x3 <- fitTransformToPairedPoints(moving, fixed, "affine", regularization = -1)
  expect_equal(applyAntsrTransformToPoint(x3, fixed), moving, tolerance = 1e-4,
               ignore_attr = TRUE)
})

test_that("fitTransformToPairedPoints validates input", {
  fixed <- lm_points()
  expect_error(fitTransformToPairedPoints(fixed, fixed, "projective"), "not supported")
  expect_error(fitTransformToPairedPoints(fixed, fixed, "bspline"),
               "Domain image needs to be specified")
  expect_error(fitTransformToPairedPoints(fixed[1:3, ], fixed, "affine"),
               "Mismatch in the size")
})

test_that("fitTransformToPairedPoints fits bspline and tps displacement transforms", {
  domain <- makeImage(c(16, 16), 0)
  fixed <- lm_points(10)
  moving <- fixed + 0.5
  xb <- fitTransformToPairedPoints(moving, fixed, "bspline", domainImage = domain,
                                   numberOfFittingLevels = 3, meshSize = 2)
  expect_s4_class(xb, "antsrTransform")
  expect_equal(getAntsrTransformFixedParameters(xb)[1:2], c(16, 16))
  err <- mean(sqrt(rowSums((applyAntsrTransformToPoint(xb, fixed) - moving)^2)))
  expect_lt(err, 0.5)
  xt <- fitTransformToPairedPoints(moving, fixed, "tps", domainImage = domain)
  mapped <- applyAntsrTransformToPoint(xt, fixed)
  expect_lt(mean(sqrt(rowSums((mapped - moving)^2))), 0.2)
})

test_that("fitTransformToPairedPoints diffeo converges toward targets", {
  local_mocked_bindings(composeDisplacementFields = working_compose, .package = "ANTsR")
  domain <- makeImage(c(16, 16), 0)
  fixed <- lm_points(8)
  moving <- fixed + matrix(c(0.6, -0.4), nrow(fixed), 2, byrow = TRUE)
  start_err <- mean(sqrt(rowSums((fixed - moving)^2)))
  out <- capture.output(
    xd <- fitTransformToPairedPoints(moving, fixed, "diffeo", domainImage = domain,
                                     numberOfFittingLevels = 2, numberOfCompositions = 11,
                                     compositionStepSize = 0.5, sigma = 0.5,
                                     convergenceThreshold = -Inf, verbose = TRUE)
  )
  expect_true(any(grepl("Composition 11", out)))
  expect_true(any(grepl("Total elapsed time", out)))
  mapped <- applyAntsrTransformToPoint(xd, fixed)
  expect_lt(mean(sqrt(rowSums((mapped - moving)^2))), start_err)
  # early stopping when converged (threshold large)
  xd2 <- fitTransformToPairedPoints(moving, fixed, "diffeo", domainImage = domain,
                                    numberOfFittingLevels = 2, numberOfCompositions = 12,
                                    convergenceThreshold = 1e6)
  expect_s4_class(xd2, "antsrTransform")
})

test_that("fitTransformToPairedPoints syn returns symmetric transforms", {
  local_mocked_bindings(composeDisplacementFields = working_compose, .package = "ANTsR")
  domain <- makeImage(c(16, 16), 0)
  fixed <- lm_points(8)
  moving <- fixed + 0.5
  out <- capture.output(
    xs <- fitTransformToPairedPoints(moving, fixed, "syn", domainImage = domain,
                                     numberOfFittingLevels = 2, numberOfCompositions = 10,
                                     sigma = 0.5, convergenceThreshold = 1e6,
                                     verbose = TRUE)
  )
  expect_true(any(grepl("Composition 1:", out)))
  expect_named(xs, c("forwardTransform", "inverseTransform", "fixedToMiddleTransform",
                     "middleToFixedTransform", "movingToMiddleTransform",
                     "middleToMovingTransform"))
  for (x in xs) expect_s4_class(x, "antsrTransform")
  fwd <- applyAntsrTransformToPoint(xs$forwardTransform, fixed)
  expect_lt(mean(sqrt(rowSums((fwd - moving)^2))),
            mean(sqrt(rowSums((fixed - moving)^2))))
})

test_that("fitTransformToPairedPoints time-varying fits in 2D and 3D", {
  domain <- makeImage(c(12, 12), 0)
  fixed <- lm_points(8) * 0.75
  moving <- fixed + 0.5
  out <- capture.output(
    xt <- fitTransformToPairedPoints(moving, fixed, "tv", domainImage = domain,
                                     numberOfFittingLevels = 2, numberOfCompositions = 2,
                                     numberOfTimeSteps = 3, numberOfIntegrationSteps = 5,
                                     sigma = 0.5, verbose = TRUE)
  )
  expect_true(any(grepl("integration point", out)))
  expect_named(xt, c("forwardTransform", "inverseTransform", "velocityField"))
  expect_equal(dim(xt$velocityField), c(12, 12, 3))
  expect_equal(xt$velocityField@components, 2)

  set.seed(3)
  domain3 <- makeImage(c(6, 6, 6), 0)
  f3 <- matrix(runif(18, 1, 4), ncol = 3)
  xt3 <- fitTransformToPairedPoints(f3 + 0.3, f3, "time-varying", domainImage = domain3,
                                    numberOfFittingLevels = 1, numberOfCompositions = 1,
                                    numberOfTimeSteps = 2, numberOfIntegrationSteps = 3)
  expect_equal(dim(xt3$velocityField), c(6, 6, 6, 2))
})

test_that("fitTimeVaryingTransformToPointSets validates and fits", {
  domain <- makeImage(c(12, 12), 0)
  p1 <- lm_points(6) * 0.75
  sets <- list(p1, p1 + 0.25, p1 + 0.5)
  expect_error(fitTimeVaryingTransformToPointSets(p1, domainImage = domain),
               "list of corresponding")
  expect_error(fitTimeVaryingTransformToPointSets(sets, timePoints = c(0, 1),
               domainImage = domain), "number of time points")
  expect_error(fitTimeVaryingTransformToPointSets(sets, timePoints = c(0, 0.5, 2),
               domainImage = domain), "between 0 and 1")
  expect_error(fitTimeVaryingTransformToPointSets(sets[1:2], domainImage = domain),
               "three or greater")
  expect_error(fitTimeVaryingTransformToPointSets(sets), "Domain image")
  expect_error(fitTimeVaryingTransformToPointSets(list(p1, p1[1:3, ], p1),
               domainImage = domain), "number of points")
  expect_error(fitTimeVaryingTransformToPointSets(list(p1, cbind(p1, 1)[, 1:3, drop = FALSE], p1),
               domainImage = domain), "dimensionality")
  expect_error(fitTimeVaryingTransformToPointSets(sets, numberOfTimeSteps = 2,
               domainImage = domain), "at least as great")

  out <- capture.output(
    res <- fitTimeVaryingTransformToPointSets(sets, domainImage = domain,
      numberOfFittingLevels = 2, numberOfCompositions = 2,
      numberOfIntegrationSteps = 5, sigma = 0.5, verbose = TRUE)
  )
  expect_true(any(grepl("Total elapsed time", out)))
  expect_named(res, c("forwardTransform", "inverseTransform", "velocityField"))
  expect_equal(dim(res$velocityField), c(12, 12, 3))
  # initial velocity field: continue from previous result
  res2 <- fitTimeVaryingTransformToPointSets(sets, domainImage = domain,
    initialVelocityField = res$velocityField, numberOfFittingLevels = 2,
    numberOfCompositions = 1, numberOfIntegrationSteps = 5)
  expect_equal(dim(res2$velocityField), dim(res$velocityField))
  fwd <- applyAntsrTransformToPoint(res2$forwardTransform, p1)
  expect_equal(dim(fwd), dim(p1))
})

test_that("fitTimeVaryingTransformToPointSets handles interior time points", {
  # Non-monotone time points route an interior integration point through the
  # forward/backward averaging branch.
  domain <- makeImage(c(10, 10), 0)
  p1 <- lm_points(5) * 0.6
  sets <- lapply(0:4, function(k) p1 + 0.1 * k)
  res <- fitTimeVaryingTransformToPointSets(sets, timePoints = c(0.75, 0.5, 1, 0, 1),
    numberOfTimeSteps = 5, domainImage = domain, numberOfFittingLevels = 1,
    numberOfCompositions = 1, numberOfIntegrationSteps = 3)
  expect_equal(dim(res$velocityField), c(10, 10, 5))
  expect_true(all(is.finite(as.array(res$velocityField))))

  set.seed(4)
  domain3 <- makeImage(c(5, 5, 5), 0)
  q <- matrix(runif(12, 1, 3), ncol = 3)
  res3 <- fitTimeVaryingTransformToPointSets(list(q, q + 0.1, q + 0.2),
    domainImage = domain3, numberOfFittingLevels = 1, numberOfCompositions = 1,
    numberOfIntegrationSteps = 2)
  expect_equal(dim(res3$velocityField), c(5, 5, 5, 3))
})
