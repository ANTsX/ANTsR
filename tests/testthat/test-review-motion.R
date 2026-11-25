# Framewise displacement from antsMotionCorr parameters (MOCOparams):
# row-major ITK matrix, translation, and the transform center.

identityAffine <- c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)

test_that("affine matrix is read in ITK row-major order", {
  # shear x += 0.1 * y: non-symmetric, so a column-major read differs
  shear <- c(1, 0.1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)
  tx <- ANTsR:::.mocoTransform(shear, c(0, 0, 0))
  expect_equal(tx$matrix[1, 2], 0.1)
  expect_equal(tx$matrix[2, 1], 0)
  fd <- ANTsR:::.framewiseDisplacementFromParams(
    rbind(identityAffine, shear), c(0, 0, 0), rbind(c(0, 1, 0))
  )
  # (0, 1, 0) moves to (0.1, 1, 0); a column-major read would give 0
  expect_equal(fd$MeanDisplacement, c(0, 0.1))
})

test_that("mapping matches ITK for an affine with a nonzero center", {
  set.seed(1)
  A <- diag(3) + matrix(rnorm(9, sd = 0.05), 3, 3)
  trans <- c(1.5, -2, 0.7)
  center <- c(10, -20, 30)
  params <- c(t(A), trans) # row-major, as ITK GetParameters()
  itk <- createAntsrTransform(
    type = "AffineTransform", precision = "double", dimension = 3,
    parameters = params, fixed.parameters = center
  )
  pts <- rbind(c(0, 0, 0), c(12, -5, 40), c(-30, 8, 3))
  tx <- ANTsR:::.mocoTransform(params, center)
  ours <- sweep(pts %*% t(tx$matrix), 2, tx$offset, "+")
  expect_equal(ours, applyAntsrTransformToPoint(itk, pts), tolerance = 1e-8,
    ignore_attr = TRUE)
})

test_that("Euler3D rigid parameters match ITK", {
  angles <- c(0.05, -0.1, 0.2)
  trans <- c(1, 2, 3)
  center <- c(5, 6, 7)
  itk <- createAntsrTransform(
    type = "Euler3DTransform", precision = "double", dimension = 3,
    parameters = c(angles, trans), fixed.parameters = c(center, 0)
  )
  pts <- rbind(c(0, 0, 0), c(20, -10, 4))
  tx <- ANTsR:::.mocoTransform(c(angles, trans), center)
  ours <- sweep(pts %*% t(tx$matrix), 2, tx$offset, "+")
  expect_equal(ours, applyAntsrTransformToPoint(itk, pts), tolerance = 1e-8,
    ignore_attr = TRUE)
})

test_that("rotation about a nonzero center gives the analytic displacement", {
  theta <- 0.1
  center <- c(10, 20, 30)
  rz <- matrix(c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0, 0, 0, 1),
    3, 3, byrow = TRUE)
  rotation <- c(t(rz), 0, 0, 0)
  r <- 5
  pts <- rbind(
    center,                 # on the rotation axis
    center + c(0, 0, 7),    # also on the axis
    center + c(r, 0, 0)     # radius r from the axis
  )
  fd <- ANTsR:::.framewiseDisplacementFromParams(
    rbind(identityAffine, rotation), center, pts
  )
  chord <- 2 * r * sin(theta / 2)
  expect_equal(fd$MaxDisplacement, c(0, chord))
  expect_equal(fd$MeanDisplacement, c(0, chord / 3))
  # points on the axis do not move, but the rotation is still measured
  expect_gt(fd$MaxDisplacement[2], 0)

  # ignoring the center would treat the rotation as being about the origin
  # and report a spurious displacement (here 2.5 mm instead of 0.5 mm)
  noCenter <- ANTsR:::.framewiseDisplacementFromParams(
    rbind(identityAffine, rotation), c(0, 0, 0), pts
  )
  expect_gt(noCenter$MaxDisplacement[2], 4 * chord)
})

test_that("rigid (Euler3D) parameters give the same displacement", {
  theta <- 0.1
  center <- c(10, 20, 30)
  pts <- rbind(center, center + c(5, 0, 0))
  fd <- ANTsR:::.framewiseDisplacementFromParams(
    rbind(c(0, 0, 0, 0, 0, 0), c(0, 0, theta, 0, 0, 0)), center, pts
  )
  expect_equal(fd$MaxDisplacement[2], 2 * 5 * sin(theta / 2))
})

test_that("framewise displacement uses the fixed image center of gravity", {
  fixed <- makeImage(c(20, 20, 20), 0)
  fixed[11:20, 11:20, 11:20] <- 1
  fixed <- antsCopyImageInfo(fixed, fixed)
  center <- getCenterOfMass(fixed)
  theta <- 0.1
  rz <- matrix(c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0, 0, 0, 1),
    3, 3, byrow = TRUE)
  moco <- rbind(c(0, 0, identityAffine), c(0, 0, c(t(rz), 0, 0, 0)))
  mask <- fixed > 0
  fd <- ANTsR:::.mocoFramewiseDisplacement(moco, fixed, mask)
  pts <- antsTransformIndexToPhysicalPoint(
    mask, which(as.array(mask) > 0.5, arr.ind = TRUE)
  )
  radius <- sqrt(rowSums(sweep(pts[, 1:2], 2, center[1:2])^2))
  expect_equal(fd$MeanDisplacement[2], mean(2 * radius * sin(theta / 2)),
    tolerance = 1e-6)
})

test_that("unrecognized parameter counts give an informative error", {
  expect_error(ANTsR:::.mocoTransform(1:5, c(0, 0, 0)), "Unrecognized motion")
})
