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

test_that("rigid landmark fit handles reflections (Kabsch)", {
  set.seed(1)
  x <- matrix(rnorm(60), 20, 3)
  # best orthogonal fit is a reflection; a proper rotation must be returned
  y <- x %*% diag(c(1, 1, -1)) + matrix(rnorm(60, sd = 0.05), 20, 3)
  xfrm <- fitTransformToPairedPoints(y, x, transformType = "rigid")
  A <- matrix(getAntsrTransformParameters(xfrm)[1:9], 3, 3, byrow = TRUE)
  expect_equal(det(A), 1, tolerance = 1e-4)

  xc <- sweep(x, 2, colMeans(x))
  yc <- sweep(y, 2, colMeans(y))
  s <- svd(t(yc) %*% xc)
  D <- diag(c(1, 1, sign(det(s$u %*% t(s$v)))))
  kabsch <- s$u %*% D %*% t(s$v)
  residual <- function(M) sum((yc - xc %*% t(M))^2)
  expect_lte(residual(A), residual(kabsch) + 1e-4)
})

test_that("time-varying landmark fit returns a non-identity inverse", {
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 2)
  pts <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  skip_if_not(
    is.matrix(applyAntsrTransformToPoint(tx, pts)),
    "applyAntsrTransformToPoint does not handle point matrices"
  )
  dom <- makeImage(c(32, 32), 0)
  fixed <- unname(as.matrix(expand.grid(c(10, 16, 22), c(10, 16, 22))))
  moving <- fixed + 2
  xf <- fitTransformToPairedPoints(moving, fixed,
    transformType = "tv",
    domainImage = dom, numberOfCompositions = 10, numberOfTimeSteps = 3,
    numberOfIntegrationSteps = 10, meshSize = 4, numberOfFittingLevels = 3
  )
  fw <- applyAntsrTransformToPoint(xf$forwardTransform, fixed)
  iv <- applyAntsrTransformToPoint(xf$inverseTransform, moving)
  expect_lt(mean(abs(fw - moving)), 0.5)
  expect_lt(mean(abs(iv - fixed)), 0.5)
})

test_that("antsApplyTransformsToPoints honors whichtoinvert", {
  tx1 <- createAntsrTransform(type = "AffineTransform", dimension = 2, translation = c(1, 0))
  tx2 <- createAntsrTransform(type = "AffineTransform", dimension = 2, translation = c(0, 2))
  f1 <- tempfile(fileext = ".mat")
  f2 <- tempfile(fileext = ".mat")
  writeAntsrTransform(tx1, f1)
  writeAntsrTransform(tx2, f2)
  pts <- data.frame(x = c(1, 5), y = c(2, 7))
  d1 <- as.matrix(antsApplyTransformsToPoints(2, pts, f1)) - as.matrix(pts)
  d2 <- as.matrix(antsApplyTransformsToPoints(2, pts, f2)) - as.matrix(pts)
  expect_equal(abs(d1[, 1]), c(1, 1), tolerance = 1e-5)

  out <- antsApplyTransformsToPoints(2, pts, c(f1, f2), whichtoinvert = c(FALSE, FALSE))
  expect_equal(as.matrix(out), as.matrix(pts) + d1 + d2, ignore_attr = TRUE)
  out <- antsApplyTransformsToPoints(2, pts, c(f1, f2), whichtoinvert = c(TRUE, FALSE))
  expect_equal(as.matrix(out), as.matrix(pts) - d1 + d2, ignore_attr = TRUE)
  out <- antsApplyTransformsToPoints(2, pts, c(f1, f2), whichtoinvert = c(FALSE, TRUE))
  expect_equal(as.matrix(out), as.matrix(pts) + d1 - d2, ignore_attr = TRUE)
  # default: two matrices are not inverted
  out <- antsApplyTransformsToPoints(2, pts, c(f1, f2))
  expect_equal(as.matrix(out), as.matrix(pts) + d1 + d2, ignore_attr = TRUE)
})

test_that("cropImageFromCenterPoint centers the patch on the point", {
  # voxel value = x index (1-based); spacing 1, origin 0
  ramp <- as.antsImage(matrix(rep(1:20, 20), 20, 20))
  center <- c(9, 9) # physical point of index (10, 10)
  p <- as.array(cropImageFromCenterPoint(ramp, center, c(5, 5)))
  expect_equal(p[, 3], 8:12)
  p <- as.array(cropImageFromCenterPoint(ramp, center, c(3, 3)))
  expect_equal(p[, 2], 9:11)
  p <- as.array(cropImageFromCenterPoint(ramp, center, c(7, 7)))
  expect_equal(p[, 4], 7:13)
})

test_that("segmentationRefinement.predict uses a 0.5 background row", {
  skip_if_not_installed("xgboost")
  seg <- matrix(0, 10, 10)
  seg[4:7, 4:7] <- 1
  segImg <- as.antsImage(seg)
  feat <- as.antsImage(matrix(rnorm(100), 10, 10))
  # fake model: same probability for every (FP, FN, TN, TP) class
  registerS3method("predict", "fakeRefinementModel", function(object, newdata, ...) {
    rep(object$p, 4 * nrow(newdata))
  })
  run <- function(p) {
    model <- structure(list(p = p), class = "fakeRefinementModel")
    suppressMessages(segmentationRefinement.predict(segImg, 1, list(model),
      list(feat), "T1",
      dilationRadius = 1
    ))$RefinedSegmentationImage
  }
  # foreground probability below 0.5 -> background
  expect_true(all(as.array(run(0.3)) == 0))
  expect_true(all(as.array(run(0.9))[seg == 1] == 1))
})

test_that("rfSegmentation returns label values, not factor codes", {
  skip_if_not_installed("randomForest")
  set.seed(2)
  lab <- matrix(2, 10, 10)
  lab[, 6:10] <- 5
  img <- as.antsImage(lab + matrix(rnorm(100, sd = 0.1), 10, 10))
  mask <- as.antsImage(matrix(1, 10, 10))
  labImg <- as.antsImage(lab)
  fmat <- matrix(img[mask == 1], ncol = 1)
  colnames(fmat) <- "intensity"
  rf <- rfSegmentation(fmat, mask, labImg, ntrees = 20)
  expect_setequal(unique(c(as.array(rf$segmentation))), c(2, 5))
  pred <- rfSegmentationPredict(rf$rfModel, fmat, mask)
  expect_equal(as.array(pred), lab)
})

test_that("sparseDecom verbose does not overwrite varex", {
  set.seed(1)
  mat <- replicate(30, rnorm(20))
  quiet <- sparseDecom(mat, nvecs = 2)
  capture.output(loud <- sparseDecom(mat, nvecs = 2, verbose = TRUE))
  expect_equal(length(loud$varex), length(quiet$varex))
  expect_true("meanReconstructionCorrelation" %in% names(loud))
  expect_true("Reconstruction" %in% names(loud))
})

test_that("abpN4 bias corrects the truncated image", {
  img <- resampleImage(ri(1), c(64, 64), useVoxels = TRUE, interpType = 0)
  truncated <- iMath(img, "TruncateIntensity", 0.025, 0.975, 256)
  expect_equal(
    as.array(abpN4(img)),
    as.array(n4BiasFieldCorrection(truncated)),
    tolerance = 1e-4
  )
})

test_that("partialVolumeCorrection honors proportion", {
  obs <- c(5, 6, 7, 8)
  gm <- c(0.5, 0.6, 0.7, 0.8)
  wm <- 1 - gm
  expect_equal(partialVolumeCorrection(obs, gm, wm, proportion = 0.25), obs / (gm + 0.25 * wm))
  expect_equal(partialVolumeCorrection(obs, gm, wm), obs / (gm + 0.4 * wm))
})

test_that("extractImagePatches -> reconstructImageFromPatches round-trips", {
  set.seed(3)
  cases <- list(
    list(c(6, 6), c(5, 5), 1), list(c(10, 8), c(3, 4), 1),
    list(c(9, 9), c(5, 5), 1), list(c(10, 10), c(4, 4), 2),
    list(c(9, 7), c(3, 3), c(2, 1)),
    list(c(6, 6, 6), c(5, 5, 5), 1), list(c(7, 6, 5), c(3, 2, 4), 1),
    list(c(8, 8, 8), c(4, 4, 4), 2)
  )
  for (cs in cases) {
    img <- as.antsImage(array(rnorm(prod(cs[[1]])), cs[[1]]))
    for (asArray in c(FALSE, TRUE)) {
      patches <- extractImagePatches(img, cs[[2]], "all", cs[[3]],
        returnAsArray = asArray
      )
      recon <- reconstructImageFromPatches(patches, img, cs[[3]])
      expect_equal(as.array(recon), as.array(img),
        tolerance = 1e-5,
        info = paste(c(cs[[1]], "|", cs[[2]], "|", cs[[3]], asArray), collapse = " ")
      )
    }
  }
})

test_that("labelImageRegistration uses every label of every pair", {
  makeLabels <- function(shift) {
    a <- matrix(0, 16, 16)
    a[(3:6) + shift, 3:6] <- 1
    a[(9:13) + shift, 9:13] <- 2
    as.antsImage(a)
  }
  fixed <- list(makeLabels(0), makeLabels(0))
  moving <- list(makeLabels(1), makeLabels(1))
  quickSyN <- list("SyN", 0.2, "CC", 2, c(2, 0), c(1, 0), c(2, 1))
  # default scalar labelImageWeighting with two label image pairs
  out <- capture.output(suppressMessages(
    labelImageRegistration(fixed, moving,
      initialTransforms = "identity",
      typeOfDeformableTransform = quickSyN, verbose = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  # 2 pairs x 2 labels
  expect_equal(lengths(regmatches(out, gregexpr("MSQ[", out, fixed = TRUE))), 4)

  # randomSeed is appended instead of replacing all arguments
  out <- capture.output(suppressMessages(
    labelImageRegistration(fixed[1], moving[1],
      initialTransforms = "identity",
      typeOfDeformableTransform = quickSyN, randomSeed = 1, verbose = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  expect_true(grepl("--random-seed 1", out, fixed = TRUE))
  expect_true(grepl("MSQ[", out, fixed = TRUE))
})
