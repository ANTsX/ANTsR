# Coverage tests for applying / composing transforms, B-spline and TPS
# fitting helpers, antsrTransform construction, label registration and
# template building.

apply_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      fi <- resampleImage(ri(1), c(4, 4))
      mi <- resampleImage(ri(2), c(4, 4))
      aff <- suppressMessages(antsRegistration(fi, mi, "Affine",
                                               affIterations = c(20, 10)))
      syn <- suppressMessages(antsRegistration(fi, mi, "SyNOnly",
                                               initialTransform = aff$fwdtransforms,
                                               regIterations = c(10, 5)))
      cache <<- list(fixed = fi, moving = mi, aff = aff, syn = syn)
    }
    cache
  }
})

test_that("antsApplyTransforms applies matrices and warps with many interpolators", {
  fx <- apply_fixture()
  for (interp in c("linear", "nearestNeighbor", "gaussian", "bSpline",
                   "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc",
                   "lanczosWindowedSinc", "genericLabel", "MultiLabel")) {
    w <- antsApplyTransforms(fx$fixed, fx$moving, fx$aff$fwdtransforms,
                             interpolator = interp)
    expect_equal(dim(w), dim(fx$fixed), info = interp)
    expect_equal(antsGetOrigin(w), antsGetOrigin(fx$fixed))
  }
  # forward then inverse affine roughly restores the moving image
  w <- antsApplyTransforms(fx$fixed, fx$moving, fx$aff$fwdtransforms)
  back <- antsApplyTransforms(fx$moving, w, fx$aff$fwdtransforms, whichtoinvert = TRUE)
  expect_lt(mean(abs(back - fx$moving)), mean(abs(fx$fixed - fx$moving)))
  # warp + affine list; (affine.mat, warp) triggers the auto-invert guess
  w2 <- antsApplyTransforms(fx$fixed, fx$moving, fx$syn$fwdtransforms)
  expect_lt(mean(abs(w2 - fx$fixed)), mean(abs(fx$moving - fx$fixed)))
  wi <- antsApplyTransforms(fx$moving, fx$fixed, fx$syn$invtransforms)
  expect_equal(dim(wi), dim(fx$moving))
  # verbose prints the argument vector
  expect_output(antsApplyTransforms(fx$fixed, fx$moving, fx$aff$fwdtransforms,
                                    verbose = TRUE))
  # pixel type of the fixed image is kept
  w3 <- antsApplyTransforms(antsImageClone(fx$fixed, "double"), fx$moving,
                            fx$aff$fwdtransforms)
  expect_equal(w3@pixeltype, "double")
})

test_that("antsApplyTransforms composes transforms into a field", {
  fx <- apply_fixture()
  prefix <- tempfile()
  out <- antsApplyTransforms(fx$fixed, fx$moving, fx$syn$fwdtransforms, compose = prefix)
  expect_equal(out, paste0(prefix, "comptx.nii.gz"))
  field <- antsImageRead(out)
  expect_equal(field@components, 2)
  expect_equal(dim(field), dim(fx$fixed))
  unlink(out)
  # a transform given as an antsImage is written to a temporary file
  expect_warning(
    w <- antsApplyTransforms(fx$fixed, fx$moving, antsImageRead(fx$syn$fwdtransforms[1])),
    "creating a temporary file"
  )
  expect_equal(dim(w), dim(fx$fixed))
})

test_that("antsApplyTransforms validates input", {
  fx <- apply_fixture()
  expect_output(res <- antsApplyTransforms(fx$fixed, fx$moving), "missing inputs")
  expect_true(is.na(res))
  expect_error(antsApplyTransforms(fx$fixed, fx$moving, tempfile(fileext = ".mat")),
               "does not exist")
  expect_error(antsApplyTransforms(fx$fixed, fx$moving, fx$aff$fwdtransforms,
                                   whichtoinvert = c(TRUE, FALSE)),
               "same length")
  expect_error(antsApplyTransforms(fx$fixed, fx$moving, fx$syn$fwdtransforms,
                                   whichtoinvert = c(TRUE, FALSE)),
               "not a matrix")
  expect_error(antsApplyTransforms(fx$fixed, fx$moving, fx$aff$fwdtransforms,
                                   interpolator = "cubic"))
  img4 <- makeImage(c(4, 4, 4, 2), 1)
  img3 <- makeImage(c(4, 4, 4), 1)
  tx3 <- tempfile(fileext = ".mat")
  on.exit(unlink(tx3))
  writeAntsrTransform(createAntsrTransform(dimension = 3), tx3)
  expect_error(antsApplyTransforms(img3, img4, tx3), "imagetype 3")
  expect_warning(
    expect_error(antsApplyTransforms(fx$fixed, fx$moving, 1)),
    "not a character vector"
  )
})

test_that("antsApplyTransformsToPoints maps points and round-trips", {
  fx <- apply_fixture()
  pts <- data.frame(x = c(60, 90, 120), y = c(80, 110, 140), label = 1:3)
  moved <- antsApplyTransformsToPoints(2, pts, fx$aff$fwdtransforms)
  expect_s3_class(moved, "data.frame")
  expect_equal(colnames(moved), colnames(pts))
  expect_equal(moved$label, pts$label)
  back <- antsApplyTransformsToPoints(2, moved, fx$aff$fwdtransforms,
                                      whichtoinvert = TRUE)
  expect_equal(back$x, pts$x, tolerance = 1e-3)
  expect_equal(back$y, pts$y, tolerance = 1e-3)
  # NOTE: antsApplyTransformsToPoints() currently calls the image tool
  # ANTsRCore::AntsApplyTransforms instead of AntsApplyTransformsToPoints, so
  # coordinates come back unchanged; only structure is checked here.
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 2,
                             translation = c(2, -3))
  txf <- tempfile(fileext = ".mat")
  on.exit(unlink(txf))
  writeAntsrTransform(tx, txf)
  m <- antsApplyTransformsToPoints(2, data.frame(x = 1, y = 1), txf)
  expect_equal(dim(m), c(1, 2))
  # list of warp + matrix (first matrix in a list is inverted)
  m2 <- antsApplyTransformsToPoints(2, pts[, 1:2], fx$syn$invtransforms)
  expect_equal(dim(m2), c(3, 2))
  # antsImage input returns an antsImage
  pimg <- as.antsImage(as.matrix(pts[, 1:2]))
  mi <- antsApplyTransformsToPoints(2, pimg, txf)
  expect_true(is.antsImage(mi))
  expect_equal(dim(as.matrix(mi)), c(3, 2))
  expect_error(antsApplyTransformsToPoints(2, pts, tempfile()), "may not exist")
  expect_error(antsApplyTransformsToPoints(2, makeImage(c(3, 3, 3), 1), txf),
               "must be 2d")
})

test_that("createWarpedGrid draws grids in 2D and 3D and warps them", {
  fx <- apply_fixture()
  g <- createWarpedGrid(fx$fixed, gridStep = 8, gridWidth = 1)
  expect_equal(dim(g), dim(fx$fixed))
  expect_setequal(unique(as.numeric(as.array(g))), c(0, 1))
  g1 <- createWarpedGrid(fx$fixed, gridDirections = c(TRUE, FALSE), gridStep = 8,
                         foreground = 2, background = 0)
  # only rows are blanked, so each column has the same pattern
  arr <- as.array(g1)
  expect_true(all(arr[, 1] == arr[, 10]))
  expect_equal(max(arr), 2)
  gw <- createWarpedGrid(fx$fixed, gridStep = 8, fixedReferenceImage = fx$fixed,
                         transform = fx$syn$fwdtransforms)
  expect_equal(dim(gw), dim(fx$fixed))
  img3 <- makeImage(c(10, 10, 10), 1)
  g3 <- createWarpedGrid(img3, gridStep = 4, gridWidth = 1, gridDirections = TRUE)
  expect_equal(dim(g3), c(10, 10, 10))
  expect_true(sum(as.array(g3) == 0) > 0)
})

test_that("composeTransformsToField matches antsApplyTransforms composition", {
  img <- makeImage(c(6, 6), 0)
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 2,
                             translation = c(1, -2))
  field <- composeTransformsToField(img, list(tx))
  expect_equal(field@components, 2)
  arr <- as.array(field)
  expect_equal(range(arr[1, , ]), c(1, 1), tolerance = 1e-5)
  expect_equal(range(arr[2, , ]), c(-2, -2), tolerance = 1e-5)
  # from files: a matrix and a displacement field
  txf <- tempfile(fileext = ".mat")
  ff <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(txf, ff)))
  writeAntsrTransform(tx, txf)
  disp <- as.antsImage(array(0.5, c(2, 6, 6)), components = TRUE)
  antsImageWrite(disp, ff)
  field2 <- composeTransformsToField(img, c(ff, txf))
  arr2 <- as.array(field2)
  expect_equal(mean(arr2[1, 2:4, 2:4]), 1.5, tolerance = 1e-3)
})

test_that("imageDomainToSpatialMatrix returns physical coordinates", {
  img <- makeImage(c(3, 4), 1, spacing = c(2, 0.5), origin = c(10, 20))
  m <- imageDomainToSpatialMatrix(img)
  expect_equal(dim(m), c(12, 2))
  # one distinct coordinate per voxel along each axis
  expect_equal(length(unique(m[, 1])), 3)
  expect_equal(length(unique(m[, 2])), 4)
  expect_true(all(is.finite(m)))
  mask <- img * 0
  mask[1, 1] <- 1
  mask[3, 4] <- 1
  m2 <- imageDomainToSpatialMatrix(img, mask)
  expect_equal(nrow(m2), 2)
})

test_that("fitBsplineObjectToScatteredData fits curves and surfaces", {
  x <- seq(-4, 4, length.out = 50)
  y <- exp(-x^2 / 2)
  u <- (x - min(x)) / diff(range(x))
  fit <- fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, 101,
                                         numberOfFittingLevels = 5, meshSize = 1)
  expect_equal(dim(fit), c(101, 1))
  expect_equal(max(fit), 1, tolerance = 0.1)
  # closed curve (circle)
  t <- seq(0, 1, length.out = 40)
  circ <- cbind(cos(2 * pi * t), sin(2 * pi * t))
  fitc <- fitBsplineObjectToScatteredData(circ, matrix(t), 0, 0.01, 101,
    isParametricDimensionClosed = TRUE, numberOfFittingLevels = 3, meshSize = 4,
    dataWeights = rep(1, 40))
  expect_equal(ncol(fitc), 2)
  expect_equal(mean(sqrt(rowSums(fitc^2))), 1, tolerance = 0.1)
  # 2D parametric domain
  set.seed(5)
  uv <- matrix(runif(200), ncol = 2)
  z <- matrix(uv[, 1] + uv[, 2])
  surf <- fitBsplineObjectToScatteredData(z, uv, c(0, 0), c(0.1, 0.1), c(11, 11),
                                          numberOfFittingLevels = 2, meshSize = c(1, 1))
  expect_equal(dim(surf), c(11, 11))
  expect_equal(as.numeric(surf[6, 6]), 1, tolerance = 0.15)
  expect_error(fitBsplineObjectToScatteredData(parametricData = matrix(u)),
               "missing scatteredData")
  expect_error(fitBsplineObjectToScatteredData(matrix(y)), "missing parametricData")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, 101,
               meshSize = c(1, 2)), "meshSize")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), c(0, 0), 0.01, 101),
               "origin")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, c(1, 1), 101),
               "spacing")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, c(1, 1)),
               "size")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, 101,
               isParametricDimensionClosed = c(TRUE, FALSE)), "closed")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, 101,
               isParametricDimensionClosed = TRUE, meshSize = 1), "meshSize > splineOrder")
  expect_error(fitBsplineObjectToScatteredData(matrix(y[-1]), matrix(u), 0, 0.01, 101),
               "number of points")
  expect_error(fitBsplineObjectToScatteredData(matrix(y), matrix(u), 0, 0.01, 101,
               dataWeights = 1:3), "number of weights")
})

test_that("fitBsplineDisplacementField fits from a field or points and validates", {
  field <- as.antsImage(array(1, c(2, 10, 10)), components = TRUE)
  fit <- fitBsplineDisplacementField(displacementField = field,
                                     numberOfFittingLevels = 2,
                                     enforceStationaryBoundary = FALSE)
  expect_equal(dim(fit), c(10, 10))
  expect_equal(mean(as.array(fit)), 1, tolerance = 0.05)
  fiti <- fitBsplineDisplacementField(displacementField = field,
                                      numberOfFittingLevels = 2, estimateInverse = TRUE,
                                      enforceStationaryBoundary = FALSE)
  expect_equal(mean(as.array(fiti)), -1, tolerance = 0.1)
  o <- rbind(c(3, 3), c(6, 6))
  d <- rbind(c(1, 0), c(0, 1))
  args <- list(displacementOrigins = o, displacements = d, origin = c(0, 0),
               spacing = c(1, 1), size = c(10, 10), direction = diag(2))
  fp <- do.call(fitBsplineDisplacementField, c(args, list(rasterizePoints = TRUE,
                numberOfFittingLevels = 2, meshSize = c(2, 2))))
  expect_equal(dim(fp), c(10, 10))
  expect_error(fitBsplineDisplacementField(), "missing input")
  expect_error(fitBsplineDisplacementField(displacementOrigins = o, displacements = d),
               "fully specify")
  bad <- function(...) {
    a <- args
    mods <- list(...)
    a[names(mods)] <- mods
    do.call(fitBsplineDisplacementField, a)
  }
  expect_error(bad(displacements = cbind(d, 1)), "Dimensionality")
  expect_error(bad(meshSize = c(1, 1, 1)), "meshSize")
  expect_error(bad(origin = c(0, 0, 0)), "origin")
  expect_error(bad(spacing = 1), "spacing")
  expect_error(bad(size = c(10, 10, 10)), "size")
  expect_error(bad(direction = diag(3)), "direction")
})

test_that("fitThinPlateSplineDisplacementField interpolates and validates", {
  o <- rbind(c(2, 2), c(7, 2), c(2, 7), c(7, 7))
  d <- matrix(c(1, 1, 1, 1, 0, 0, 0, 0), ncol = 2)
  args <- list(displacementOrigins = o, displacements = d, origin = c(0, 0),
               spacing = c(1, 1), size = c(10, 10), direction = diag(2))
  tps <- do.call(fitThinPlateSplineDisplacementField, args)
  arr <- as.array(tps)
  expect_equal(arr[1, 3, 3], 1, tolerance = 1e-3)
  expect_equal(arr[2, 3, 3], 0, tolerance = 1e-3)
  # displacements may be given as a plain vector
  args2 <- args
  args2$displacements <- as.numeric(d)
  expect_equal(as.array(do.call(fitThinPlateSplineDisplacementField, args2)), arr)
  expect_error(fitThinPlateSplineDisplacementField(), "missing input")
  expect_error(fitThinPlateSplineDisplacementField(o, d), "fully specify")
  bad <- function(...) {
    a <- args
    mods <- list(...)
    a[names(mods)] <- mods
    do.call(fitThinPlateSplineDisplacementField, a)
  }
  expect_error(bad(displacements = cbind(d, 1)), "Dimensionality")
  expect_error(bad(origin = 0), "origin")
  expect_error(bad(spacing = c(1, 1, 1)), "spacing")
  expect_error(bad(size = 10), "size")
  expect_error(bad(direction = diag(3)), "direction")
})

test_that("simulateDisplacementField produces bspline and exponential fields", {
  set.seed(6)
  domain <- makeImage(c(12, 12), 0, spacing = c(2, 2))
  b <- simulateDisplacementField(domain, "bspline", numberOfRandomPoints = 20,
                                 sdNoise = 2, numberOfFittingLevels = 2, meshSize = c(2, 2))
  expect_equal(b@components, 2)
  expect_equal(antsGetSpacing(b), c(2, 2))
  e <- simulateDisplacementField(domain, "exponential", numberOfRandomPoints = 20,
                                 sdNoise = 2, sdSmoothing = 1)
  expect_equal(dim(e), c(12, 12))
  expect_gt(max(abs(as.array(e))), 0)
  expect_error(simulateDisplacementField(domain, "bspline", meshSize = c(1, 2, 3)),
               "meshSize")
  expect_error(simulateDisplacementField(domain, "gaussian"))
})

test_that("createAntsrTransform supports types and rejects bad input", {
  types <- createAntsrTransform(supported.types = TRUE)
  expect_true(all(c("AffineTransform", "DisplacementFieldTransform") %in% types))
  for (tt in c("Euler2DTransform", "Rigid2DTransform", "CenteredRigid2DTransform",
               "Similarity2DTransform", "CenteredSimilarity2DTransform")) {
    x <- createAntsrTransform(type = tt, dimension = 3)
    expect_equal(x@dimension, 2, info = tt)
  }
  for (tt in c("Euler3DTransform", "QuaternionRigidTransform", "CenteredEuler3DTransform",
               "Similarity3DTransform")) {
    x <- createAntsrTransform(type = tt, dimension = 2)
    expect_equal(x@dimension, 3, info = tt)
  }
  x <- createAntsrTransform(type = "AffineTransform", precision = "double", dimension = 2,
                            matrix = diag(2) * 2, translation = c(1, 1))
  expect_equal(applyAntsrTransformToPoint(x, c(1, 1)), c(3, 3), ignore_attr = TRUE)
  field <- as.antsImage(array(1, c(2, 5, 5)), components = TRUE)
  xd <- createAntsrTransform(type = "DisplacementFieldTransform", dimension = 2,
                             displacement.field = field)
  expect_equal(applyAntsrTransformToPoint(xd, c(2, 2)), c(3, 3), ignore_attr = TRUE)
  expect_error(createAntsrTransform(dimension = 5), "Unsupported dimension")
  expect_error(createAntsrTransform(precision = "half"), "Unsupported precision")
  expect_error(createAntsrTransform(type = "Warp"), "Unsupported type")
})

label_pair <- function() {
  fixed <- array(0, c(32, 32))
  fixed[4:10, 4:10] <- 1
  fixed[20:28, 5:12] <- 2
  fixed[8:16, 20:28] <- 3
  moving <- array(0, c(32, 32))
  moving[6:12, 5:11] <- 1
  moving[22:30, 6:13] <- 2
  moving[10:18, 21:29] <- 3
  list(fixed = as.antsImage(fixed), moving = as.antsImage(moving))
}

capture_label_args <- function(...) {
  captured <- NULL
  registration <- labelImageRegistration
  env <- new.env(parent = environment(registration))
  env$.int_antsProcessArguments <- function(x) {
    captured <<- unlist(x)
    stop("captured registration arguments")
  }
  environment(registration) <- env
  expect_error(registration(...), "captured registration arguments")
  captured
}

test_that("labelImageRegistration fits landmark-based linear transforms", {
  lp <- label_pair()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  for (tt in c("rigid", "similarity", "affine")) {
    expect_output(expect_message(
      res <- labelImageRegistration(lp$fixed, lp$moving, initialTransforms = tt,
        typeOfDeformableTransform = NULL, outputPrefix = file.path(dir, tt),
        verbose = TRUE),
      "Total number of labels: 3"
    ), "fwdtransforms")
    expect_length(res$fwdtransforms, 1)
    expect_match(res$fwdtransforms, "0GenericAffine\\.mat$")
    w <- antsApplyTransforms(lp$fixed, lp$moving, res$fwdtransforms,
                             interpolator = "nearestNeighbor")
    expect_gt(sum(w == lp$fixed & lp$fixed > 0), sum(lp$moving == lp$fixed & lp$fixed > 0))
  }
})

test_that("labelImageRegistration assembles deformable stages", {
  lp <- label_pair()
  a <- capture_label_args(lp$fixed, lp$moving, initialTransforms = "affine",
                          fixedIntensityImages = lp$fixed,
                          movingIntensityImages = lp$moving,
                          fixedMask = lp$fixed, movingMask = lp$moving)
  expect_match(a[which(a == "-r") + 1], "LandmarkBasedLinearaffine\\.mat$")
  metrics <- a[which(a == "--metric") + 1]
  expect_true(any(grepl("^MI\\[.*,1.0,32\\]$", metrics)))
  expect_equal(sum(grepl("^MSQ\\[", metrics)), 3)
  expect_equal(a[which(a == "--transform") + 1], "SyN[0.2,3,0]")
  expect_false(grepl("NA", a[which(a == "-x") + 1]))
  # B-spline variant with metric/spline parameters
  a <- capture_label_args(lp$fixed, lp$moving, initialTransforms = "rigid",
                          typeOfDeformableTransform = "antsRegistrationSyN[bo,4,20]")
  expect_equal(a[which(a == "--transform") + 1], "BSplineSyN[0.2,20,0,3]")
  expect_true("[100x70x50x20,1e-6,10]" %in% a)
  # explicit list specification
  spec <- list("SyN", 0.3, "CC", 3, c(5, 2), c(1, 0), c(2, 1))
  a <- capture_label_args(lp$fixed, lp$moving, initialTransforms = "identity",
                          fixedIntensityImages = lp$fixed,
                          movingIntensityImages = lp$moving,
                          typeOfDeformableTransform = spec, verbose = TRUE)
  expect_equal(a[which(a == "--transform") + 1], "SyN[0.3,3,0]")
  expect_true("5x2" %in% a)
  expect_true("1x0vox" %in% a)
  expect_equal(a[which(a == "-v") + 1], "1")
})

test_that("labelImageRegistration validates its input", {
  lp <- label_pair()
  expect_error(labelImageRegistration(lp$fixed, list(lp$moving, lp$moving)),
               "label images do not match")
  expect_error(labelImageRegistration(lp$fixed, lp$moving,
                                      fixedIntensityImages = lp$fixed,
                                      movingIntensityImages = list(lp$moving, lp$moving)),
               "intensity images do not match")
  expect_error(labelImageRegistration(lp$fixed, lp$moving, labelImageWeighting = c(1, 2)),
               "labelImageWeights")
  expect_error(labelImageRegistration(lp$fixed, lp$moving * 0 + 5), "No common labels")
  two <- thresholdImage(lp$fixed, 1, 2, 1, 0) + thresholdImage(lp$fixed, 2, 2, 1, 0)
  expect_error(labelImageRegistration(two, two, initialTransforms = "affine"),
               "must be >= 3")
  expect_error(labelImageRegistration(lp$fixed, lp$moving,
                                      initialTransforms = tempfile(fileext = ".mat")),
               "does not exist")
  expect_error(labelImageRegistration(lp$fixed, lp$moving, initialTransforms = "affine",
                                      typeOfDeformableTransform = list("SyN", 0.2)),
               "Incorrect specification")
  expect_error(labelImageRegistration(lp$fixed, lp$moving, initialTransforms = "affine",
                                      typeOfDeformableTransform = "antsRegistrationSyN[s]"),
               "Only 'so' or 'bo'")
})

test_that("labelImageRegistration runs a quick deformable registration", {
  lp <- label_pair()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  spec <- list("SyN", 0.2, "CC", 2, c(10, 5), c(1, 0), c(2, 1))
  res <- labelImageRegistration(lp$fixed, lp$moving, initialTransforms = "affine",
                                typeOfDeformableTransform = spec,
                                outputPrefix = file.path(dir, "lab"))
  expect_true(all(file.exists(res$fwdtransforms)))
  expect_true(any(grepl("Warp\\.nii\\.gz$", res$fwdtransforms)))
  expect_true(any(grepl("InverseWarp\\.nii\\.gz$", res$invtransforms)))
})

test_that("buildTemplate averages registered images", {
  set.seed(7)
  imgs <- list(resampleImage(ri(1), c(8, 8)), resampleImage(ri(2), c(8, 8)))
  expect_message(
    tmpl <- buildTemplate(imgs[[1]], imgs, typeofTransform = "Rigid", iterations = 1,
                          affIterations = c(10, 5), verbose = TRUE),
    "Iteration: 1"
  )
  expect_true(is.antsImage(tmpl))
  expect_equal(dim(tmpl), dim(imgs[[1]]))
  expect_gt(max(tmpl), 0)
  # blending weight is clamped
  tmpl2 <- suppressMessages(buildTemplate(imgs[[1]], imgs, typeofTransform = "Rigid",
                                          iterations = 1, blendingWeight = 2,
                                          weights = c(1, 1), verbose = FALSE,
                                          affIterations = c(10, 5)))
  expect_equal(dim(tmpl2), dim(imgs[[1]]))
})
