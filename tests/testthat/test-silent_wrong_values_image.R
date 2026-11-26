context("Functions that silently returned wrong values (image/registration)")

# the sampling-default warning is tested explicitly; mark it as already shown
for (.tx in c("DenseRigid", "SyNabp", "antsRegistrationSyN*", "jointLabelFusion")) {
  assign(.tx, TRUE, envir = ANTsR:::.int_deprecationWarned)
}
rm(.tx)

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
  # DenseRigid keeps its old 0.8 when samplingPercentage is not given, and
  # warns (once per session) that the default will become 1.0 as in ANTsPy
  rm("DenseRigid", envir = ANTsR:::.int_deprecationWarned)
  expect_warning(
    out <- capture.output(suppressMessages(
      antsRegistration(imgs$f, imgs$m, "DenseRigid",
        affIterations = c(2, 1), printArgs = TRUE
      )
    )),
    "will change to 1"
  )
  expect_true(grepl("regular,0.8]", paste(out, collapse = " "), fixed = TRUE))
  expect_no_warning(capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "DenseRigid",
      affIterations = c(2, 1), printArgs = TRUE
    )
  )))
})

test_that("antsRegistrationSyN metric/transform strings are well formed", {
  imgs <- .small_reg_images()
  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuickRepro[b]",
      printArgs = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  # QuickRepro uses CC with radius 2 (ANTsPy registration(), do_repro)
  expect_true(grepl("CC\\[[^],]+,[^],]+,1,2\\]", out))
  expect_false(grepl(",132]", out, fixed = TRUE))
  expect_false(grepl(",1,32]", out, fixed = TRUE))
  # linear stages keep the old 0.2 by default (ANTsPy: 0.25)
  expect_true(grepl("GC\\[[^]]+,1,1,regular,0.2\\]", out))

  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]",
      printArgs = TRUE
    )
  ))
  out <- paste(out, collapse = " ")
  expect_true(grepl("--transform Affine[0.1] ", out, fixed = TRUE))
  expect_true(grepl("MI\\[[^]]+,1,32,regular,0.2\\]", out))
  # an explicit samplingPercentage is still honored
  out <- capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]",
      samplingPercentage = 0.6, printArgs = TRUE
    )
  ))
  expect_true(grepl("MI\\[[^]]+,1,32,regular,0.6\\]", paste(out, collapse = " ")))
  # without samplingPercentage: one warning per session about the future 0.25
  rm("antsRegistrationSyN*", envir = ANTsR:::.int_deprecationWarned)
  expect_warning(
    capture.output(suppressMessages(
      antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]", printArgs = TRUE)
    )),
    "will change to 0.25"
  )
  expect_no_warning(capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]", printArgs = TRUE)
  )))
  expect_no_warning(capture.output(suppressMessages(
    antsRegistration(imgs$f, imgs$m, "antsRegistrationSyNQuick[a]",
      samplingPercentage = 0.2, printArgs = TRUE
    )
  )))
})

test_that("maskImage binarizes with a scalar level", {
  img <- as.antsImage(matrix(c(5, 7, 9, 11), 2, 2))
  seg <- as.antsImage(matrix(c(1, 1, 2, 0), 2, 2))
  out <- as.array(maskImage(img, seg, 1, binarize = TRUE))
  expect_equal(out, matrix(c(1, 1, 0, 0), 2, 2))
  out <- as.array(maskImage(img, seg, 1, binarize = FALSE))
  expect_equal(out, matrix(c(5, 7, 0, 0), 2, 2))
  # as ANTsPy mask_image: binarize = (image != 0) & (mask == level)
  img0 <- as.antsImage(matrix(c(0, 7, 9, -2), 2, 2))
  seg2 <- as.antsImage(matrix(c(1, 1, 2, 3), 2, 2))
  expect_equal(as.array(maskImage(img0, seg2, 1, binarize = TRUE)), matrix(c(0, 1, 0, 0), 2, 2))
  expect_equal(
    as.array(maskImage(img0, seg2, c(1, 3), binarize = TRUE)),
    matrix(c(0, 1, 0, 1), 2, 2)
  )
  expect_equal(
    as.array(maskImage(img0, seg2, c(1, 3), binarize = FALSE)),
    matrix(c(0, 7, 0, -2), 2, 2)
  )
})

test_that("round(img, digits) honors digits; cumulative Math functions work", {
  vals <- c(1.234, 2.345, 3.456, 4.567)
  img <- as.antsImage(matrix(vals, 2, 2))
  expect_equal(c(as.array(round(img, 2))), round(vals, 2), tolerance = 1e-6)
  expect_equal(c(as.array(round(img))), round(vals))
  # vector and NA digits go through base round() instead of erroring
  expect_equal(c(as.array(round(img, c(0, 1)))), round(vals, c(0, 1)),
    tolerance = 1e-6)
  expect_true(all(is.na(as.array(round(img, NA)))))
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

test_that("affine landmark fit flips the smallest singular direction (ANTsPy)", {
  set.seed(1)
  x <- matrix(rnorm(60), 20, 3)
  y <- x %*% diag(c(1, 1, -1)) + matrix(rnorm(60, sd = 0.05), 20, 3)
  xfrm <- fitTransformToPairedPoints(y, x, transformType = "affine")
  A <- matrix(getAntsrTransformParameters(xfrm)[1:9], 3, 3, byrow = TRUE)
  # ants.polar_decomposition: Xtilde = U diag(d) diag(1, 1, -1) V^T
  xc <- sweep(x, 2, colMeans(x))
  yc <- sweep(y, 2, colMeans(y))
  X <- t(qr.solve(cbind(xc, 1), yc)[1:3, ])
  s <- svd(X)
  expect_equal(A, s$u %*% diag(s$d * c(1, 1, -1)) %*% t(s$v), tolerance = 1e-4)
  # values from ANTsPy 0.6.3 fit_transform_to_paired_points on the same points
  expect_equal(A[1, ], c(0.133950, -0.511184, 0.835491), tolerance = 1e-4)
})

test_that("applyAntsrTransformToPoint treats matrix rows as points", {
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 2, translation = c(1, 2))
  pts <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  expected <- sweep(pts, 2, c(1, 2), "+")
  expect_equal(applyAntsrTransformToPoint(tx, pts), expected)
  expect_equal(applyAntsrTransformToPoint(tx, c(1, 4)), c(2, 6))
  expect_equal(applyAntsrTransform(tx, pts), expected)
  expect_equal(applyAntsrTransform(tx, c(1, 4)), c(2, 6))
  expect_equal(applyAntsrTransformToVector(tx, pts), pts)
  expect_equal(applyAntsrTransform(tx, c(1, 4), dataType = "vector"), c(1, 4))
})

test_that("composeDisplacementFields gives w(x) + d(x + w(x)) as ANTsPy", {
  set.seed(6)
  makeField <- function() {
    a <- array(rnorm(2 * 24 * 24), c(2, 24, 24))
    smoothImage(as.antsImage(a, components = TRUE), 2) * 10
  }
  d <- makeField()
  w <- makeField()
  comp <- composeDisplacementFields(d, w)
  txD <- createAntsrTransform(type = "DisplacementFieldTransform", displacement.field = d)
  txW <- createAntsrTransform(type = "DisplacementFieldTransform", displacement.field = w)
  p <- c(11, 13)
  expected <- applyAntsrTransformToPoint(txD, applyAntsrTransformToPoint(txW, p)) - p
  expect_equal(as.array(comp)[, 12, 14], expected, tolerance = 1e-4)
})

test_that("dense landmark fits run and match their targets", {
  dom <- makeImage(c(32, 32), 0)
  fixed <- unname(as.matrix(expand.grid(c(10, 16, 22), c(10, 16, 22))))
  moving <- fixed + 2
  # ANTsPy 0.6.3 on the same points (fit_transform_to_paired_points):
  #   tv: forward error 0.0657, inverse error 0.0764
  #   diffeo: forward error 0.0020; syn: forward 0.00016, inverse 0.00014
  xf <- fitTransformToPairedPoints(moving, fixed,
    transformType = "tv",
    domainImage = dom, numberOfCompositions = 10, numberOfTimeSteps = 3,
    numberOfIntegrationSteps = 10, meshSize = 4, numberOfFittingLevels = 3
  )
  fw <- applyAntsrTransformToPoint(xf$forwardTransform, fixed)
  iv <- applyAntsrTransformToPoint(xf$inverseTransform, moving)
  expect_equal(mean(abs(fw - moving)), 0.0657, tolerance = 0.01)
  expect_equal(mean(abs(iv - fixed)), 0.0764, tolerance = 0.01)

  xd <- fitTransformToPairedPoints(moving, fixed,
    transformType = "diffeo",
    domainImage = dom, meshSize = 4, numberOfFittingLevels = 3
  )
  expect_lt(mean(abs(applyAntsrTransformToPoint(xd, fixed) - moving)), 0.01)

  xs <- fitTransformToPairedPoints(moving, fixed,
    transformType = "syn",
    domainImage = dom, meshSize = 4, numberOfFittingLevels = 3
  )
  expect_lt(mean(abs(applyAntsrTransformToPoint(xs$forwardTransform, fixed) - moving)), 0.01)
  expect_lt(mean(abs(applyAntsrTransformToPoint(xs$inverseTransform, moving) - fixed)), 0.01)
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
  # even sizes: the point is the lower of the two central voxels (ANTsPy)
  p <- as.array(cropImageFromCenterPoint(ramp, center, c(4, 4)))
  expect_equal(p[, 2], 9:12)
  p <- as.array(cropImageFromCenterPoint(ramp, center, c(2, 2)))
  expect_equal(p[, 1], 10:11)
  # a centre outside the image is honoured (not clamped to the edge): the
  # patch sits at the requested place and is entirely zero
  far <- cropImageFromCenterPoint(ramp + 1, c(-20, 9), c(5, 5))
  expect_equal(antsGetOrigin(far), c(-22, 7))
  expect_true(all(as.array(far) == 0))
  # partly outside: the outside part is zero, the inside part is image data
  edge <- as.array(cropImageFromCenterPoint(ramp, c(0, 9), c(5, 5)))
  expect_equal(edge[, 3], c(0, 0, 1, 2, 3))
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

test_that("reconstructImageFromPatches averages overlaps with a mask domain", {
  set.seed(5)
  for (sz in list(c(8, 9), c(7, 6, 5))) {
    ps <- rep(3, length(sz))
    img <- as.antsImage(array(rnorm(prod(sz)), sz))
    patches <- extractImagePatches(img, ps, "all", 1)
    # stride-1 overlaps are averaged (as ANTsPy's count_array), not summed
    full <- as.antsImage(array(1, sz))
    recon <- reconstructImageFromPatches(patches, full, 1, domainImageIsMask = TRUE)
    expect_equal(as.array(recon), as.array(img), tolerance = 1e-5)

    # only patches whose middle voxel is in the mask are supplied
    maskArray <- array(0, sz)
    if (length(sz) == 2) maskArray[3:6, 2:5] <- 1 else maskArray[3:5, 2:4, 2:3] <- 1
    # patch start indices in extraction order (last index fastest)
    starts <- as.matrix(rev(expand.grid(lapply(rev(sz - ps + 1), seq_len))))
    mid <- starts + matrix(round(ps / 2) - 1, nrow(starts), length(sz), byrow = TRUE)
    keep <- maskArray[mid] == 1
    recon <- reconstructImageFromPatches(patches[keep], as.antsImage(maskArray), 1,
      domainImageIsMask = TRUE
    )
    covered <- as.array(recon) != 0
    expect_equal(as.array(recon)[covered], as.array(img)[covered], tolerance = 1e-5)
    expect_true(all(covered[maskArray == 1]))
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
  # convergence string as in ANTsPy label_image_registration
  expect_true(grepl("--convergence [2x0,1e-6,10]", out, fixed = TRUE))

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

test_that("jointLabelFusion honors usecor and needs >= 2 atlases", {
  set.seed(4)
  base <- matrix(0, 16, 16)
  base[4:12, 4:12] <- 1
  tgt <- as.antsImage(base * 10 + matrix(rnorm(256), 16, 16))
  mask <- as.antsImage(matrix(1, 16, 16))
  atl <- lapply(c(1, 3, 0.5), function(s) {
    as.antsImage(s * base * 10 + matrix(rnorm(256), 16, 16))
  })
  labs <- lapply(1:3, function(i) as.antsImage(base + 1))
  # capture the metric passed to antsJointFusion without running it
  metricUsed <- function(...) {
    testthat::local_mocked_bindings(
      antsJointFusion = function(args) {
        stop("METRIC:", args[which(args == "-m") + 1])
      },
      .package = "ANTsRCore"
    )
    msg <- tryCatch(
      jointLabelFusion(tgt, mask, atl, labelList = labs, rad = 1, rSearch = 1, ...),
      error = function(e) conditionMessage(e)
    )
    sub(".*METRIC:", "", msg)
  }
  # default keeps the correlation metric ANTsR has always used
  expect_equal(metricUsed(), "PC")
  expect_equal(metricUsed(usecor = TRUE), "PC")
  expect_equal(metricUsed(usecor = FALSE), "MSQ")
  # without usecor: one warning per session that the default will change
  rm("jointLabelFusion", envir = ANTsR:::.int_deprecationWarned)
  expect_warning(metricUsed(), "will change to FALSE")
  expect_no_warning(metricUsed())
  expect_no_warning(metricUsed(usecor = TRUE))
  expect_error(
    jointLabelFusion(tgt, mask, atl[1], labelList = labs[1], usecor = TRUE),
    "at least 2 atlases"
  )
})

test_that("resampleImageToTarget passes ... to antsApplyTransforms", {
  img <- as.antsImage(matrix(1, 4, 4))
  tgt <- as.antsImage(matrix(0, 8, 8))
  expected <- matrix(7, 8, 8)
  expected[1:4, 1:4] <- 1
  expect_equal(as.array(resampleImageToTarget(img, tgt, f = 7)), expected)
  expected[expected == 7] <- 0
  expect_equal(as.array(resampleImageToTarget(img, tgt)), expected)
})

test_that("iMath errors clearly for listed but unsupported operations", {
  img <- as.antsImage(matrix(1:16, 4, 4))
  roi <- as.antsImage(matrix(rep(1:2, each = 8), 4, 4))
  expect_error(iMath(img, "LabelStats", roi), "not currently supported")
})

test_that("antsImageRead keeps components when dimension is given", {
  vec <- as.antsImage(array(rnorm(2 * 4 * 5), c(2, 4, 5)), components = TRUE)
  f <- tempfile(fileext = ".nii.gz")
  antsImageWrite(vec, f)
  img <- antsImageRead(f, dimension = 2)
  expect_equal(img@components, 2)
  expect_equal(as.array(img), as.array(vec), tolerance = 1e-6)
  expect_equal(antsImageRead(f)@components, 2)
})

test_that("rfSegmentation labels survive an unsigned char mask", {
  skip_if_not_installed("randomForest")
  set.seed(7)
  mask <- antsImageClone(as.antsImage(matrix(1, 10, 10)), "unsigned char")
  labs <- matrix(rep(c(300, 500), each = 50), 10, 10)
  labImg <- as.antsImage(labs)
  fmat <- cbind(as.numeric(labs) + rnorm(100, sd = 1), rnorm(100))
  rf <- rfSegmentation(fmat, mask, labImg, ntrees = 20)
  expect_setequal(unique(as.numeric(rf$segmentation[mask == 1])), c(300, 500))
  pred <- rfSegmentationPredict(rf$rfModel, fmat, mask)
  expect_setequal(unique(as.numeric(pred[mask == 1])), c(300, 500))
})

test_that("partialVolumeCorrection keeps NA and large values on integer images", {
  img <- antsImageClone(as.antsImage(matrix(c(200, 100, 50, 80), 2, 2)), "unsigned char")
  gm <- as.antsImage(matrix(c(0.4, 0, 0.5, 1), 2, 2))
  wm <- as.antsImage(matrix(c(0.6, 1, 0.5, 0), 2, 2))
  expect_warning(out <- partialVolumeCorrection(img, gm, wm, proportion = 0),
    "returning NA")
  vals <- as.array(out)
  expect_true(is.na(vals[2, 1])) # pure WM: GM flow cannot be recovered
  expect_equal(vals[1, 1], 500) # 200 / 0.4, above the unsigned char range
  expect_equal(vals[1, 2], 100)
})

test_that("rfSegmentation keeps the full label range and regression predictions", {
  skip_if_not_installed("randomForest")
  set.seed(9)
  mask <- antsImageClone(as.antsImage(matrix(1, 10, 10)), "unsigned char")
  vals <- c(2^31 + 5, 2^32 - 1) # valid unsigned 32-bit labels above 2^31 - 1
  out <- ANTsR:::.rfLabelImage(mask, rep(vals, each = 50))
  expect_equal(out@pixeltype, "unsigned int")
  expect_setequal(unique(as.numeric(out[mask == 1])), vals)
  # regression forest: numeric predictions are returned, not NA
  y <- rep(c(1.5, 7.25), each = 50) + rnorm(100, sd = 0.01)
  fmat <- cbind(y + rnorm(100, sd = 0.01), rnorm(100))
  rfr <- randomForest::randomForest(x = fmat, y = y, ntree = 20)
  pred <- rfSegmentationPredict(rfr, fmat, mask)
  expect_false(anyNA(as.array(pred)))
  expect_equal(as.numeric(pred[mask == 1]),
    as.numeric(predict(rfr, newdata = fmat)), tolerance = 1e-6)
})

test_that("masked reconstruction handles length-one patch axes", {
  set.seed(8)
  img <- as.antsImage(matrix(rnorm(48), 6, 8))
  full <- as.antsImage(matrix(1, 6, 8))
  patches <- extractImagePatches(img, c(1, 3), "all", 1)
  recon <- reconstructImageFromPatches(patches, full, 1, domainImageIsMask = TRUE)
  expect_equal(as.array(recon), as.array(img), tolerance = 1e-5)
  img3 <- as.antsImage(array(rnorm(5 * 6 * 4), c(5, 6, 4)))
  full3 <- as.antsImage(array(1, c(5, 6, 4)))
  patches3 <- extractImagePatches(img3, c(1, 3, 2), "all", 1)
  recon3 <- reconstructImageFromPatches(patches3, full3, 1, domainImageIsMask = TRUE)
  expect_equal(as.array(recon3), as.array(img3), tolerance = 1e-5)
})
