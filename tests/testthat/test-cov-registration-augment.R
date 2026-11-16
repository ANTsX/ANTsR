# Coverage tests for random spatial / intensity data augmentation.

aug_images <- function() {
  img <- resampleImage(ri(1), c(8, 8))
  img2 <- resampleImage(ri(2), c(8, 8))
  seg <- thresholdImage(img, "Otsu", 3)
  seg2 <- thresholdImage(img2, "Otsu", 3)
  list(img = img, img2 = img2, seg = seg, seg2 = seg2)
}

test_that("randomlyTransformImageData produces linear simulations", {
  set.seed(10)
  d <- aug_images()
  for (tt in c("translation", "rotation", "rigid", "scaleShear", "affine")) {
    out <- randomlyTransformImageData(d$img, list(list(d$img), list(d$img2)),
                                      numberOfSimulations = 2, transformType = tt,
                                      sdAffine = 0.05)
    expect_named(out, c("simulatedImages", "simulatedTransforms", "whichSubject"))
    expect_length(out$simulatedImages, 2)
    expect_true(all(out$whichSubject %in% 1:2))
    sim <- out$simulatedImages[[1]][[1]]
    expect_equal(dim(sim), dim(d$img))
    expect_equal(antsGetSpacing(sim), antsGetSpacing(d$img))
    expect_s4_class(out$simulatedTransforms[[1]], "antsrTransform")
  }
  expect_error(randomlyTransformImageData(d$img, list(list(d$img)),
                                          transformType = "warp"),
               "not a possible option")
})

test_that("randomlyTransformImageData handles segmentations and resampling", {
  set.seed(11)
  d <- aug_images()
  # second subject lives on a different grid and is resampled to the reference
  other <- resampleImage(d$img2, c(48, 48), useVoxels = TRUE)
  other_seg <- resampleImage(d$seg2, c(48, 48), useVoxels = TRUE, interpType = 1)
  out <- randomlyTransformImageData(d$img,
    inputImageList = list(list(other, other)),
    segmentationImageList = list(other_seg),
    numberOfSimulations = 2, transformType = "affineAndDeformation",
    numberOfRandomPoints = 30, sdNoise = 1, numberOfFittingLevels = 2)
  expect_named(out, c("simulatedImages", "simulatedSegmentationImages",
                      "simulatedTransforms", "whichSubject"))
  expect_length(out$simulatedImages[[1]], 2)
  seg <- out$simulatedSegmentationImages[[1]]
  expect_equal(dim(seg), dim(d$img))
  expect_true(all(as.array(seg) %in% c(0, 1, 2, 3)))

  out <- randomlyTransformImageData(d$img, list(list(d$img)),
    segmentationImageList = list(d$seg), numberOfSimulations = 1,
    transformType = "deformation", deformationTransformType = "exponential",
    numberOfRandomPoints = 30, sdNoise = 1, sdSmoothing = 2)
  expect_equal(dim(out$simulatedImages[[1]][[1]]), dim(d$img))
})

test_that("histogramWarpImageIntensities keeps range and validates input", {
  set.seed(12)
  img <- resampleImage(ri(1), c(8, 8))
  w <- histogramWarpImageIntensities(img, breakPoints = c(0.25, 0.5, 0.75),
                                     displacements = c(0.05, -0.05, 0.05))
  expect_equal(dim(w), dim(img))
  expect_gt(cor(as.numeric(w), as.numeric(img)), 0.95)
  expect_false(isTRUE(all.equal(as.array(w), as.array(img))))
  # zero displacements leave intensities essentially unchanged
  w0 <- histogramWarpImageIntensities(img, displacements = c(0, 0, 0))
  expect_lt(max(abs(as.array(w0) - as.array(img))), 1)
  # clamped end points, scalar break points, random displacements
  wc <- histogramWarpImageIntensities(img, breakPoints = c(0.3, 0.6),
                                      clampEndPoints = c(TRUE, TRUE))
  expect_equal(dim(wc), dim(img))
  ws <- histogramWarpImageIntensities(img, breakPoints = 4,
                                      clampEndPoints = c(TRUE, FALSE))
  expect_equal(dim(ws), dim(img))
  expect_error(histogramWarpImageIntensities(img, clampEndPoints = TRUE), "length 2")
  expect_error(histogramWarpImageIntensities(img, breakPoints = c(-0.1, 0.5)),
               "range \\[0, 1\\]")
  expect_output(
    expect_error(histogramWarpImageIntensities(img, displacements = c(0.1, 0.1)),
                 "does not match"),
    "displacements"
  )
})

test_that("dataAugmentation returns images, segmentations and points", {
  # dataAugmentation() imports numpy whenever a point set list is supplied,
  # even when nothing is written; stub the import so no Python is needed.
  skip_if_not_installed("reticulate")
  local_mocked_bindings(import = function(...) NULL, .package = "reticulate")
  set.seed(13)
  d <- aug_images()
  pts <- matrix(c(20, 20, 30, 35, 40, 25), ncol = 2, byrow = TRUE)
  out <- capture.output(
    res <- dataAugmentation(list(list(d$img, d$img2)),
      segmentationImageList = list(d$seg),
      pointsetList = list(pts),
      numberOfSimulations = 2, transformType = "affine",
      noiseModel = "additivegaussian", noiseParameters = c(0, 0.02),
      sdSimulatedBiasField = 0.1, sdHistogramWarping = 0.02, verbose = TRUE)
  )
  expect_true(any(grepl("Processing simulation", out)))
  expect_named(res, c("simulatedImages", "simulatedSegmentationImages",
                      "simulatedPointsetList", "subjectIDs"))
  expect_length(res$simulatedImages, 2)
  expect_length(res$simulatedImages[[1]], 2)
  sim <- res$simulatedImages[[1]][[1]]
  expect_equal(dim(sim), dim(d$img))
  # intensities rescaled to the range of the spatially transformed input
  expect_lte(max(sim), max(d$img) + 1e-3)
  expect_equal(dim(res$simulatedPointsetList[[1]]), dim(pts))
  expect_equal(res$subjectIDs, c(1, 1))
})

test_that("dataAugmentation supports each noise model and output layout", {
  set.seed(14)
  d <- aug_images()
  for (nm in c("saltandpepper", "shot", "speckle")) {
    params <- switch(nm, saltandpepper = c(0.05, 0, 1), shot = 50, speckle = 0.1)
    res <- dataAugmentation(list(list(d$img)), numberOfSimulations = 1,
      transformType = "translation", noiseModel = nm, noiseParameters = params,
      sdSimulatedBiasField = 0, sdHistogramWarping = 0)
    expect_named(res, c("simulatedImages", "subjectIDs"), info = nm)
    expect_equal(dim(res$simulatedImages[[1]][[1]]), dim(d$img))
  }
  # random choice among several noise models
  res <- dataAugmentation(list(list(d$img)), numberOfSimulations = 1,
    transformType = "rigid", noiseModel = c("additivegaussian", "saltandpepper"),
    sdSimulatedBiasField = 0, sdHistogramWarping = 0)
  expect_length(res$simulatedImages, 1)
  # no noise, points only (stub the unconditional numpy import)
  skip_if_not_installed("reticulate")
  local_mocked_bindings(import = function(...) NULL, .package = "reticulate")
  res <- dataAugmentation(list(list(d$img)), pointsetList = list(matrix(c(30, 30), 1)),
    numberOfSimulations = 1, transformType = "rotation", noiseModel = NULL,
    sdSimulatedBiasField = 0, sdHistogramWarping = 0)
  expect_named(res, c("simulatedImages", "simulatedPointsetList", "subjectIDs"))
  # segmentation only
  res <- dataAugmentation(list(list(d$img)), segmentationImageList = list(d$seg),
    numberOfSimulations = 1, transformType = "scaleShear", noiseModel = NULL,
    sdSimulatedBiasField = 0, sdHistogramWarping = 0)
  expect_named(res, c("simulatedImages", "simulatedSegmentationImages", "subjectIDs"))
  expect_error(dataAugmentation(list(list(d$img)), numberOfSimulations = 1,
    transformType = "translation", noiseModel = "pink",
    sdSimulatedBiasField = 0, sdHistogramWarping = 0), "Unrecognized noise model")
})
