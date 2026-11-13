test_that("geoSeg refines an initial segmentation", {
  set.seed(1)
  img <- makeImage(c(24, 24), 0)
  img[4:20, 4:20] <- 1
  img[8:16, 8:16] <- 2
  img[11:13, 11:13] <- 3
  img <- smoothImage(img * 30, 0.5) + makeImage(c(24, 24), rnorm(576))
  bm <- getMask(img, 5, Inf, 0)
  km <- kmeansSegmentation(img, 3, bm)
  invisible(capture.output(gs <- geoSeg(img, bm, km$probabilityimages, km, atroposits = 2, jacw = bm * 1)))
  expect_named(gs, c("segobj", "seginit", "thksig", "thkj", "jacw"))
  expect_length(gs$segobj$probabilityimages, 3)
  expect_true(all(as.array(gs$thksig) >= 0 & as.array(gs$thksig) <= 1))
  # jacobian computed from a TVMSQ registration
  invisible(capture.output(suppressMessages(gs2 <- geoSeg(img, bm, km$probabilityimages, km, atroposits = 2))))
  expect_s4_class(gs2$jacw, "antsImage")
  # vessel handling and a 4th prior
  invisible(capture.output(gs3 <- geoSeg(img, bm, c(km$probabilityimages, km$probabilityimages[1]), km,
    vesselopt = "dark", atroposits = 2, jacw = bm * 1
  )))
  expect_equal(dim(gs3$segobj$segmentation), c(24, 24))
})

test_that("pairwiseImageDistanceMatrix with clustering", {
  set.seed(2)
  img <- makeImage(c(24, 24), 0)
  img[6:18, 6:18] <- 10
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  fns <- file.path(td, paste0(1:3, ".nii.gz"))
  for (i in 1:3) antsImageWrite(smoothImage(img, i * 0.5) + makeImage(c(24, 24), runif(576)), fns[i])
  invisible(capture.output(suppressMessages(pw <- pairwiseImageDistanceMatrix(2, fns, nclusters = 2))))
  expect_equal(dim(pw$rawMatrix), c(3, 3))
  expect_true(all(is.na(diag(pw$rawMatrix))))
  expect_equal(pw$symmMatrix, t(pw$symmMatrix))
  expect_length(pw$clusters, 3)
  expect_length(pw$representatives, 2)
  invisible(capture.output(suppressMessages(pw2 <- pairwiseImageDistanceMatrix(2, fns[1:2], metrictype = "MSQ"))))
  expect_named(pw2, c("rawMatrix", "symmMatrix"))
  expect_gte(min(pw2$rawMatrix, na.rm = TRUE), 0)
})

test_that("reho on a small 4D series", {
  skip_if_not_installed("magic")
  set.seed(3)
  bold <- makeImage(c(4, 4, 4, 8), rnorm(512))
  invisible(capture.output(rh <- reho(bold, makeImage(c(4, 4, 4), 1))))
  expect_equal(dim(rh), c(4, 4, 4))
  expect_true(all(abs(as.array(rh)) <= 1))
  invisible(capture.output(rh2 <- reho(bold + 10, method = "pearson")))
  expect_equal(dim(rh2), c(4, 4, 4))
})

test_that("combineNuisancePredictors selection methods", {
  set.seed(4)
  inmat <- matrix(rnorm(30 * 10), 30)
  target <- inmat[, 1] + rnorm(30)
  gp <- matrix(rnorm(30 * 5), 30)
  cn <- combineNuisancePredictors(inmat, target, gp, maxpreds = 2, k = 3)
  expect_equal(dim(cn), c(30, 2))
  # selected columns come from the global predictors
  expect_true(all(apply(cn, 2, function(v) any(apply(gp, 2, function(g) isTRUE(all.equal(v, g)))))))
  cn2 <- combineNuisancePredictors(inmat, target, gp, maxpreds = 2, method = "svd")
  expect_equal(dim(cn2), c(30, 2))
  cn3 <- combineNuisancePredictors(inmat, target, gp, maxpreds = 5, k = 3, ordered = TRUE)
  expect_length(cn3, 30)
})

test_that("abpBrainExtraction runs with a rigid map", {
  set.seed(5)
  r16 <- antsImageRead(getANTsRData("r16"))
  r64 <- antsImageRead(getANTsRData("r64"))
  img <- resampleImage(r16, c(64, 64), useVoxels = TRUE, interpType = 0)
  tem <- resampleImage(r64, c(64, 64), useVoxels = TRUE, interpType = 0)
  tm <- getMask(tem)
  invisible(capture.output(suppressMessages(
    bx <- abpBrainExtraction(img, tem, tm, regtype = "Rigid", pad = 2, verbose = TRUE)
  )))
  expect_equal(dim(bx$brain), c(64, 64))
  expect_equal(dim(bx$bmask), c(64, 64))
  expect_true(all(c(as.array(bx$bmask)) %in% c(0, 1)))
  expect_gt(sum(bx$bmask), 0)
  expect_length(bx$dsearchvals, 100)
  td <- file.path(tempfile("abp"), "")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  invisible(capture.output(suppressMessages(
    bx2 <- abpBrainExtraction(img, tem, tm, temregmask = tm, regtype = "Rigid", tdir = td)
  )))
  expect_equal(bx2$pad, 0)
  expect_output(expect_null(abpBrainExtraction(img, tem, NULL)), "usage")
  # package bug: with no arguments the non-short-circuit `|` evaluates is.null(img)
  expect_error(abpBrainExtraction(), "missing")
})

test_that("petSUVR on a 4D series without motion correction", {
  set.seed(6)
  anat <- makeImage(c(16, 16, 16), 0)
  anat[4:13, 4:13, 4:13] <- 50
  anat[6:11, 6:11, 6:11] <- 100
  anat <- smoothImage(anat, 1)
  seg <- thresholdImage(anat, 30, 70)
  pet <- smoothImage(anat, 1.5) + makeImage(c(16, 16, 16), runif(4096))
  pet4 <- mergeListToNDImage(makeImage(c(16, 16, 16, 3), 0), list(pet, pet, pet))
  s1 <- suppressMessages(petSUVR(pet4, anat, seg, smoothingParameter = 1, noMotionCorr = TRUE))
  expect_named(s1, c("petSUVR", "petMask", "petMOCO"))
  expect_equal(dim(s1$petSUVR), c(16, 16, 16))
  # SUVR is normalised to ~1 in the reference region
  expect_equal(mean(s1$petSUVR[seg == 1]), 1, tolerance = 0.15)
  s2 <- suppressMessages(petSUVR(pet4, anat, seg * 2,
    smoothingParameter = 1, labelValue = 2,
    subtractBackground = TRUE, mapToPet = FALSE, noMotionCorr = TRUE
  ))
  expect_equal(dim(s2$petMask), c(16, 16, 16))
  # package bugs: 3D input is averaged as a time series (dimension mismatch),
  # and the 4D motion-correction path fails in ITK's extraction filter
  expect_error(suppressMessages(petSUVR(pet, anat, seg, smoothingParameter = 1)), "dimensions are not the same")
  expect_error(suppressMessages(petSUVR(pet4, anat, seg, smoothingParameter = 1)), "zero sized")
})
