test_that("getNeighborhoodAtVoxel and getNeighborhoodInMask", {
  img <- makeImage(c(8, 8), 1:64)
  nb <- getNeighborhoodAtVoxel(img, c(4, 4), c(1, 1))
  expect_equal(nb$values, c(19, 20, 21, 27, 28, 29, 35, 36, 37))
  expect_equal(dim(nb$indices), c(9, 2))
  nb2 <- getNeighborhoodAtVoxel(img, c(4, 4), array(1, c(3, 3)), physical.coordinates = TRUE)
  expect_equal(nb2$values, nb$values)
  expect_equal(nb2$indices, nb$indices - 1)
  expect_error(getNeighborhoodAtVoxel(1, c(4, 4), c(1, 1)), "antsImage")
  expect_error(getNeighborhoodAtVoxel(img, "a", c(1, 1)), "numeric")
  expect_error(getNeighborhoodAtVoxel(img, c(4, 4), array(1, c(3, 3, 3))), "dimensions")

  m <- img * 0 + 1
  x <- getNeighborhoodInMask(img, m, c(1, 1))
  expect_equal(dim(x), c(9, 64))
  x2 <- getNeighborhoodInMask(img, m, c(1, 1),
    boundary.condition = "image",
    spatial.info = TRUE, get.gradient = TRUE
  )
  expect_true(all(c("values", "indices", "offsets") %in% names(x2)))
  expect_equal(dim(x2$offsets), c(9, 2))
  x3 <- getNeighborhoodInMask(img, m, c(1, 1), boundary.condition = "mean")
  expect_false(anyNA(x3))
  expect_error(getNeighborhoodInMask(1, m, 1), "antsImage")
  expect_error(getNeighborhoodInMask(img, 1, 1), "mask")
  expect_error(getNeighborhoodInMask(img, m, "a"), "radius")
})

test_that("getCentroids in 2D and 3D", {
  lab <- makeImage(c(10, 10), 0)
  lab[2:3, 2:3] <- 1
  lab[6:8, 6:8] <- 2
  cen <- getCentroids(lab)
  expect_equal(unname(cen[, 1:2]), matrix(c(1.5, 6, 1.5, 6), 2))
  expect_equal(colnames(cen), c("x", "y", "z", "t"))
  cc <- getCentroids(lab, clustparam = 1)
  expect_equal(nrow(cc), 2)
  expect_output(expect_equal(getCentroids(), 1))
  l3 <- makeImage(c(6, 6, 6), 0)
  l3[2:3, 2:3, 2:3] <- 1
  expect_equal(unname(getCentroids(l3)[1, ]), c(1.5, 1.5, 1.5, 0))
})

test_that("crossvalidatedR2 variants", {
  set.seed(1)
  x <- matrix(rnorm(40 * 3), 40)
  y <- x[, 1] + rnorm(40, sd = 0.2)
  r1 <- crossvalidatedR2(x, y, 4)
  expect_equal(dim(r1), c(4, 3))
  expect_true(all(r1[, 1] > 80))
  r2 <- crossvalidatedR2(x, y, 4, covariates = rnorm(40), fast = TRUE)
  expect_true(all(r2[, 1] > 80))
  r3 <- crossvalidatedR2(x, y, rep(1:4, 10), covariates = matrix(rnorm(80), 40))
  expect_equal(dim(r3), c(4, 3))
})

test_that("antsImageTypeCast chooses the widest type", {
  img <- makeImage(c(4, 4), 1:16)
  tl <- antsImageTypeCast(list(antsImageClone(img, "unsigned char"), antsImageClone(img, "unsigned int")))
  expect_equal(sapply(tl, pixeltype), rep("unsigned int", 2))
  expect_equal(sapply(antsImageTypeCast(list(img, antsImageClone(img, "double"))), pixeltype), rep("double", 2))
  expect_equal(sapply(antsImageTypeCast(list(img, antsImageClone(img, "unsigned char"))), pixeltype), rep("float", 2))
  dd <- antsImageTypeCast(list(img, antsImageClone(img, "unsigned char")), "double")
  expect_equal(sapply(dd, pixeltype), rep("double", 2))
  expect_equal(as.array(dd[[2]]), as.array(img))
})

test_that("antsImageMutualInformation and antsrMetricCreate", {
  set.seed(2)
  img <- makeImage(c(8, 8), 1:64)
  i2 <- makeImage(c(8, 8), runif(64))
  mi <- antsImageMutualInformation(img, i2)
  mi_self <- antsImageMutualInformation(img, img, 0.5, 16)
  expect_lt(mi_self, mi) # more (negative) MI with itself
  expect_output(expect_error(antsImageMutualInformation(antsImageClone(img, "unsigned char"), i2), "float"))
  expect_error(antsImageMutualInformation(img, makeImage(c(8, 8), 1, spacing = c(2, 2))), "physical space")
  m <- img * 0 + 1
  for (ty in c(
    "MeanSquares", "Correlation", "Demons", "ANTSNeighborhoodCorrelation",
    "JointHistogramMutualInformation", "MattesMutualInformation"
  )) {
    met <- antsrMetricCreate(img, i2,
      type = ty, fixed.mask = m, moving.mask = m,
      sampling.strategy = "random", sampling.percentage = 0.5
    )
    expect_true(is.finite(antsrMetricGetValue(met)), info = ty)
  }
  ms <- antsrMetricGetValue(antsrMetricCreate(img, img, type = "MeanSquares"))
  expect_equal(ms, 0)
  expect_error(antsrMetricCreate(img, i2, sampling.strategy = "foo"), "sampling.strategy")
  expect_error(antsrMetricCreate(img, i2, sampling.strategy = "regular", sampling.percentage = 2), "sampling.percentage")
  expect_error(antsrMetricCreate(img, i2, type = "MattesMutualInformation", nBins = 2), "bins")
  expect_error(antsrMetricCreate(img, makeImage(c(4, 4, 4), 1)), "same dimension")
  expect_error(antsrMetricCreate(img, antsImageClone(i2, "double")), "same pixeltype")
  expect_error(antsrMetricCreate(mergeChannels(list(img, img)), i2), "Multichannel")
})

test_that("antsAverageImages from lists and files", {
  img <- makeImage(c(8, 8), 1:64)
  av <- antsAverageImages(list(img, img * 3), verbose = FALSE)
  expect_equal(as.array(av), as.array(img * 2))
  invisible(capture.output(av2 <- antsAverageImages(list(img, img * 3), normalize = TRUE, weights = c(.25, .75))))
  expect_equal(mean(av2), 1, tolerance = 1e-5)
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  f1 <- file.path(td, "a.nii.gz")
  f2 <- file.path(td, "b.nii.gz")
  antsImageWrite(img, f1)
  antsImageWrite(img * 3, f2)
  expect_equal(mean(antsAverageImages(c(f1, f2), verbose = FALSE)), 65)
  antsImageWrite(makeImage(c(4, 4), 1), f2)
  expect_error(antsAverageImages(c(f1, f2), verbose = FALSE), "Different dimensions")
  expect_error(antsAverageImages(c(f1, file.path(td, "nope.nii.gz"))), "do not exist")
  expect_error(antsAverageImages(list(img), weights = c(1, 2)), "weights")
})

test_that("addNoiseToImage models", {
  set.seed(3)
  img <- makeImage(c(16, 16), 50)
  g <- addNoiseToImage(img, "additivegaussian", c(0, 1))
  expect_equal(mean(g), 50, tolerance = 0.5)
  expect_gt(sd(as.array(g)), 0)
  sp <- addNoiseToImage(img, "saltandpepper", c(0.1, 0, 100))
  expect_true(all(c(as.array(sp)) %in% c(0, 50, 100)))
  expect_equal(dim(addNoiseToImage(img, "shot", 1)), c(16, 16))
  expect_equal(dim(addNoiseToImage(img, "speckle", 1)), c(16, 16))
  expect_error(addNoiseToImage(img, "shot", c(1, 2)), "parameters")
  expect_error(addNoiseToImage(img, "additivegaussian", 1), "parameters")
  expect_error(addNoiseToImage(img, "saltandpepper", 1), "parameters")
  expect_error(addNoiseToImage(img, "speckle", c(1, 2)), "parameters")
})

test_that("fuzzySpatialCMeansSegmentation clusters a ramp", {
  img <- makeImage(c(8, 8), 1:64)
  expect_output(fz <- fuzzySpatialCMeansSegmentation(img,
    numberOfClusters = 3, radius = 1,
    maxNumberOfIterations = 3, verbose = TRUE
  ), "Dice")
  seg <- as.array(fz$segmentationImage)
  expect_equal(sort(unique(c(seg))), 1:3)
  expect_length(fz$probabilityImages, 3)
  expect_equal(seg[1, 1], 1)
  expect_equal(seg[8, 8], 3)
  fz2 <- fuzzySpatialCMeansSegmentation(img, mask = img * 0 + 1, numberOfClusters = 2, maxNumberOfIterations = 2)
  expect_length(fz2$probabilityImages, 2)
})

test_that("segmentShapeFromImage correlates a shape template", {
  set.seed(4)
  fi <- makeImage(c(20, 20), rnorm(400, mean = 1, sd = 0.1))
  shp <- makeImage(c(5, 5), 0)
  shp[2:4, 2:4] <- 1
  ss <- suppressWarnings(segmentShapeFromImage(fi, shp, getMask(fi, 0.5, Inf, 0)))
  expect_equal(dim(ss), c(20, 20))
  expect_true(all(abs(as.array(ss)) <= 1 + 1e-6, na.rm = TRUE))
  ss2 <- segmentShapeFromImage(fi, shp, scfun = function(a, b) sum(a * b), rad = c(1, 1))
  expect_equal(dim(ss2), c(20, 20))
  expect_output(expect_equal(segmentShapeFromImage(), 1))
})

test_that("rfSegmentation supervised and unsupervised", {
  skip_if_not_installed("randomForest")
  set.seed(5)
  msk <- makeImage(c(8, 8), 1)
  li <- makeImage(c(8, 8), rep(1:2, each = 32))
  fm <- matrix(rnorm(64 * 3), 64)
  fm[, 1] <- c(as.array(li)) + rnorm(64, sd = 0.1)
  rs <- rfSegmentation(fm, msk, li, ntrees = 10)
  expect_named(rs, c("segmentation", "probabilityimages", "rfModel"))
  expect_length(rs$probabilityimages, 2)
  rp <- rfSegmentationPredict(rs$rfModel, fm, msk)
  expect_equal(c(as.array(rp)), c(as.array(li)))
  px <- rfSegmentation(fm, msk, ntrees = 10)
  expect_equal(dim(px), c(64, 64))
  expect_output(expect_equal(rfSegmentation(), 1), "Usage")
})

test_that("cropImageFromCenterPoint and abpN4", {
  set.seed(6)
  img <- makeImage(c(20, 20), runif(400))
  cp <- cropImageFromCenterPoint(img, c(10, 10), c(5, 5))
  expect_equal(dim(cp), c(5, 5))
  # package bug: points outside the image yield an invalid crop domain
  expect_error(cropImageFromCenterPoint(img, c(-5, 30), c(5, 5)), "outside")

  img16 <- makeImage(c(20, 20), runif(400, 10, 100))
  a1 <- abpN4(img16, shrinkFactor = 1, convergence = list(iters = 5, tol = 1e-4))
  expect_equal(dim(a1), c(20, 20))
  a2 <- abpN4(img16, mask = img16 * 0 + 1, shrinkFactor = 1, convergence = list(iters = 5, tol = 1e-4))
  expect_equal(dim(a2), c(20, 20))
  a3 <- suppressWarnings(abpN4(img16, usen3 = TRUE))
  expect_equal(dim(a3), c(20, 20))
  expect_error(abpN4(1), "Missing image")
  expect_error(abpN4(img16, c(1, 2)), "intensityTruncation")
})
