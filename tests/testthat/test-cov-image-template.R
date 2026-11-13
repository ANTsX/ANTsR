make_template_data <- function() {
  tmpl <- makeImage(c(12, 12, 12), 0)
  tmpl[3:10, 3:10, 3:10] <- 1
  tmpl <- smoothImage(tmpl, 1) * 100
  ba <- array(0, c(12, 12, 12))
  ba[1:6, , ] <- 3
  ba[7:12, , ] <- 5
  brod <- as.antsImage(ba)
  blobs <- makeImage(c(12, 12, 12), 0)
  blobs[3:4, 3:4, 3:4] <- 1
  blobs[8:9, 8:9, 8:9] <- 2
  # an identity affine at the expected output location skips registration
  op <- file.path(tempfile("gtc"), "Z")
  dir.create(dirname(op))
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 3)
  writeAntsrTransform(tx, paste0(op, "0GenericAffine.mat"))
  list(tmpl = tmpl, brod = brod, blobs = blobs, op = op)
}

test_that("getTemplateCoordinates labels blob centroids", {
  d <- make_template_data()
  on.exit(unlink(dirname(d$op), recursive = TRUE), add = TRUE)
  g <- getTemplateCoordinates(list(d$tmpl, d$blobs), list(d$tmpl, d$brod), outprefix = d$op)
  tp <- g$templatepoints
  expect_equal(nrow(tp), 2)
  # LPS -> RAS flip of x and y
  expect_equal(tp$x, c(-2, -8))
  expect_equal(tp$y, c(-2, -8))
  expect_equal(tp$z, c(2, 8))
  expect_true(all(c("Brodmann", "AAL") %in% names(tp)))
  expect_s4_class(g$myLabelsInTemplateSpace, "antsImage")
  expect_equal(as.array(g$myLabelsInTemplateSpace), as.array(d$blobs))

  g2 <- getTemplateCoordinates(list(d$tmpl, d$blobs), list(d$tmpl, d$brod, d$brod),
    outprefix = d$op, convertToTal = TRUE
  )
  expect_equal(nrow(g2$templatepoints), 2)
  expect_true(all(g2$templatepoints$z <= c(2, 8)))
  expect_output(expect_equal(getTemplateCoordinates(list(d$tmpl), list(d$tmpl)), 1))
})

test_that("getMultivariateTemplateCoordinates builds a network table", {
  d <- make_template_data()
  on.exit(unlink(dirname(d$op), recursive = TRUE), add = TRUE)
  stat <- makeImage(c(12, 12, 12), 0)
  stat[2:5, 2:5, 2:5] <- 3
  mg <- getMultivariateTemplateCoordinates(list(d$tmpl, stat, stat), list(d$tmpl, d$brod),
    outprefix = d$op, clustparam = 5, pvals = c(0.01, 0.02)
  )
  expect_equal(mg$networks$NetworkID, c("N1_node", "N2_node"))
  expect_equal(as.character(mg$networks$pval), c("0.01", "0.02"))
  mg2 <- getMultivariateTemplateCoordinates(list(d$tmpl, stat), list(d$tmpl, d$brod),
    outprefix = d$op, clustparam = 5, identifier = "A"
  )
  expect_equal(mg2$networks$NetworkID, "NA_node")
})

test_that("functionalLungSegmentation (fast settings) and input checks", {
  set.seed(1)
  img <- makeImage(c(10, 10, 10), rep(c(1, 3, 6, 9), length.out = 1000) + runif(1000))
  mask <- makeImage(c(10, 10, 10), 1)
  fl <- functionalLungSegmentation(img, mask,
    numberOfIterations = 2,
    numberOfAtroposIterations = 2, numberOfClusters = 3, biasCorrection = "n3",
    verbose = FALSE, mrfParameters = "[0.1,1x1x1]"
  )
  expect_named(fl, c("segmentationImage", "probabilityImages", "processedImage"))
  expect_length(fl$probabilityImages, 3)
  expect_equal(sort(unique(c(as.array(fl$segmentationImage)))), c(1, 2, 3))
  expect_true(max(fl$processedImage) <= 1 + 1e-6)
  invisible(capture.output(suppressMessages(
    fl2 <- functionalLungSegmentation(img, mask,
      numberOfIterations = 1,
      numberOfAtroposIterations = 1, numberOfClusters = 3, biasCorrection = "none",
      clusterCenters = c(0.2, 0.5, 0.9), verbose = FALSE, mrfParameters = "[0.1,1x1x1]"
    )
  )))
  expect_equal(dim(fl2$segmentationImage), c(10, 10, 10))
  expect_error(functionalLungSegmentation(makeImage(c(5, 5), 1), mask), "3-D")
  expect_error(functionalLungSegmentation(img), "Mask is missing")
  expect_error(functionalLungSegmentation(img, mask, numberOfIterations = 0), ">= 1")
  expect_error(
    functionalLungSegmentation(img, mask, numberOfClusters = 3, clusterCenters = c(1, 2), biasCorrection = "none", verbose = FALSE),
    "clusterCenters"
  )
})
