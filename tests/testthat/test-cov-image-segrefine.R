skip_if_not_installed("xgboost")

make_refine_data <- function(seed = 1) {
  set.seed(seed)
  truth <- array(0, c(16, 16))
  truth[4:12, 4:12] <- 1
  truth[6:10, 6:10] <- 2
  # segmentation is the truth shifted by one row: gives FP, FN, TP, TN voxels
  seg <- array(0, c(16, 16))
  seg[5:13, 4:12] <- 1
  seg[7:11, 6:10] <- 2
  f1 <- as.antsImage(truth * 10 + rnorm(256))
  f2 <- smoothImage(f1, 1)
  list(
    truth = as.antsImage(truth), seg = as.antsImage(seg),
    feats = list(f1, f2)
  )
}

test_that("segmentationRefinement train/predict round trip (numeric dilation)", {
  skip_if_new_xgboost()
  d1 <- make_refine_data(1)
  d2 <- make_refine_data(2)
  m <- suppressWarnings(suppressMessages(segmentationRefinement.train(
    featureImages = list(d1$feats, d2$feats),
    truthLabelImages = list(d1$truth, d2$truth),
    segmentationImages = list(d1$seg, d2$seg),
    featureImageNames = c("int", "smooth"),
    labelSet = c(1, 2),
    maximumNumberOfSamplesOrProportionPerClass = 0.5,
    dilationRadius = 1, neighborhoodRadius = 1
  )))
  expect_named(m, c("LabelModels", "LabelSet", "FeatureImageNames"))
  expect_length(m$LabelModels, 2)
  expect_equal(m$LabelSet, c(1, 2))
  expect_equal(m$FeatureImageNames, c("int", "smooth"))

  p <- suppressWarnings(suppressMessages(segmentationRefinement.predict(
    d1$seg, m$LabelSet, m$LabelModels, d1$feats, m$FeatureImageNames,
    dilationRadius = 1, neighborhoodRadius = 1
  )))
  expect_s4_class(p$RefinedSegmentationImage, "antsImage")
  expect_equal(dim(p$RefinedSegmentationImage), c(16, 16))
  expect_true(all(unique(as.array(p$RefinedSegmentationImage)) %in% c(0, 1, 2)))
  expect_length(p$ForegroundProbabilityImages, 2)
  pr <- as.array(p$ForegroundProbabilityImages[[1]])
  expect_true(all(pr >= 0 & pr <= 1))
})

test_that("segmentationRefinement with mm dilation and boundary regions", {
  skip_if_new_xgboost()
  d <- make_refine_data(3)
  for (entire in c(TRUE, FALSE)) {
    m <- suppressWarnings(suppressMessages(segmentationRefinement.train(
      list(d$feats[1]), list(d$truth), list(d$seg),
      labelSet = c(1, 2), maximumNumberOfSamplesOrProportionPerClass = 1,
      dilationRadius = "2mm", useEntireLabeledRegion = entire,
      normalizeSamplesPerLabel = c(TRUE)
    )))
    expect_equal(m$FeatureImageNames, "FeatureImage.1")
    p <- suppressWarnings(suppressMessages(segmentationRefinement.predict(
      d$seg, m$LabelSet, m$LabelModels, d$feats[1], m$FeatureImageNames,
      dilationRadius = "2mm", useEntireLabeledRegion = entire
    )))
    expect_equal(dim(p$RefinedSegmentationImage), dim(d$seg))
  }
  # non-entire region with numeric dilation
  m <- suppressWarnings(suppressMessages(segmentationRefinement.train(
    list(d$feats[1]), list(d$truth), list(d$seg),
    labelSet = c(1, 2), dilationRadius = 1, useEntireLabeledRegion = FALSE
  )))
  p <- suppressWarnings(suppressMessages(segmentationRefinement.predict(
    d$seg, m$LabelSet, m$LabelModels, d$feats[1], m$FeatureImageNames,
    dilationRadius = 1, useEntireLabeledRegion = FALSE
  )))
  expect_s4_class(p$RefinedSegmentationImage, "antsImage")
})

test_that("segmentationRefinement balanced sampling, background label and missing labels", {
  skip_if_new_xgboost()
  d <- make_refine_data(4)
  msgs <- capture_messages(m <- suppressWarnings(segmentationRefinement.train(
    list(d$feats[1]), list(d$truth), list(d$seg),
    maximumNumberOfSamplesOrProportionPerClass = 3, dilationRadius = 1
  )))
  expect_true(any(grepl("Number of samples per class = 3", msgs)))
  expect_equal(m$LabelSet, c(0, 1, 2))
  p <- suppressWarnings(suppressMessages(segmentationRefinement.predict(
    d$seg, m$LabelSet, m$LabelModels, d$feats[1], m$FeatureImageNames,
    dilationRadius = 1
  )))
  expect_equal(dim(p$RefinedSegmentationImage), c(16, 16))

  # a label absent from the segmentation gives an all-zero probability image
  seg2 <- thresholdImage(d$seg, 1, 1, 1, 0)
  m1 <- suppressWarnings(suppressMessages(segmentationRefinement.train(
    list(d$feats[1]), list(d$truth), list(d$seg),
    labelSet = c(1, 2), dilationRadius = 1
  )))
  msgs <- capture_messages(p2 <- suppressWarnings(
    segmentationRefinement.predict(seg2, m1$LabelSet, m1$LabelModels,
      d$feats[1], m1$FeatureImageNames,
      dilationRadius = 1
    )
  ))
  expect_true(any(grepl("No voxels exist for label 2", msgs)))
  expect_equal(sum(as.array(p2$ForegroundProbabilityImages[[2]])), 0)
})

test_that("segmentationRefinement input checking", {
  d <- make_refine_data(5)
  expect_error(
    segmentationRefinement.train(list(d$feats), list(d$truth, d$truth), list(d$seg, d$seg)),
    "feature image sets"
  )
  expect_error(
    segmentationRefinement.train(list(d$feats), list(d$truth), list(d$seg, d$seg)),
    "segmentation image sets"
  )
  expect_error(
    segmentationRefinement.train(list(d$feats), list(d$truth), list(d$seg),
      featureImageNames = c("a", "b"), normalizeSamplesPerLabel = c(TRUE, FALSE, TRUE)
    ),
    "normalizeSamplesPerLabel"
  )
  expect_error(
    segmentationRefinement.train(list(d$feats), list(d$truth), list(d$seg),
      featureImageNames = c("a", "b"), labelSet = c(1, 2),
      useEntireLabeledRegion = c(TRUE, FALSE, TRUE)
    ),
    "useEntireLabeledRegion"
  )
  expect_error(segmentationRefinement.predict(), "segmentation image")
  expect_error(segmentationRefinement.predict(d$seg), "label set")
  expect_error(segmentationRefinement.predict(d$seg, 1), "label models")
  expect_error(segmentationRefinement.predict(d$seg, 1, list(1)), "features images")
  expect_error(segmentationRefinement.predict(d$seg, 1, list(1), d$feats), "feature image names")
  expect_error(
    segmentationRefinement.predict(d$seg, c(1, 2), list(1), d$feats, "a"),
    "number of labels"
  )
  expect_error(
    segmentationRefinement.predict(d$seg, 1, list(1), d$feats, c("a", "b"),
      normalizeSamplesPerLabel = c(TRUE, TRUE, TRUE)
    ),
    "normalizeSamplesPerLabel"
  )
  expect_error(
    segmentationRefinement.predict(d$seg, 1, list(1), d$feats, c("a", "b"),
      useEntireLabeledRegion = c(TRUE, TRUE)
    ),
    "useEntireLabeledRegion"
  )
})
