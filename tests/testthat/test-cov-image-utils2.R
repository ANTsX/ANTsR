make_biased <- function() {
  set.seed(1)
  img <- makeImage(c(24, 24), 0)
  img[6:18, 6:18] <- 100
  img <- smoothImage(img, 1) + makeImage(c(24, 24), runif(576, 1, 5))
  img * makeImage(c(24, 24), seq(1, 2, length.out = 576))
}

test_that("n4BiasFieldCorrection options", {
  bimg <- make_biased()
  bf <- n4BiasFieldCorrection(bimg,
    shrinkFactor = 1,
    convergence = list(iters = c(10, 10), tol = 1e-5), splineParam = c(50, 50),
    returnBiasField = TRUE
  )
  expect_equal(dim(bf), c(24, 24))
  expect_true(min(bf) > 0)
  out <- n4BiasFieldCorrection(bimg,
    mask = getMask(bimg, 1, Inf), weightMask = bimg * 0 + 1,
    shrinkFactor = 1, convergence = list(iters = 10, tol = 1e-5),
    rescaleIntensities = TRUE
  )
  expect_equal(dim(out), c(24, 24))
  expect_equal(antsGetSpacing(out), antsGetSpacing(bimg))
  expect_error(n4BiasFieldCorrection(bimg, splineParam = c(1, 2, 3)), "splineParam")
  expect_error(n4BiasFieldCorrection(bimg * 0 + 1), "no variation")
})

test_that("n3 bias field corrections", {
  bimg <- make_biased()
  n3 <- n3BiasFieldCorrection2(bimg, shrinkFactor = 1, convergence = list(iters = 10, tol = 1e-5))
  expect_equal(dim(n3), c(24, 24))
  bf <- n3BiasFieldCorrection2(bimg,
    mask = getMask(bimg, 1, Inf), weightMask = bimg * 0 + 1,
    shrinkFactor = 1, convergence = list(iters = 10, tol = 1e-5),
    returnBiasField = TRUE
  )
  expect_true(min(bf) > 0)
  expect_error(n3BiasFieldCorrection2(bimg, splineParam = c(1, 2, 3)), "splineParam")
  expect_error(n3BiasFieldCorrection2(bimg * 0 + 1), "no variation")
  n3o <- n3BiasFieldCorrection(bimg, 2)
  expect_equal(dim(n3o), c(24, 24))
})

test_that("multiLabelMorphology preserves labels", {
  lab <- makeImage(c(12, 12), 0)
  lab[2:4, 2:4] <- 1
  lab[7:9, 7:9] <- 2
  md <- multiLabelMorphology(lab, "MD", 1)
  expect_equal(sort(unique(c(as.array(md)))), c(0, 1, 2))
  expect_gt(sum(md == 1), sum(lab == 1))
  me <- multiLabelMorphology(lab, "ME", 1)
  expect_lt(sum(me > 0), sum(lab > 0))
  mo <- multiLabelMorphology(lab, "MO", 1)
  expect_equal(dim(mo), dim(lab))
  mc <- multiLabelMorphology(lab, "MC", 1)
  expect_equal(dim(mc), dim(lab))
  mdm <- multiLabelMorphology(lab, "MD", 1, dilationMask = lab * 0 + 1, labelList = c(1, 2))
  expect_true(all(c(1, 2) %in% c(as.array(mdm))))
  expect_error(multiLabelMorphology(lab, "MD", 1, dilationMask = lab), "not binary")
})

test_that("morphology operations and structuring elements", {
  b <- makeImage(c(12, 12), 0)
  b[5:8, 5:8] <- 1
  expect_equal(sum(morphology(b, "dilate", 1, shape = "ball")), 36)
  expect_equal(sum(morphology(b, "dilate", 1, shape = "box")), 36)
  expect_equal(sum(morphology(b, "dilate", 1, shape = "cross")), 32)
  expect_equal(sum(morphology(b, "dilate", 1, shape = "annulus")), 36)
  expect_equal(sum(morphology(b, "dilate", 1, shape = "polygon")), 16)
  expect_equal(sum(morphology(b, "erode", 1)), 4)
  expect_equal(sum(morphology(b, "open", 1, shape = "cross")), 12)
  expect_equal(sum(morphology(b, "close", 1)), 16)
  expect_equal(sum(morphology(b, "dilate", 1, type = "grayscale")), 36)
  expect_equal(sum(morphology(b, "erode", 1, type = "greyscale")), 4)
  expect_error(morphology(mergeChannels(list(b, b)), "dilate", 1), "Multichannel")
  expect_error(morphology(b, "explode", 1))
})

test_that("mni2tal, maskImage, interleaveMatrixWithItself", {
  expect_equal(mni2tal(c(10, 12, 14)), c(-9.9, -10.9816, 13.4466), tolerance = 1e-6)
  expect_equal(mni2tal(c(10, 12, -14)), c(-9.9, -12.2136, -11.164), tolerance = 1e-6)
  expect_error(mni2tal(1), "3 coordinates")
  lab <- makeImage(c(12, 12), 0)
  lab[2:4, 2:4] <- 1
  lab[7:9, 7:9] <- 2
  expect_equal(sum(maskImage(lab, lab, 2)), 18)
  expect_equal(sum(maskImage(lab, lab, c(1, 2), binarize = TRUE)), 18)
  expect_equal(sum(maskImage(lab, lab, c(1, 2))), 27)
  x <- matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
  xi <- interleaveMatrixWithItself(x, 2)
  expect_equal(colnames(xi), c("a1", "a2", "b1", "b2"))
  expect_equal(unname(xi[, 3]), c(3, 4))
  expect_output(expect_equal(interleaveMatrixWithItself(), 1))
})

test_that("imageListToMatrix with resampling and smoothing", {
  set.seed(2)
  il <- list(makeImage(c(6, 6), rnorm(36)), makeImage(c(6, 6), rnorm(36)), makeImage(c(3, 3), rnorm(9)))
  invisible(capture.output(m <- imageListToMatrix(il, makeImage(c(6, 6), 1))))
  expect_equal(dim(m), c(3, 36))
  expect_equal(m[1, ], c(as.array(il[[1]])), tolerance = 1e-6)
  invisible(capture.output(m2 <- imageListToMatrix(il[1:2], makeImage(c(6, 6), 1), sigma = 1)))
  expect_equal(dim(m2), c(2, 36))
  expect_lt(sd(m2[1, ]), sd(m[1, ]))
  invisible(capture.output(m3 <- imageListToMatrix(lapply(il[1:2], function(x) x + 10))))
  expect_equal(nrow(m3), 2)
})

test_that("icawhiten and histogram matching", {
  set.seed(3)
  iw <- icawhiten(matrix(rnorm(200), 20), 3)
  expect_equal(dim(iw), c(3, 10))
  expect_error(icawhiten(array(1:8, c(2, 2, 2)), 1))
  src <- makeImage(c(20, 20), runif(400))
  ref <- makeImage(c(20, 20), rnorm(400, 5))
  h <- histogramMatchImage2(src, ref)
  expect_s4_class(h, "antsImage")
  expect_equal(mean(h), mean(ref), tolerance = 0.2)
  h2 <- histogramMatchImage2(src, ref,
    sourceMask = src * 0 + 1, referenceMask = ref * 0 + 1,
    matchPoints = seq(0, 1, by = 0.1)
  )
  expect_equal(dim(h2), c(20, 20))
  expect_error(histogramMatchImage2(src, ref, matchPoints = c(0, 0.5, 2)), "range")
  h1 <- histogramMatchImage(src, ref)
  expect_equal(mean(h1), mean(ref), tolerance = 0.2)
})
