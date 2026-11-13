test_that("euler characteristic densities for each field type", {
  for (ft in c("T", "F", "X", "Z")) {
    ec <- euler(3, c(2, 10), ft)
    expect_length(ec, 4)
    expect_true(all(is.finite(ec)))
  }
  expect_equal(euler(3, c(1, 10), "T")[1], 1 - pt(3, 10))
  expect_equal(euler(3, c(2, 10), "F")[1], 1 - pf(3, 2, 10))
  expect_equal(euler(3, c(1, 5), "X")[1], 1 - pchisq(3, 5))
  # higher threshold -> lower exceedance probability
  expect_lt(euler(4, c(1, 10), "T")[1], euler(2, c(1, 10), "T")[1])
  expect_error(euler(3, c(1, 10)), "fieldType")
  expect_error(euler(3, fieldType = "T"), "df")
  expect_error(euler(df = c(1, 10), fieldType = "T"), "u")
})

test_that("rftPval returns probabilities", {
  res <- c(1, 20, 150, 400)
  p <- rftPval(3, 1, 2, 4, 1, res, c(1, 10), "T")
  expect_named(p, c("Pcor", "Punc", "Ec", "ek"))
  expect_true(p$Pcor >= 0 && p$Pcor <= 1)
  expect_true(p$Punc >= 0 && p$Punc <= 1)
  expect_gt(p$Ec, 0)
  # larger cluster extent -> smaller uncorrected p
  p2 <- rftPval(3, 1, 5, 4, 1, res, c(1, 10), "T")
  expect_lt(p2$Punc, p$Punc)
  expect_warning(
    p3 <- rftPval(3, 1, 2, 4, 1.4, res, c(1, 10), "T"),
    "rounding"
  )
  expect_equal(p3, p)
  # n > 1 (conjunctions) exercises the matrix power loop
  p4 <- rftPval(3, 1, 0, 4, 3, res, c(1, 10), "T")
  expect_true(p4$Pcor <= 1)
  expect_error(rftPval(3, 1, 2, 4, 1, res, c(1, 10)), "fieldType")
  expect_error(rftPval(3, 1, 2, 4, 1, res, fieldType = "T"), "df")
  expect_error(rftPval(3, 1, 2, 4, 1, df = c(1, 10), fieldType = "T"), "resels")
  expect_error(rftPval(3, n = 1, resels = res, df = c(1, 10), fieldType = "T"), "atleast")
})

test_that("resels in 2D and input validation", {
  m2 <- makeImage(c(12, 12), 1)
  r <- resels(m2, c(1, 1))
  expect_equal(r, c(1, 22, 120, 1))
  expect_equal(resels(m2), r)
  # larger fwhm -> fewer resels
  r2 <- resels(m2, c(2, 2))
  expect_equal(r2[2:3], c(11, 30))
  expect_error(resels(as.array(m2)), "antsImage")
  expect_error(resels(m2 * 2), "binarized")
  # package bug: 3D resels uses image[i, j, k] with vector k, which fails
  # in the antsImage `[` method (`missing(k) || is.na(k)` on a vector)
  expect_error(resels(makeImage(c(5, 5, 5), 1), c(1, 1, 1)), "length")
})

test_that("estSmooth on 2D images and residual matrices", {
  set.seed(1)
  m2 <- makeImage(c(12, 12), 1)
  noise <- makeImage(c(12, 12), rnorm(144))
  e1 <- suppressWarnings(estSmooth(noise, m2, verbose = FALSE))
  expect_length(e1$fwhm, 2)
  expect_s4_class(e1$RPVImg, "antsImage")
  expect_equal(dim(e1$RPVImg), c(12, 12))
  # smoothing the noise increases the estimated FWHM
  e2 <- suppressWarnings(estSmooth(smoothImage(noise, 1.5), m2, verbose = FALSE))
  expect_true(all(e2$fwhm > e1$fwhm))

  mat <- matrix(rnorm(6 * 144), 6)
  mat[1, 1] <- NA
  invisible(capture.output(e3 <- estSmooth(mat, m2, verbose = TRUE)))
  expect_length(e3$fwhm, 2)
  expect_true(all(is.finite(e3$fwhm)))
  e4 <- estSmooth(mat, m2, rdf = 4, scaleResid = FALSE, sample = 4, verbose = FALSE)
  expect_length(e4$fwhm, 2)
  expect_error(estSmooth(noise), "mask")
  # package bug: 3D estSmooth fails for the same antsImage `[` reason as resels
  expect_error(
    suppressWarnings(estSmooth(makeImage(c(5, 5, 5), rnorm(125)), makeImage(c(5, 5, 5), 1), verbose = FALSE)),
    "length"
  )
})

test_that("statFieldThresh RFT thresholds", {
  x <- makeImage(c(12, 12, 12), 0)
  x[3:5, 3:5, 3:5] <- 8
  x[8:10, 8:10, 8:10] <- 5
  res <- c(1, 3, 5, 5)
  u <- statFieldThresh(x, 0.05, 10, 1, c(1, 1, 1), res, c(1, 20), "T", "pRFT")
  expect_true(u < 8 && u > 0)
  # the threshold is where the corrected p first reaches 0.05
  expect_gte(rftPval(3, 1, 0, u, 1, res, c(1, 20), "T")$Pcor, 0.05)
  expect_lt(rftPval(3, 1, 0, u + 0.01, 1, res, c(1, 20), "T")$Pcor, 0.05)
  res2 <- c(1, 20, 150, 400)
  uc <- statFieldThresh(x, 0.05, 10, 1, c(1, 1, 1), res2, c(1, 10), "T", "cRFT")
  expect_true(uc < 8 && uc > 0)
  expect_error(
    statFieldThresh(x, 1e-12, 10, 1, c(1, 1, 1), res2, c(1, 10), "T", "pRFT"),
    "No voxels survive"
  )
  # package bugs in the FDR branches
  expect_error(
    statFieldThresh(x, 0.05, 10, 1, c(1, 1, 1), res2, c(1, 10), "T", "cFDR"),
    "'u' not found"
  )
  expect_error(
    suppressWarnings(statFieldThresh(x * 1.5, 0.05, 10, 1, c(1, 1, 1), res2, c(1, 10), "Z", "pFDR")),
    "binary"
  )
})

test_that("rftResults produces cluster and peak tables", {
  x <- makeImage(c(12, 12, 12), 0)
  x[3:5, 3:5, 3:5] <- 8
  x[8:10, 8:10, 8:10] <- 5
  res <- c(1, 20, 150, 400)
  r1 <- rftResults(x, res, c(1, 1, 1), c(1, 10), "T", threshType = 3)
  expect_named(r1, c("SetStats", "ClusterStats", "PeakStats", "LabeledClusters", "threshold", "ClusterImage"))
  expect_equal(r1$LabeledClusters, 2)
  expect_equal(r1$threshold, 3)
  expect_equal(dim(r1$ClusterStats), c(2, 7))
  expect_equal(sort(unname(r1$ClusterStats[, "Voxels"])), c(27, 27))
  expect_equal(sort(unname(r1$PeakStats[, "MaxStat"])), c(5, 8))
  expect_s4_class(r1$ClusterImage, "antsImage")

  r2 <- rftResults(x, c(1, 3, 5, 5), c(1, 1, 1), c(1, 20), "T", threshType = "pRFT")
  expect_true(r2$threshold > 0 && r2$threshold < 8)

  sd <- file.path(tempfile("rft"), "")
  dir.create(sd)
  on.exit(unlink(sd, recursive = TRUE), add = TRUE)
  r3 <- rftResults(x, res, c(1, 1, 1), c(2, 10), "F", threshType = 3, statdir = sd)
  expect_true(all(c("ClusterImg.nii.gz", "ClusterStats", "PeakStats") %in% list.files(sd)))
  for (ft in c("Z", "X")) {
    p <- rftResults(x, res, c(1, 1, 1), c(1, 10), ft, threshType = 3)$PeakStats
    expect_true(all(p[, "P"] >= 0 & p[, "P"] <= 1))
  }
  # 2D with a resels-per-voxel image
  x2 <- makeImage(c(12, 12), 0)
  x2[3:5, 3:5] <- 6
  x2[8:10, 8:11] <- 4
  r4 <- rftResults(x2, res[1:3], c(1, 1), c(1, 10), "T", threshType = 3, RPVImg = makeImage(c(12, 12), 1))
  expect_equal(r4$LabeledClusters, 2)
  expect_equal(sort(unname(r4$ClusterStats[, "Voxels"])), c(9, 12))

  expect_error(rftResults(), "x")
  expect_error(rftResults(x), "resels")
  expect_error(rftResults(x, res), "fwhm")
  expect_error(rftResults(x, res, c(1, 1, 1)), "df")
  expect_error(rftResults(x, res, c(1, 1, 1), c(1, 10)), "fieldType")
  expect_error(rftResults(x, res, c(1, 1, 1), c(1, 10), "X", threshType = list()), "threshType must be")
})
