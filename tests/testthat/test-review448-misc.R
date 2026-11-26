test_that("labelImageRegistration seeds QuickRepro and Repro presets", {
  old <- options(ants.randomSeed = NULL)
  on.exit(options(old))
  r16 <- resampleImage(ri(1), c(64, 64), 1, 0)
  r64 <- resampleImage(ri(2), c(64, 64), 1, 0)
  fixedSeg <- thresholdImage(r16, "Kmeans", 3)
  movingSeg <- thresholdImage(r64, "Kmeans", 3)
  deformableCommand <- function(preset, ...) {
    out <- capture.output(
      labelImageRegistration(fixedSeg, movingSeg,
        initialTransforms = "affine",
        typeOfDeformableTransform = preset, verbose = TRUE, ...
      )
    )
    # the deformable stage is the antsRegistration call with a SyN transform
    grep("^antsRegistration .*SyN\\[", out, value = TRUE)
  }
  quickRepro <- deformableCommand("antsRegistrationSyNQuickRepro[so]")
  expect_length(quickRepro, 1)
  expect_match(quickRepro, "--random-seed 1", fixed = TRUE)

  repro <- deformableCommand("antsRegistrationSyNRepro[so]")
  expect_match(repro, "--random-seed 1", fixed = TRUE)

  # an explicitly supplied seed is used
  seeded <- deformableCommand("antsRegistrationSyNQuickRepro[so]",
    randomSeed = 42
  )
  expect_match(seeded, "--random-seed 42", fixed = TRUE)

  # non-Repro presets still get no automatic seed
  quick <- deformableCommand("antsRegistrationSyNQuick[so]")
  expect_false(grepl("--random-seed", quick, fixed = TRUE))
})

test_that("reconstructImageFromPatches keeps length-one patch axes", {
  img <- makeImage(c(6, 6), rnorm(36))
  for (patchSize in list(c(1, 3), c(3, 1))) {
    patches <- extractImagePatches(img, patchSize,
      maxNumberOfPatches = "all", strideLength = 1, returnAsArray = TRUE
    )
    recon <- reconstructImageFromPatches(patches, img, strideLength = 1)
    expect_equal(as.array(recon), as.array(img), tolerance = 1e-6)

    listPatches <- extractImagePatches(img, patchSize,
      maxNumberOfPatches = "all", strideLength = 1, returnAsArray = FALSE
    )
    reconList <- reconstructImageFromPatches(listPatches, img, strideLength = 1)
    expect_equal(as.array(reconList), as.array(img), tolerance = 1e-6)
  }
  # strided path and an ordinary patch shape
  patches <- extractImagePatches(img, c(1, 3),
    maxNumberOfPatches = "all", strideLength = c(1, 3), returnAsArray = TRUE
  )
  recon <- reconstructImageFromPatches(patches, img, strideLength = c(1, 3))
  expect_equal(as.array(recon), as.array(img), tolerance = 1e-6)
  patches <- extractImagePatches(img, c(2, 3),
    maxNumberOfPatches = "all", strideLength = 1, returnAsArray = TRUE
  )
  recon <- reconstructImageFromPatches(patches, img, strideLength = 1)
  expect_equal(as.array(recon), as.array(img), tolerance = 1e-6)
})

test_that("reconstructImageFromPatches keeps length-one axes in 3D", {
  img <- makeImage(c(4, 4, 4), rnorm(64))
  patches <- extractImagePatches(img, c(1, 2, 2),
    maxNumberOfPatches = "all", strideLength = 1, returnAsArray = TRUE
  )
  recon <- reconstructImageFromPatches(patches, img, strideLength = 1)
  expect_equal(as.array(recon), as.array(img), tolerance = 1e-6)
})

test_that("jointLabelFusion(noZeroes = TRUE) does not modify the caller's mask", {
  target <- makeImage(c(12, 12), 1)
  mask <- makeImage(c(12, 12), 1)
  lab1 <- makeImage(c(12, 12), 0)
  lab2 <- makeImage(c(12, 12), 0)
  lab1[1:12, 1:8] <- 1
  lab2[1:12, 1:10] <- 2
  atlases <- list(antsImageClone(target), antsImageClone(target))
  before <- sum(as.array(mask))
  suppressWarnings(suppressMessages(capture.output(
    try(jointLabelFusion(target, mask, atlases,
      labelList = list(lab1, lab2), rad = 1, rSearch = 1, noZeroes = TRUE
    ), silent = TRUE)
  )))
  expect_equal(sum(as.array(mask)), before)
  expect_equal(before, 144)
})

test_that("random-forest predictions map to label values, or codes for text labels", {
  expect_equal(ANTsR:::.rfLabelValues(factor(c(5, 2, 5), levels = c(2, 5))), c(5, 2, 5))
  expect_equal(ANTsR:::.rfLabelValues(factor(c(0, 1), levels = c(0, 1))), c(0, 1))
  expect_equal(ANTsR:::.rfLabelValues(factor(c("WM", "GM"), levels = c("GM", "WM"))), c(2L, 1L))
})
