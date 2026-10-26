test_that("multiLabelMorphology preserves labels and adds no new values", {
  a <- array(0, c(12, 12))
  a[3:6, 3:9] <- 1
  a[7:10, 3:9] <- 3
  img <- as.antsImage(a)
  for (op in c("MD", "ME", "MC", "MO")) {
    out <- as.array(multiLabelMorphology(img, op, 1))
    expect_true(all(out %in% c(0, 1, 3)), info = op)
    # original label voxels are never relabeled as another label
    expect_true(all(out[a == 1] %in% c(0, 1)), info = op)
    expect_true(all(out[a == 3] %in% c(0, 3)), info = op)
    if (op %in% c("MD", "MC")) {
      expect_true(all(out[a == 1] == 1), info = op)
      expect_true(all(out[a == 3] == 3), info = op)
    }
  }
  md <- as.array(multiLabelMorphology(img, "MD", 1))
  expect_gt(sum(md == 1), sum(a == 1))
  expect_gt(sum(md == 3), sum(a == 3))

  # dilation mask restricts growth to the mask
  m <- as.antsImage((row(a) <= 6) * 1)
  mdm <- as.array(multiLabelMorphology(img, "MD", 1, dilationMask = m))
  expect_true(all(mdm %in% c(0, 1, 3)))
  expect_true(all(mdm[a == 3] == 3))
  expect_equal(sum(mdm == 3), sum(a == 3))
})

test_that("extractImagePatchCoordinates 'all' matches extractImagePatches", {
  img <- as.antsImage(array(as.numeric(1:36), c(6, 6)))
  arr <- as.array(img)
  co <- extractImagePatchCoordinates(img, c(2, 2), "all",
    physicalCoordinates = FALSE
  )
  expect_equal(dim(co), c(25, 2))
  expect_equal(co[1, ], c(1, 1))
  pa <- extractImagePatches(img, c(2, 2), "all")
  expect_length(pa, nrow(co))
  for (k in seq_len(nrow(co))) {
    expect_equal(arr[co[k, 1] + 0:1, co[k, 2] + 0:1], pa[[k]])
  }

  img3 <- as.antsImage(array(as.numeric(1:125), c(5, 5, 5)))
  arr3 <- as.array(img3)
  co3 <- extractImagePatchCoordinates(img3, c(2, 2, 2), "all",
    strideLength = 2, physicalCoordinates = FALSE
  )
  expect_equal(dim(co3), c(8, 3))
  expect_equal(co3[1, ], c(1, 1, 1))
  pa3 <- extractImagePatches(img3, c(2, 2, 2), "all", strideLength = 2)
  expect_length(pa3, nrow(co3))
  for (k in seq_len(nrow(co3))) {
    expect_equal(
      arr3[co3[k, 1] + 0:1, co3[k, 2] + 0:1, co3[k, 3] + 0:1],
      pa3[[k]]
    )
  }

  # random patches with a mask use the same index convention
  mask <- as.antsImage(array(1, c(6, 6)))
  cr <- extractImagePatchCoordinates(img, c(2, 2), 5,
    maskImage = mask,
    randomSeed = 1, physicalCoordinates = FALSE
  )
  pr <- extractImagePatches(img, c(2, 2), 5, maskImage = mask, randomSeed = 1)
  expect_equal(nrow(cr), 5)
  for (k in seq_len(nrow(cr))) {
    expect_equal(unname(arr[cr[k, 1] + 0:1, cr[k, 2] + 0:1]), pr[[k]])
  }
})

test_that("masked patches are centered on a masked voxel (as in ANTsPy)", {
  img <- as.antsImage(array(as.numeric(1:144), c(12, 12)))
  arr <- as.array(img)
  for (p in 2:7) {
    # patch center offset from the corner, as in ANTsPy:
    # round(0.5 * patch_size) - 1
    off <- round(p / 2) - 1
    for (v in list(c(6, 6), c(1 + off, 1 + off), c(12 - p + 1 + off, 12 - p + 1 + off))) {
      m <- array(0, c(12, 12))
      m[v[1], v[2]] <- 1
      mask <- as.antsImage(m)
      pa <- extractImagePatches(img, c(p, p), 1, maskImage = mask)
      expect_length(pa, 1)
      expect_equal(dim(pa[[1]]), c(p, p))
      expect_equal(pa[[1]][off + 1, off + 1], arr[v[1], v[2]], info = p)
      co <- extractImagePatchCoordinates(img, c(p, p), 1,
        maskImage = mask, physicalCoordinates = FALSE
      )
      expect_equal(as.vector(co), v - off, info = p)
      cc <- extractImagePatchCoordinates(img, c(p, p), 1,
        maskImage = mask, physicalCoordinates = FALSE,
        cornerCoordinates = FALSE
      )
      expect_equal(as.vector(cc), v, info = p)
    }
    # a masked voxel too close to the edge for a full patch is skipped
    m <- array(0, c(12, 12))
    m[12 - p + 2 + off, 6] <- 1
    expect_length(
      extractImagePatches(img, c(p, p), 1, maskImage = as.antsImage(m)), 0
    )
  }
})

test_that("sparseDecomboot runs with a mask", {
  set.seed(1)
  mask <- makeImage(c(4, 4), 1)
  mat <- matrix(rnorm(20 * 16), nrow = 20)
  invisible(capture.output(
    res <- sparseDecomboot(mat,
      inmask = mask, sparseness = 0.25, nvecs = 2,
      its = 2, cthresh = 0, nboot = 3, doseg = FALSE
    )
  ))
  expect_equal(dim(res$eigenanatomyimages), c(2, 16))
})

test_that("ants.deterministic option is set on load", {
  expect_false(getOption("ants.deterministic"))
  old <- options(ants.deterministic = TRUE)
  on.exit(options(old))
  # .onLoad must not override a user-set value
  ANTsR:::.onLoad("", "ANTsR")
  expect_true(getOption("ants.deterministic"))
  options(ants.deterministic = NULL)
  ANTsR:::.onLoad("", "ANTsR")
  expect_false(getOption("ants.deterministic"))
})
