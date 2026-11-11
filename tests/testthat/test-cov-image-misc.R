test_that("getANTsRData returns bundled files", {
  expect_output(expect_null(getANTsRData()), "usage")
  ids <- getANTsRData("show")
  expect_true(all(c("r16", "r64", "mni", "multi_component_image") %in% ids))
  for (id in c("r16", "r27", "r30", "r62", "r64", "r85", "simple", "rand", "multi_component_image")) {
    fn <- getANTsRData(id)
    expect_true(file.exists(fn), info = id)
  }
  pop <- getANTsRData("population")
  expect_length(pop, 3)
  expect_equal(dim(pop[[1]]), c(256, 256))
  expect_error(getANTsRData("not_an_id"), "no data with that id")
})

test_that("rapidlyInspectImageData summarises files", {
  skip_if_not_installed("moments")
  set.seed(1)
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  fns <- file.path(td, c("a.nii.gz", "b.nii.gz"))
  antsImageWrite(makeImage(c(10, 10), rnorm(100) + 5), fns[1])
  antsImageWrite(mergeChannels(list(
    makeImage(c(10, 10), rnorm(100) + 5),
    makeImage(c(10, 10), rnorm(100) + 5)
  )), fns[2])
  m <- rapidlyInspectImageData(fns)
  expect_equal(dim(m), c(2, 6))
  expect_equal(colnames(m), c("mean1", "mean2", "sd1", "sd2", "kurt1", "kurt2"))
  expect_equal(rownames(m), fns)
  expect_true(all(m[, "mean2"] > m[, "mean1"]))
  expect_output(expect_equal(rapidlyInspectImageData(), 1))
})

test_that("basicInPaint fills a lesion", {
  set.seed(2)
  img <- makeImage(c(20, 20), runif(400))
  pm <- img * 0 + 1
  pm[8:12, 8:12] <- 2
  bi <- basicInPaint(img, pm)
  expect_equal(dim(bi), c(20, 20))
  # voxels away from the lesion are untouched
  expect_equal(as.array(bi)[1:3, 1:3], as.array(img)[1:3, 1:3])
  bi2 <- basicInPaint(img, pm, speedimage = img + 1, its = 2)
  expect_equal(dim(bi2), c(20, 20))
  expect_output(expect_equal(basicInPaint(), 1))
})

test_that("labelImageCentroids, getCenterOfMass and reorientImage", {
  lab <- makeImage(c(8, 8, 8), 0)
  lab[2:3, 2:3, 2:3] <- 1
  lab[5:7, 5:7, 5:7] <- 2
  invisible(capture.output(lc <- labelImageCentroids(lab)))
  expect_equal(lc$labels, c(1, 2))
  expect_equal(unname(lc$vertices[2, ]), c(6, 6, 6))
  invisible(capture.output(lc2 <- suppressWarnings(labelImageCentroids(lab, physical = TRUE, convex = FALSE))))
  expect_equal(dim(lc2$vertices), c(2, 3))
  expect_output(expect_equal(labelImageCentroids(makeImage(c(5, 5), 1)), 1), "3D")

  img3 <- makeImage(c(10, 10, 10), 0)
  img3[3:8, 4:6, 4:7] <- 1
  img3 <- smoothImage(img3, 1)
  expect_equal(ANTsR:::getCenterOfMass(img3), c(4.5, 4, 4.5))
  expect_output(expect_true(is.na(ANTsR:::getCenterOfMass(antsImageClone(img3, "unsigned char")))))
  expect_output(expect_true(is.na(reorientImage(antsImageClone(img3, "unsigned char"), c(1, 0, 0)))))
  # package bugs: reorientImageR does not write the transform file, and a
  # vector axis2 breaks the is.na() check
  expect_error(suppressWarnings(reorientImage(img3, c(1, 0, 0))), "does not exist")
  expect_error(reorientImage(img3, c(1, 0, 0), c(0, 1, 0)), "length")
})

test_that("save.ANTsR and load.ANTsR round trip", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  en <- new.env()
  assign("im", makeImage(c(4, 4), 1:16), en)
  assign("v", 1:3, en)
  fn <- file.path(td, "a.txt")
  writeLines("hi", fn)
  assign("f", fn, en)
  assign("lst", list(makeImage(c(2, 2), 1), "x"), en)
  sd <- file.path(td, "sess")
  save.ANTsR(sd, env = en)
  expect_true(file.exists(file.path(sd, "ANTSLOAD.Rdata")))
  expect_equal(sum(grepl("nii.gz$", list.files(sd))), 2)
  expect_error(save.ANTsR(sd, env = en), "overwrite is false")
  save.ANTsR(sd, env = en, overwrite = TRUE)
  save.ANTsR(file.path(td, "s2"), objects = c("im", "v"), env = en, clonediskfiles = FALSE)
  expect_true(file.exists(file.path(td, "s2", "ANTSLOAD.Rdata")))

  e2 <- new.env()
  load.ANTsR(sd, env = e2)
  expect_s4_class(e2$im, "antsImage")
  expect_equal(as.array(e2$im), as.array(en$im))
  expect_equal(e2$v, 1:3)
  expect_equal(readLines(e2$f), "hi")
  expect_s4_class(e2$lst[[1]], "antsImage")
  e3 <- new.env()
  load.ANTsR(file.path(td, "s2"), env = e3)
  expect_true(all(c("im", "v") %in% ls(e3)))
  expect_false("f" %in% ls(e3))
})

test_that("write/readNormalizedPopulationData (directory and hdf5)", {
  set.seed(3)
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  demog <- data.frame(age = 1:3)
  mask <- makeImage(c(3, 3), 0)
  mask[1:2, 1:2] <- 1
  mat <- matrix(rnorm(8), 2, 4)
  d1 <- file.path(td, "pop")
  expect_true(writeNormalizedPopulationData(demog, mat, mask, c(TRUE, FALSE, TRUE), d1))
  r <- readNormalizedPopulationData(d1)
  expect_equal(r$imageMat, mat, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(r$imageBoolean, c(TRUE, FALSE, TRUE))
  expect_equal(as.array(r$imageMask), as.array(mask))
  expect_equal(r$demographics$age, 1:3)
  if (requireNamespace("hdf5r", quietly = TRUE)) {
    h5 <- file.path(td, "pop.h5")
    expect_true(writeNormalizedPopulationData(demog, mat, mask, c(TRUE, FALSE, TRUE), h5))
    r2 <- readNormalizedPopulationData(h5)
    expect_equal(r2$imageMat, mat, tolerance = 1e-6)
    expect_equal(r2$imageBoolean, c(TRUE, FALSE, TRUE))
  }
  expect_error(writeNormalizedPopulationData(demog, mat, mask, c(TRUE, FALSE, TRUE), d1), "file.exists")
  expect_error(writeNormalizedPopulationData(demog, mat[, 1:3], mask, c(TRUE, FALSE, TRUE), d1), "ncol")
  expect_error(writeNormalizedPopulationData(demog, mat, mask, c(TRUE, TRUE, TRUE), d1), "nrow")
  expect_error(writeNormalizedPopulationData(demog, mat, mask, c(TRUE, TRUE), d1), "nrow\\( demographics")
  expect_error(readNormalizedPopulationData(file.path(td, "nope")), "does not exist")
})

test_that("subjectDataToGroupDataFrame stacks csv files", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  c1 <- file.path(td, "c1.csv")
  c2 <- file.path(td, "c2.csv")
  c3 <- file.path(td, "c3.csv")
  write.csv(data.frame(Volume = 1:3, b = 4:6), c1, row.names = FALSE)
  write.csv(data.frame(Volume = 7:9, b = 4:6), c2, row.names = FALSE)
  write.csv(data.frame(Volume = 1:2, b = 4:5), c3, row.names = FALSE)
  df <- subjectDataToGroupDataFrame(c(c1, c2), "Volume")
  expect_equal(colnames(df), paste0("Volume", 1:3))
  expect_equal(unname(unlist(df[2, ])), 7:9)
  expect_equal(rownames(df), c(c1, c2))
  expect_output(df2 <- subjectDataToGroupDataFrame(c(c1, c2, c3), 1, mycolname = "V", datarownames = c("x", "y", "z")), "different")
  expect_equal(colnames(df2), c("Vx", "Vy", "Vz"))
  expect_true(all(is.na(df2[3, ])))
  expect_output(expect_equal(subjectDataToGroupDataFrame(), 1), "Usage")
  # package bugs: NA file entries break the row names; an unknown column
  # name gives a zero-length condition instead of the intended message
  expect_error(suppressWarnings(capture.output(subjectDataToGroupDataFrame(c(c1, NA), 1))), "row.names")
  expect_error(subjectDataToGroupDataFrame(c1, "nope"), "length zero")
})

test_that("exemplarInpainting regression and lesion modes", {
  set.seed(4)
  img <- makeImage(c(30, 30), 0)
  img[5:25, 5:25] <- runif(441, 1, 2)
  img <- smoothImage(img, 1)
  mask <- getMask(img, 0.1, Inf, 0)
  mask2 <- antsImageClone(mask)
  mask2[12:16, 12:16] <- 2
  il <- list(makeImage(c(30, 30), rnorm(900)), makeImage(c(30, 30), rnorm(900)))
  p1 <- exemplarInpainting(img, mask, il, featureRadius = 1)
  expect_equal(dim(p1), c(30, 30))
  invisible(capture.output(p2 <- exemplarInpainting(img, mask2, il, featureRadius = 1, sharpen = TRUE, debug = TRUE)))
  expect_equal(dim(p2), c(30, 30))
  # outside of the painted region the image is unchanged
  expect_equal(as.array(p2)[1:3, 1:3], as.array(img)[1:3, 1:3])
  invisible(capture.output(p3 <- exemplarInpainting(img, mask2, il[1], scaleInpaintIntensity = 1, debug = TRUE)))
  expect_equal(dim(p3), c(30, 30))
  skip_if_not_installed("e1071")
  p4 <- exemplarInpainting(img, mask2, il, featureRadius = 1, predalgorithm = "svm")
  expect_equal(dim(p4), c(30, 30))
})

test_that("atropos with multiple channels and priors", {
  set.seed(5)
  img <- makeImage(c(16, 16), rep(c(1, 5, 9, 13), each = 64) + rnorm(256, sd = .3))
  mask <- img * 0 + 1
  ch <- lapply(1:6, function(i) img + makeImage(c(16, 16), rnorm(256, sd = 1)))
  for (n in 2:6) {
    a <- atropos(ch[1:n], mask)
    expect_equal(sort(unique(c(as.array(a$segmentation)))), c(1, 2, 3), info = n)
    expect_length(a$probabilityimages, 3)
  }
  expect_error(atropos(rep(ch, 2)[1:7], mask), "more than 6")
  a1 <- atropos(img, mask)
  ap <- atropos(img, mask, i = a1$probabilityimages, priorweight = 0.5)
  expect_length(ap$probabilityimages, 3)
  expect_equal(sort(unique(c(as.array(ap$segmentation)))), c(1, 2, 3))
})

test_that("jointLabelFusion with and without labels", {
  set.seed(6)
  tgt <- makeImage(c(16, 16), 0)
  tgt[5:12, 5:12] <- 1
  tgt <- smoothImage(tgt, 1) * 100
  lab <- thresholdImage(tgt, 50, Inf)
  lab2 <- lab * 2
  lab2[lab2 == 0] <- 1
  atl <- list(tgt + makeImage(c(16, 16), rnorm(256)), tgt + makeImage(c(16, 16), rnorm(256)))
  tm <- tgt * 0 + 1
  invisible(capture.output(j1 <- suppressWarnings(jointLabelFusion(tgt, tm, atl, labelList = list(lab * 2, lab * 2), rad = 1, rSearch = 1))))
  expect_named(j1, c("segmentation", "intensity", "probabilityimages", "segmentationNumbers", "jlfmask"))
  expect_equal(j1$segmentationNumbers, 2)
  expect_equal(as.array(j1$segmentation), as.array(lab * 2))
  invisible(capture.output(j2 <- suppressWarnings(jointLabelFusion(tgt, tm, atl,
    labelList = list(lab2, lab2),
    rad = c(1, 1), rSearch = 1, maxLabelPlusOne = TRUE, noZeroes = TRUE,
    nonnegative = TRUE
  ))))
  expect_true(all(c("segmentation_raw", "backgroundProb") %in% names(j2)))
  j3 <- suppressWarnings(jointLabelFusion(tgt, tm, atl, rad = 1, rSearch = 1))
  expect_s4_class(j3, "antsImage")
  expect_gt(cor(c(as.array(j3)), c(as.array(tgt))), 0.9)
  expect_error(jointLabelFusion(tgt, tm, atl, labelList = list(lab), rad = 1), "length\\(labelList\\)")
  expect_error(
    jointLabelFusion(tgt, tm, atl, labelList = list(lab, lab), rad = c(1, 1, 1)),
    "patch radius"
  )
})
