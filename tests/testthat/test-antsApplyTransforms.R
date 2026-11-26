fixed <- resampleImage(ri(1), c(64, 64), 1, 0)
moving <- resampleImage(ri(2), c(64, 64), 1, 0)
reg <- antsRegistration(fixed = fixed, moving = moving, typeofTransform = "SyN")
field <- antsImageRead(reg$fwdtransforms[1])
affine <- readAntsrTransform(reg$fwdtransforms[2])
fromFiles <- antsApplyTransforms(fixed, moving, reg$fwdtransforms)

test_that("transform objects give the same result as their files", {
  expect_equal(
    as.array(antsApplyTransforms(fixed, moving, list(field, affine))),
    as.array(fromFiles)
  )
  # mixed filenames and objects
  expect_equal(
    as.array(antsApplyTransforms(fixed, moving, list(reg$fwdtransforms[1], affine))),
    as.array(fromFiles)
  )
  # displacement field as an antsrTransform
  expect_equal(
    as.array(antsApplyTransforms(
      fixed, moving,
      list(antsrTransformFromDisplacementField(field), affine)
    )),
    as.array(fromFiles)
  )
})

test_that("linear antsrTransform objects can be inverted", {
  inverse <- antsImageRead(reg$invtransforms[2])
  expect_equal(
    as.array(antsApplyTransforms(moving, fixed, list(affine, inverse),
      whichtoinvert = c(TRUE, FALSE)
    )),
    as.array(antsApplyTransforms(moving, fixed, reg$invtransforms))
  )
})

test_that("a single object is accepted and temporary files are removed", {
  before <- list.files(tempdir())
  expect_silent(out <- antsApplyTransforms(fixed, moving, affine))
  expect_true(is.antsImage(out))
  expect_length(setdiff(list.files(tempdir()), before), 0)
})

test_that("objects can be composed into a field", {
  cfile <- antsApplyTransforms(fixed, moving, list(field, affine),
    compose = tempfile()
  )
  expect_true(file.exists(cfile))
  expect_equal(
    as.array(antsApplyTransforms(fixed, moving, antsImageRead(cfile))),
    as.array(fromFiles),
    tolerance = 1e-4
  )
})

test_that("invalid transforms give informative errors", {
  expect_error(
    antsApplyTransforms(fixed, moving, list(fixed)),
    "not a displacement field"
  )
  expect_error(
    antsApplyTransforms(fixed, moving, list(1)),
    "must be a filename"
  )
})

test_that("multi-component fixed/moving images do not reach native code", {
  vec <- mergeChannels(list(moving, moving))
  # a vector reference is used only for its geometry
  expect_equal(
    as.array(antsApplyTransforms(mergeChannels(list(fixed, fixed)), moving,
      reg$fwdtransforms)),
    as.array(fromFiles)
  )
  # a vector moving image is warped channel by channel
  for (it in c(0, 1)) {
    out <- antsApplyTransforms(fixed, vec, reg$fwdtransforms, imagetype = it)
    expect_equal(out@components, 2L)
    expect_equal(as.array(splitChannels(out)[[1]]), as.array(fromFiles))
    expect_equal(as.array(splitChannels(out)[[2]]), as.array(fromFiles))
  }
  # moving is unused when composing, so a vector moving image is fine
  cfile <- antsApplyTransforms(vec, vec, list(field, affine),
    compose = tempfile()
  )
  expect_true(file.exists(cfile))
})

test_that("temporary files are removed when a later transform is invalid", {
  before <- list.files(tempdir())
  expect_error(
    antsApplyTransforms(fixed, moving, list(field, 1)),
    "must be a filename"
  )
  expect_length(setdiff(list.files(tempdir()), before), 0)
})

test_that("composite antsrTransform objects are applied, not silently skipped", {
  img <- makeImage(c(20, 20), 0)
  img[10, 10] <- 5
  t1 <- createAntsrTransform(dimension = 2, translation = c(1, 0))
  t2 <- createAntsrTransform(dimension = 2, translation = c(2, 0))
  comp <- composeAntsrTransforms(list(t1, t2))
  h5 <- tempfile(fileext = ".h5")
  on.exit(unlink(h5))
  writeAntsrTransform(comp, h5)
  viaFile <- antsApplyTransforms(img, img, h5, interpolator = "nearestNeighbor")
  viaObject <- antsApplyTransforms(img, img, comp, interpolator = "nearestNeighbor")
  # two translations totalling three voxels move the value from x = 10 to 7
  expect_equal(unname(which(as.array(viaFile) == 5, arr.ind = TRUE)[1, 1]), 7)
  expect_equal(as.array(viaObject), as.array(viaFile))
})

test_that("native failures raise an error instead of returning the input", {
  bad <- tempfile(fileext = ".mat")
  on.exit(unlink(bad))
  writeLines("not a transform", bad)
  expect_error(
    suppressWarnings(capture.output(antsApplyTransforms(fixed, moving, bad))),
    "antsApplyTransforms failed"
  )
})
