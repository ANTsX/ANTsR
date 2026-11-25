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
