context("Cropping and plotting non-float images")

test_that("cropImage works on non-float images and keeps the pixeltype", {
  img <- antsImageRead(getANTsRData("r16"))
  mask <- getMask(img)
  for (pt in c("unsigned char", "unsigned int", "double")) {
    x <- antsImageClone(img, pt)
    cropped <- cropImage(x, antsImageClone(mask, "unsigned char"))
    expect_equal(cropped@pixeltype, pt)
    expect_equal(dim(cropped), dim(cropImage(img, mask)))
    expect_equal(
      as.array(cropped),
      as.array(antsImageClone(cropImage(x %>% antsImageClone("float"), mask), pt))
    )
  }
})

test_that("cropIndices, decropImage and extractSlice accept non-float images", {
  img <- antsImageClone(antsImageRead(getANTsRData("r16")), "unsigned char")
  cropped <- cropIndices(img, c(10, 10), c(100, 100))
  expect_equal(cropped@pixeltype, "unsigned char")
  expect_equal(dim(cropped), c(91, 91))
  decropped <- decropImage(cropped, img)
  expect_equal(decropped@pixeltype, "unsigned char")
  expect_equal(dim(decropped), dim(img))

  vol <- antsImageClone(makeImage(c(5, 5, 5), 1:125), "unsigned int")
  slice <- extractSlice(vol, 2, 3)
  expect_equal(slice@pixeltype, "unsigned int")
  expect_equal(as.array(slice), as.array(vol)[, , 2])
})

test_that("plot works with integer images and mask overlays", {
  skip_if_not_installed("pixmap")
  img <- antsImageRead(getANTsRData("r16"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_error(plot(img, img > 100), NA)
  expect_error(plot(antsImageClone(img, "unsigned char")), NA)
})
