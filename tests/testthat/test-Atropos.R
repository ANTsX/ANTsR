context("segmenting image")

test_that("image can be segmented with atropos", {
  img <- antsImageRead(getANTsRData("r16"), 2)
  mask <- antsImageClone(img)
  mask[img > 10] <- 1
  mask[img <= 10] <- 0
  nvox <- sum(as.array(mask))
  segs1 <- atropos(
    d = img@dimension, a = img, m = "[0.2,1x1]",
    c = "[5,0]", i = "kmeans[3]", x = mask
  )
  testcsf <- abs(sum(segs1$segmentation == 1) - 2330) < 100
  testgm <- abs(sum(segs1$segmentation == 2) - 7333) < 100
  testwm <- abs(sum(segs1$segmentation == 3) - 8518) < 100
  expect_true(testcsf & testgm & testwm)
})
# todo : other segmentation approaches of atropos
