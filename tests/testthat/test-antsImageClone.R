context("Cloning image")

test_that("image can be cloned", {
  img1 <- antsImageRead(getANTsRData("r16"), 2)
  img2 <- antsImageClone(img1)
  expect_true(sum(abs(as.array(img1))) == sum(abs(as.array(img2))))
})
