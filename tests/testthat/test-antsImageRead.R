context("Reading image")

test_that("image can be read", {
  img <- antsImageRead(getANTsRData("r16"), 2)
  expect_true(sum(abs(as.array(img))) == 3307036)
})
