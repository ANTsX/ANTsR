context("antsrMotion_estimation")

test_that(".motion_correction gives same result", {
  set.seed(1000)
  testimg <- makeImage(c(10, 10, 10, 5),  rnorm(5000))
  testimg <- iMath(testimg, "PadImage", 5)
  mocorr <- .motion_correction(testimg,
                               num_threads = 1, seed = 1234)
  mocorr2 <- .motion_correction(testimg, num_threads = 1,
                                seed = 1234)
  testthat::expect_equal(mocorr, mocorr2)
  
  mocorr3 <- .motion_correction(testimg, num_threads = 1,
                                seed = 2332)
  testthat::expect_failure(testthat::expect_equal(mocorr, mocorr3))
})
