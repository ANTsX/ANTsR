context("antsrMotionCalculation consistent results")

set.seed(120)
simimg<-makeImage(rep(5,4), rnorm(5^4))
testthat::expect_equal(mean(simimg), 0.0427369860965759)
res = antsrMotionCalculation( simimg, num_threads = 1, seed = 1234  )
res2 = antsrMotionCalculation( simimg, num_threads = 1, seed = 1234 )
res3 = antsrMotionCalculation( simimg, num_threads = 1, seed = 1 )


test_that(
	"antsrMotionCalculation gives same result", {
	testthat::expect_equal(res, res2)
})


test_that(
	"antsrMotionCalculation gives different result w/diff seed", {
  testthat::expect_failure(testthat::expect_equal(res, res3))
})


