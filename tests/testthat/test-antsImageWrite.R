
context("writing image")

test_that("image can be written with antsImageWrite", {
          img1_array <- array(rnorm(5*5), dim=c(5,5))
          img1 <- as.antsImage(img1_array, pixeltype="double")
          antsImageWrite(img1, 'output.nii.gz' )
          img2 <- antsImageRead('output.nii.gz', 2, pixeltype="double")
          expect_equal(sum(abs(as.array(img2))), sum(abs(img1_array)))
})
