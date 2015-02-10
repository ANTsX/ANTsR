context("Reading image")

testthat("image can be read", {
         img<-antsImageRead( getANTsRData("r16"), 2 )
         pixeltype = "float"
         expect_match(img@pixeltype, pixeltype)
})
