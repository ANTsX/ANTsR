context("segmenting image")

test_that("image can be segmented with Atropos", {
          img <- antsImageRead("data/r16slice.nii.gz", 2)
          mask <- antsImageClone(img)
          mask[img>10] <- 1
          mask[img<=10] <- 0
          segs1<-Atropos( d = img@dimension, a = img, m = "[0.2,1x1]",c = "[5,0]",  i = "kmeans[3]", x =mask)
          csf <- antsImageRead("data/csf.nii.gz")
          expect_true(mean(csf) == mean(probabilityimages[[1]])
})
