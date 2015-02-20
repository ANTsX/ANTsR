context("segmenting image")

test_that("image can be segmented with atropos", {
          img <- antsImageRead( getANTsRData( "r16" ) , 2)
          mask <- antsImageClone(img)
          mask[img>10] <- 1
          mask[img<=10] <- 0
          nvox <- sum(as.array(mask))
          segs1<-atropos( d = img@dimension, a = img, m = "[0.2,1x1]",c = "[5,0]",  i = "kmeans[3]", x =mask)
          seg <- antsImageRead(getANTsRData( "atroposseg" ), 2)
          # condition for passing is that less than 0.1% of voxels are classified
          # differently from our standard to allow for randomness in initialization.
          expect_true(
           sum(abs(as.array(seg) - as.array(segs1$segmentation))) < nvox*1e-1)

})
#todo : other segmentation approaches of atropos
