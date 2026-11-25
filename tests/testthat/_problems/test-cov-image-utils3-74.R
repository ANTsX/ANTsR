# Extracted from test-cov-image-utils3.R:74

# test -------------------------------------------------------------------------
set.seed(2)
img <- makeImage(c(8, 8), 1:64)
i2 <- makeImage(c(8, 8), runif(64))
mi <- antsImageMutualInformation(img, i2)
mi_self <- antsImageMutualInformation(img, img, 0.5, 16)
expect_lt(mi_self, mi)
