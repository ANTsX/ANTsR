library(ANTsRCore)
set.seed(1234)
dims <- c(30, 30)
n <- prod(dims)
r <- runif(n, min = 0, max = 255)
g <- runif(n, min = 0, max = 255)
b <- runif(n, min = 0, max = 255)
sums <- (r + g + b)
r <- floor(r / sums * 255)
g <- floor(g / sums * 255)
b <- floor(b / sums * 255)
dim(r) <- dims
dim(g) <- dims
dim(b) <- dims
r <- as.antsImage(r)
g <- as.antsImage(g)
b <- as.antsImage(b)
multi_component_image <- mergeChannels(list(r, g, b))
antsImageWrite(multi_component_image, "inst/extdata/multi_component_image.nii.gz")
