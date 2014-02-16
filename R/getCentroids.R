getCentroids <- function(img, clustparam = 250, threshparam = NA, outprefix = NA) {
  if (nargs() == 0 | missing(img)) {
    print(args(getCentroids))
    return(1)
  }
  if (class(img)[[1]] != "antsImage") {
    print("  class(img)[[1]] != antsImage ")
  }
  imagedim <- img@dimension
  if (is.na(outprefix)) {
    outprefix <- paste(tempdir(), "/Z", sep = "")
  }
  pointfile <- paste(outprefix, "coords.csv", sep = "")
  clust <- NA
  if ( ! is.na( threshparam ) ) {
    threshimg <- antsImageClone(img)
    ImageMath(threshimg@dimension, threshimg, "abs", threshimg)
    meanval <- mean(threshimg[threshimg > (.Machine$double.eps * 2)])
    sdval <- sd(threshimg[threshimg > (.Machine$double.eps * 2)])
    threshval <- (meanval - sdval * threshparam)
    if (threshval < (.Machine$double.eps * 2)) 
        threshval <- (.Machine$double.eps * 2)
    threshimg[threshimg > threshval] <- 1
    threshimg[threshimg <= threshval] <- 0
    clust <- labelClusters(threshimg, clustparam)
    ImageMath(imagedim, pointfile, "LabelStats", clust, clust, 1)
  } else ImageMath(imagedim, pointfile, "LabelStats", img, img, clustparam)
  mypoints <- read.csv(pointfile)
  scl <- 10
  mypoints$x <- round(mypoints$x * scl)/scl
  mypoints$y <- round(mypoints$y * scl)/scl
  mypoints$z <- round(mypoints$z * scl)/scl
  centroids <- as.matrix(data.frame(x = mypoints$x, y = mypoints$y, z = mypoints$z))
  return(list(centroids = centroids, clustimg = clust))
  ########################## 
} 
