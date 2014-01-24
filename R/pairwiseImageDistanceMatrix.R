pairwiseImageDistanceMatrix <- function(dim, myFileList, metrictype = "PearsonCorrelation", nclusters = NA) {
  fnl <- length(myFileList)
  mymat <- matrix(rep(NA, fnl * fnl), nrow = fnl, ncol = fnl)
  tct <- 0
  for (ct in 1:fnl) {
    for (ct2 in 1:fnl) {
      if (ct != ct2) {
        i1 <- antsImageRead(myFileList[ct], dim)
        i2 <- antsImageRead(myFileList[ct2], dim)
        toutfn <- paste(tempdir(), "/Z", sep = "")
        sink(toutfn)
        mytx <- antsRegistration(fixed = i1, moving = i2, typeofTransform = c("AffineFast"), outprefix = toutfn)
        mywarpedimage <- antsApplyTransforms(fixed = i1, moving = i2, transformlist = mytx$fwdtransforms)
        sink(NULL)
        metric <- capture.output(ImageMath(dim, "j", metrictype, i1, mywarpedimage))[1]
        mymat[ct, ct2] <- (as.numeric(metric))
        tct <- tct + 1
        print(paste(100 * tct/(fnl * fnl), "%"))
      }
    }
  }
  if (metrictype == "PearsonCorrelation") {
    mymat <- mymat * (-1)  # make a dissimilarity matrix
  }
  mymat <- mymat - min(mymat, na.rm = T)  # make min zero
  symat <- (mymat + t(mymat)) * 0.5  # make symmetric 
  if (!is.na(nclusters)) {
    library(cluster)
    clusters <- rep(NA, fnl)
    clusterrep <- rep(NA, nclusters)
    pamx <- pam(symat, nclusters)
    clusters <- summary(pamx)$clustering
    for (nc in 1:nclusters) {
      wc <- c(1:fnl)[clusters == nc]
      means <- apply(symat[, wc], MARGIN = 2, mean, na.rm = T)
      clusterrep[nc] <- wc[which.min(means)]
    }
    return(list(rawMatrix = mymat, symmMatrix = symat, clusters = clusters, representatives = myFileList[clusterrep]))
  }
  return(list(rawMatrix = mymat, symmMatrix = symat))
} 
