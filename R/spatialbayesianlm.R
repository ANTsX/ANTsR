#' spatially constrained bayesian regression function.
#'
#' Take a standard lm result and use bayesian regression to impose spatial
#' regularity.
#'
#'
#' @param mylm standard lm result of the form mylm<-lm(ymat~.)
#' @param ymat outcome matrix - usually from imaging data
#' @param mask mask with non-zero entries n-columns of ymat
#' @param smth smoothness parameter
#' @param priorWeight weight on the prior
#' @param nhood size of neighborhood
#' @param regweights weights on rows - size of ymat
#' @param smoothcoeffmat prior coefficient matrix
#' @return bayesian regression solution is output as a list of images
#' @author Avants BB
#' @examples
#'
#'   # make some simple data
#'   \dontrun{
#'   fn<-"PEDS012_20131101_pcasl_1.nii.gz"
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#'   asl<-antsImageRead(fn,4)
#'   tr<-antsGetSpacing(asl)[4]
#'   aslmean<-getAverageOfTimeSeries( asl )
#'   aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
#'   pcaslpre <- aslPerfusion( asl, interpolation="linear",
#'   dorobust=0, useDenoiser=NA, skip=1, useBayesian=0,
#'   moreaccurate=0, verbose=T, mask=aslmask ) # throw away lots of data
#'   # user might compare to useDenoiser=FALSE
#'   pcasl.parameters <- list( sequence="pcasl", m0=pcaslpre$m0 )
#'   aslmat<-timeseries2matrix(asl,aslmask)
#'   tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
#'   dv<-computeDVARS(aslmat)
#'   perfmodel<-lm( aslmat ~ tc + stats::poly(dv,4) ) # standard model
#'   ssp<-spatialbayesianlm( perfmodel, aslmat, aslmask,
#'     priorWeight=1.e2 ,smth=1.6, nhood=rep(2,3) )
#'   plot( ssp[[1]], slices="2x16x2", axis=3 )
#'   }
#'
#' @export spatialbayesianlm
spatialbayesianlm <- function(mylm, ymat, mask, smth = 1, priorWeight = 1, nhood = NA,
  regweights = NA, smoothcoeffmat = NA) {
  if (sum(mask == 1) != ncol(ymat)) {
    print("spatialbayesianlm: mask does not match ymat dimensions")
    return(NA)
  }
  if (all(is.na(nhood)))
    nhood <- rep(1, mask@dimension)
  if (all(is.na(regweights))) {
    regweights <- ymat
    regweights[] <- 1
  }
  if (is.null(dim(regweights))) {
    regweights <- ymat
    regweights[] <- 1
  }
  if (!all(dim(regweights) == dim(ymat))) {
    regweights <- ymat
    regweights[] <- 1
  }
  if (all(is.na(smoothcoeffmat)))
    smoothcoeffmat <- mylm$coefficients
  nmatimgs <- list()
  for (i in 1:nrow(smoothcoeffmat)) {
    temp <- antsImageClone(mask)
    temp[mask == 1] <- smoothcoeffmat[i, ]
    temp<-smoothImage(temp, smth)
    nmatimgs[[i]] <- antsGetNeighborhoodMatrix(temp, mask, nhood, boundary.condition = "mean")
    smoothcoeffmat[i, ] <- temp[mask == 1]
  }
  covmat <- cov(t(smoothcoeffmat))
  invcov <- solve(covmat + diag(ncol(covmat)) * 1e-06)
  betaideal <- rep(0, ncol(ymat))
  blmX <- model.matrix(mylm)
  betamatrix <- ymat[1:(ncol(blmX) - 1), ] * 0
  posteriorI <- rep(0, ncol(ymat))
  for (v in 1:ncol(ymat)) {
    parammat <- nmatimgs[[1]][, v]
    for (k in 2:length(nmatimgs)) parammat <- cbind(parammat, nmatimgs[[k]][,
      v])
    pcov <- cov(parammat)
    locinvcov <- tryCatch(solve(pcov), error = function(e) return(invcov))
    if (typeof(locinvcov) == "character")
      locinvcov <- invcov
    prior <- (smoothcoeffmat[, v])
    blm <- bayesianlm(blmX, ymat[, v], prior, locinvcov * priorWeight, regweights = regweights[,
      v])
    betamatrix[, v] <- blm$beta
    posteriorI[v] <- blm$posteriorProbability
  }
  mylist <- matrixToImages(betamatrix, mask)
  mylist <- lappend(mylist, makeImage(mask, posteriorI))
  return(mylist)
}
