#' Average ASL tag-control pairs to estimate perfusion
#'
#' This function averages arterial spin labeling (ASL) functional MRI
#' tag-control image pairs to estimate perfusion.
#' @param asl input asl image
#' @param mask in which to calculate perfusion
#' @param nuisance nuisance covariates to include in regression
#' @param method method to use for computing average.  One of \code{sincSubtract},
#'  \code{simpleSubtract}, \code{cubicSubtract}, \code{surroundSubtract},
#' \code{regression}, or \code{bayesian}. See \code{Details}.
#' @param ... additional parameters to pass to ASL averaging functions.
#'   See \code{Details}.
#'
#' @details
#' Two major types of methods are available for ASL signal averaging:
#' \itemize{
#'    \item{Subtraction}{: All the subtraction methods are based on subtracting
#'     the tag from control images and averaging the result.  \code{simple}
#'     subtracts adjacent tag and control images.  The other methods use
#'     interpolation to obtain a subtracted time-series.  Sinc subtraction may
#'     be marginally more accurate than cubic interpolation, but takes much
#'     longer.  Surround subtraction uses linear interpolation and is fast.}
#'    \item{Regression}{: Regression regresses the time-series on a vector
#'    of alternating tag-control dummy variables.  Regression can incorporate
#'    nuisance covariates.  Bayesian regression incorporates regularization in
#'    the regression to encourage all voxels of the same tissue type to have
#'    similar perfusion values.}
#'  }
#' For \code{bayesian}, two more arguments are required:
#' \itemize{
#'   \item{segmentation}{: a segmentation image}
#'   \item{tissuelist}{: a list of tissue probability images}
#' }
#' These would be as output from \code{atropos}; see \code{Examples} for a
#' sample usage.
#'
#' @author Kandel BM, Avants BB
#' @examples
#' nvox <- 5 * 5 * 5 * 10
#' dims <- c(5, 5, 5, 10)
#' voxvals <- array(rnorm(nvox) + 500, dim=dims)
#' asl <- makeImage(dims, voxvals) %>% iMath("PadImage", 2)
#' avg <- aslAveraging(asl)
#'
#' slice <- extractSlice(asl, 4, 4)
#' mask <-getMask(slice)
#' seg <- atropos(d=3, a=slice, x=mask, i='kmeans[6]', m='[0.0,1x1x1]')
#' bayesAvg <- aslAveraging(asl, method='bayesian',
#'   segmentation=seg$segmentation, tissuelist=seg$probabilityimages)
#'
#' @export
aslAveraging <- function(asl, mask=NA,  nuisance=NA, method="regression", ...) {
# define helper function
  bayesianPerfusion <- function(asl, mask, nuisance, segmentation, tissuelist,
   myPriorStrength=30.0,
  useDataDrivenMask=3,
  localweights=F, priorBetas=NA) {
    aslmat <- timeseries2matrix(asl, thresholdImage(segmentation, 1, Inf))
    labelfirst <- TRUE
    if (!labelfirst) {
      xideal <- (rep(c(1, 0), dim(aslmat)[1])[1:dim(aslmat)[1]] - 0.5)  # control minus tag
    } else {
      xideal <- (rep(c(0, 1), dim(aslmat)[1])[1:dim(aslmat)[1]] - 0.5)  # tag minus control
    }
    perfdf<-data.frame( xideal=xideal,
                nuis=nuisance)
    perfdf<-perfdf[,!is.na(colMeans(perfdf))]
    perfmodel<-lm(aslmat ~ perfdf)
    getpriors<-function(img, segmentation) {
      n <- max(segmentation)
      p <- rep(0,n)
      segvec <- (segmentation[segmentation > 0])
      for (i in 1:n) {
        p[i]<-median(img[segvec == as.numeric(i)])
      }
      return(p)
    }
    if (all(is.na(priorBetas)))  {
      blm<-bigLMStats(perfmodel, includeIntercept=T)
      bayespriormatfull<-blm$beta
    } else {
      bayespriormatfull<-priorBetas
    }
    n <- max(segmentation) * nrow(bayespriormatfull)
    bayespriormat <- matrix(rep(0, n), nrow=max(segmentation))
    for(i in 1:ncol(bayespriormat)) {
      bayespriormat[, i] <- getpriors(bayespriormatfull[i, ], segmentation)
    }
#   set 4 to equal 2 - dgm = gm
    bayespriormat[4, ] <- bayespriormat[2, ]
#   set csf to zero perfusion
    bayespriormat[1, 2] <- 0
    X <- model.matrix(perfmodel)
    localtissuemat <- imageListToMatrix(tissuelist, mask)
    priorwt <- diag(ncol(bayespriormat)) * myPriorStrength
    if (ncol(priorwt) > 2) {
      priorwt[3:ncol(priorwt), 3:ncol(priorwt)] <- 0
    }
    bayesianperfusionloc <- localtissuemat * 0
    bayesianperfusionlocp <- localtissuemat * 0
    for (i in 1:ncol(aslmat)) {
    # here is where we get really bayesian
    # average over all tissue models ...
      localtissuemat[,i]<-abs(localtissuemat[,i]) / sum(abs(localtissuemat[,i]))
      for ( segval in 1:max(segmentation) ) {
        tissueprior<-localtissuemat[segval,i]
        localprior<-bayespriormat[segval,]
        blm<-bayesianlm(  X, aslmat[,i], localprior, priorwt,
           includeIntercept=T)
        locbeta<-blm$beta[2]
        bayesianperfusionloc[segval,i]<-locbeta
        bayesianperfusionlocp[segval,i]<-locbeta*tissueprior
      }
    }
    bperfimg <- makeImage(mask,colSums(bayesianperfusionlocp))
    bperfimg
  }

  if (length(grep("Subtract", method)) > 0) {
    avg <- .Call("timeSeriesSubtraction", asl, method)
  } else if (method == "regression"){
    labelfirst <- TRUE
    if (is.na(mask)){
      myar <- apply(as.array(asl), c(1, 2, 3), mean)
      img <- makeImage(dim(myar), myar)
      antsSetSpacing(img, antsGetSpacing(asl)[1:3])
      antsSetOrigin(img, antsGetOrigin(asl)[1:3])
      antsSetDirection(img, antsGetDirection(asl)[1:3, 1:3])
      mask <- getMask(img)
    }
    ts <- timeseries2matrix(asl, mask)
    if (!labelfirst) {
      xideal <- (rep(c(1, 0), dim(ts)[1])[1:dim(ts)[1]] - 0.5)  # control minus tag
    } else {
      xideal <- (rep(c(0, 1), dim(ts)[1])[1:dim(ts)[1]] - 0.5)  # tag minus control
    }
    cbfform <- formula(ts ~ xideal)
    if (!all(is.na(nuisance))) {
      cbfform <- formula( ts ~ xideal + nuisance)
    }
    mycbfmodel <- lm(cbfform)  # standard regression
    cbfi <- antsImageClone(mask)
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    cbfi[mask == 1] <- betaideal  # standard results
    avg <- antsImageClone(cbfi)
  } else if (method == 'bayesian') {
     if (is.na(mask)){
      myar <- apply(as.array(asl), c(1, 2, 3), mean)
      img <- makeImage(dim(myar), myar)
      antsSetSpacing(img, antsGetSpacing(asl)[1:3])
      antsSetOrigin(img, antsGetOrigin(asl)[1:3])
      antsSetDirection(img, antsGetDirection(asl)[1:3, 1:3])
      mask <- getMask(img)
     }
     avg <- bayesianPerfusion(asl, mask, nuisance, ...)
  }
  if (mean(avg) < 0) {
    avg <- -avg
  }
  avg
}
