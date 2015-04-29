#' Average ASL tag-control pairs to estimate perfusion 
#' 
#' This function averages arterial spin labeling (ASL) functional MRI 
#' tag-control image pairs to estimate perfusion.  
#' @param asl input asl image
#' @param mask in which to calculate perfusion
#' @param nuisance nuisance covariates to include in regression
#' @param method method to use for computing average.  One of \code{sincSubtract}, 
#'  \code{simpleSubtract}, \code{regression},
#'  or \code{bayesian}. See \code{Details}. 
#' 
#' @author Kandel BM, Avants BB
#' @examples
#' nvox <- 5 * 5 * 5 * 10
#' dims <- c(5, 5, 5, 10)
#' voxvals <- array(rnorm(nvox) + 500, dim=dims)
#' voxvals[, , , 5] <- voxvals[, , , 5] + 600
#' asl <- makeImage(dims, voxvals) %>% iMath("PadImage", 2)
#' censored <- aslCensoring(asl)
#' avg <- aslAveraging(censored$asl.inlier)
#' 
#' @export 
aslAveraging <- function(asl, mask=NA,  nuisance=NA, method="regression") {
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
      cbfform <- formula(mat ~ xideal + nuisance)
    }
    mycbfmodel <- lm(cbfform)  # standard regression
    cbfi <- antsImageClone(mask)
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    cbfi[mask == 1] <- betaideal  # standard results
    avg <- antsImageClone(cbfi)
    }
  avg
}
