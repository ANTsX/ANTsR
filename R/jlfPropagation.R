#' Propagate Joint Label Fusion (JLF) solutions through a time series
#'
#' The JLF initialization is reused throughout a time series with an additional
#' lag argument.
#'
#' @param targetI antsImage list to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages with intensity images
#' @param rad neighborhood radius, default to 2
#' @param labelList optional list containing antsImages with segmentation labels
#' @param rSearch radius of search, default is 3
#' @param lagValue number of prior images to use to fwd propagate JLF solution
#' @param ... arguments to pass to \code{\link{jointLabelFusion}}
#' @param verbose boolean
#' @return segmentation of time series
#' @author Brian B. Avants
#' @keywords fusion, template
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' ref <- ri(1) %>%
#'   resampleImage(4) %>%
#'   iMath("Normalize")
#' mi1 <- ri(2)
#' mi2 <- ri(3)
#' mi3 <- ri(4)
#' mi4 <- ri(5)
#' mi5 <- ri(6)
#' refmask <- getMask(ref)
#' refmask <- iMath(refmask, "MD", 10) # just to speed things up
#' ilist <- list(mi1, mi2, mi3, mi4, mi5)
#' seglist <- list()
#' for (i in 1:length(ilist))
#' {
#'   ilist[[i]] <- iMath(ilist[[i]], "Normalize")
#'   mytx <- antsRegistration(
#'     fixed = ref, moving = ilist[[i]],
#'     typeofTransform = c("Affine"), verbose = TRUE
#'   )
#'   mywarpedimage <- antsApplyTransforms(
#'     fixed = ref,
#'     moving = ilist[[i]],
#'     transformlist = mytx$fwdtransforms
#'   )
#'   ilist[[i]] <- mywarpedimage
#'   seg <- thresholdImage(ilist[[i]], "Otsu", 3)
#'   seglist[[i]] <- seg + 1
#' }
#' tarlist <- list(
#'   iMath(ref, "GD", 3),
#'   iMath(ref, "GD", 2),
#'   iMath(ref, "GD", 1),
#'   iMath(ref, "GD", 0),
#'   iMath(ref, "GE", 1),
#'   iMath(ref, "GE", 2),
#'   iMath(ref, "GE", 3)
#' )
#' pp <- jlfProp(tarlist, refmask, ilist,
#'   rSearch = 2,
#'   labelList = seglist, rad = rep(2, length(dim(ref)))
#' )
#' }
jlfProp <- function(
    targetI,
    targetIMask,
    atlasList,
    rad = 2,
    labelList = NULL,
    rSearch = 3,
    lagValue = 3,
    verbose = FALSE,
    ...) {
  # #' @param constrain weights to be non-negative
  nonnegative <- TRUE
  # algorithm:
  #  1. compute JLF at time point1
  newAtlas <- list()
  newLabs <- list()
  for (k in 1:lagValue) {
    if (verbose) print(paste0("Pre: Lag ", k))
    jlfk <- jointLabelFusion(
      targetI = targetI[[k]],
      targetIMask = targetIMask,
      atlasList = atlasList,
      rad = rad,
      labelList = labelList,
      rSearch = rSearch, ..., verbose = verbose
    )
    newAtlas[[k]] <- targetI[[k]]
    newLabs[[k]] <- jlfk$segmentation
  }
  #  2. compute JLF at t+k including the result from (t+k-lagValue) as atlas
  for (k in (lagValue + 1):length(targetI)) {
    if (verbose) print(paste0("Prop: Lag ", k, " of ", length(targetI)))
    jlfk <- jointLabelFusion(
      targetI = targetI[[k]],
      targetIMask = targetIMask,
      atlasList = newAtlas[(k - lagValue):(k - 1)],
      rad = rad,
      labelList = newLabs[(k - lagValue):(k - 1)],
      rSearch = rSearch, ...,
      verbose = verbose
    )
    newAtlas[[k]] <- targetI[[k]]
    newLabs[[k]] <- jlfk$segmentation
  }
  return(newLabs)
}
