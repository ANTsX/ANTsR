#' Get a random mask
#'
#' Get a mask with a specific number of voxels randomly
#' distributed on the input image/mask
#'
#' @param img an antsImage, either continuous or mask-like. This will
#' define the space where to sample voxels from. If this image is
#' continous (i.e., a T1), voxels will be sampled from all nonzero
#' voxels.
#' @param nsamples the number of random voxels to select from
#' @param perLabel logical, if true the input image must be multi-label
#' and the the output will have nsamples from each label (i.e., 200
#' samples from each label)
#' @param seed random seed
#'
#' @return binary antsImage with random voxels set to 1
#'
#' @author Pustina D
#'
#' @examples
#'
#' img <- antsImageRead(getANTsRData("r16"))
#' randmask <- randomMask(img, 200)
#'
#' @export
randomMask <- function(img, nsamples, perLabel = FALSE, seed) {
  img <- check_ants(img)
  # set empty output image
  randmask <- as.antsImage(array(0, dim = dim(img)), reference = img)

  # img can be continuous, search mask is all non zeros
  if (perLabel == FALSE) {
    img[as.array(img) != 0] <- 1
  }
  if (!missing(seed)) set.seed(seed)
  # get label vector except 0
  ulabs <- sort(unique(c(as.numeric(img))))
  ulabs <- ulabs[ulabs > 0]

  for (ulab in ulabs) {
    ulabvec <- as.array(img) == as.numeric(ulab) # logical of this label
    n <- sum(ulabvec) # total available voxels for label
    k <- min(c(nsamples, n)) # reduce nsample if not enough voxels
    ulabvec[-(sample(which(ulabvec), k))] <- FALSE # k random voxels from ulabvec
    randmask[ulabvec] <- 1
  }
  return(randmask)
}
