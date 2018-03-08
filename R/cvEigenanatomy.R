#' Cross-validation method for eigenanatomy decompositions.
#' 
#' Perform cross-validation on an image set using eigencomponents to predict an
#' outcome variable.
#' 
#' 
#' @param demog Demographics information that includes outcome and (optional)
#' covariates.
#' @param images n by p input image matrix, where n is the number of subjects
#' and p is the number of voxels.
#' @param outcome Name of outcome variable. Must be present in \code{demog}.
#' @param ratio If greater than 1, number of folds for cross-validation.  If
#' less than 1, one testing-training step will be performed, using \code{ratio}
#' of the data for training and the rest for testing.
#' @param mask Mask image of type \code{antsImage}.
#' @param sparseness Desired level of sparsity in decomposition.
#' @param nvecs Number of eigenvectors to use in decomposition.
#' @param its Number of iterations for decomposition.
#' @param cthresh Cluster threshold for decomposition.
#' @param ... Additional options passed to \code{regressProjections}.
#' @return A result, or (if ratio > 1) list of results, from
#' \code{regressProjection}.
#' @author Kandel BM and Avants B
#' @examples
#' 
#' \dontrun{
#' # generate simulated outcome
#' nsubjects <- 100
#' x1 <- seq(1, 10, length.out=nsubjects) + rnorm(nsubjects, sd=2)
#' x2 <- seq(25, 15, length.out=nsubjects) + rnorm(nsubjects, sd=2)
#' outcome <- 3 * x1 + 4 * x2 + rnorm(nsubjects, sd=1)
#' # generate simulated images with outcome predicted 
#' # by sparse subset of voxels
#' voxel.1 <- 3 * x1 + rnorm(nsubjects, sd=2)
#' voxel.2 <- rnorm(nsubjects, sd=2)
#' voxel.3 <- 2 * x2 + rnorm(nsubjects, sd=2)
#' voxel.4 <- rnorm(nsubjects, sd=3)
#' input   <- cbind(voxel.1, voxel.2, voxel.3, voxel.4)
#' mask    <- as.antsImage(matrix(c(1,1,1,1), nrow=2))
#' # generate sample demographics that do not explain outcome
#' age <- runif(nsubjects, 50, 75)
#' demog <- data.frame(outcome=outcome, age=age)
#' result <- cvEigenanatomy(demog, input, 'outcome', ratio=5, mask, 
#'             sparseness=0.25, nvecs=4) 
#' }
#' 
#' @export cvEigenanatomy
cvEigenanatomy <- function(demog, images, outcome, ratio = 10, mask = NA, sparseness = 0.01, 
  nvecs = 50, its = 5, cthresh = 250, ...) {
  if (ratio < 1) {
    demog.split <- splitData(demog, ratio, return.rows = TRUE)
    mydecom <- sparseDecom(images[demog.split$rows.in, ], mask, sparseness, nvecs, 
      its, cthresh)
    result <- regressProjections(images[demog.split$rows.in, ], images[demog.split$rows.out, 
      ], demog.split$data.in, demog.split$data.out, mydecom$eigenanatomyimages, 
      mask, outcome, ...)
  } else {
    demog.split <- splitData(demog, ratio, return.rows = TRUE)
    result <- list()
    for (i in 1:ratio) {
      mydecom <- sparseDecom(
        inmatrix = images[demog.split[[i]]$rows.in, ],
        inmask = mask, 
        sparseness = sparseness, 
        nvecs = nvecs, 
        its = its, 
        cthresh = cthresh)
      eanatimages = mydecom$eigenanatomyimages
      if (is.matrix(eanatimages)){
        eanatimages <- matrixToImages( eanatimages, mask = mask)
      }
      result[[paste("fold", i, sep = "")]] <- regressProjections(
        images[demog.split[[i]]$rows.in, ], 
        images[demog.split[[i]]$rows.out, ], 
        demog.split[[i]]$data.in, 
        demog.split[[i]]$data.out, 
        eigenvectors = eanatimages, 
        mask = mask, 
        outcome = outcome, 
        ...)
    }
  }
  return(result)
} 
