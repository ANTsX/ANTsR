#' Sparse regression on input images.
#'
#' Compute a sparse, spatially coherent regression from a set of input images
#' (with mask) to an outcome variable.
#'
#' @param inmatrix Input data matrix, with dimension number of subjects by
#' number of voxels.
#' @param demog Input demographics data frame.  Contains outcome variable to
#' regress against.
#' @param outcome Name of column in \code{demog} to regress against.
#' @param mask Mask for reconstructing \code{inmatrix} in physical space.
#' @param sparseness Level of sparsity desired.  \code{0.05}, for example,
#' makes 5\% of the matrix be non-zero.
#' @param nvecs Number of eigenvectors to return.
#' @param its Number of cross-validation folds to run.
#' @param cthresh Cluster threshold.
#' @param statdir Where to put results.  If not provided, a temp directory is
#' created.
#' @param z Row (subject-wise) sparseness.
#' @param smooth Amount of smoothing.
#' @return A list of values:
#' \item{eigenanatomyimages}{Coefficient vector images.}
#' \item{umatrix}{Projections of input images on the sparse regression vectors.  Can be used for, e.g., subsequent classification/predictions.}
#' \item{projections}{Predicted values of outcome variable.}
#' @author Kandel BM, Avants BB.
#' @references Kandel B.M., D. Wolk, J. Gee, and B. Avants. Predicting
#' Cognitive Data from Medical Images Using Sparse Linear Regression.
#' Information Processing in Medical Imaging, 2013.  %% ~put references to the
#' literature/web site here ~
#' @examples
#'
#' nsubj <- 50
#' prop.train <- 1 / 2
#' subj.train <- sample(1:nsubj, prop.train * nsubj, replace = FALSE)
#' input <- t(replicate(nsubj, rnorm(125)))
#' outcome <- seq(1, 5, length.out = nsubj)
#' demog <- data.frame(outcome = outcome)
#' input[, 40:60] <- 30 + outcome + rnorm(length(input[, 40:60]), sd = 2)
#' input.train <- input[subj.train, ]
#' input.test <- input[-subj.train, ]
#' demog.train <- data.frame(outcome = demog[subj.train, ])
#' demog.test <- data.frame(outcome = demog[-subj.train, ])
#' mymask <- as.antsImage(array(rep(1, 125), dim = c(5, 5, 5)))
#' myregression <- sparseRegression(input.train, demog.train, "outcome", mymask,
#'   sparseness = 0.05, nvecs = 5, its = 3, cthresh = 250
#' )
#' # visualization of results
#' sample <- rep(0, 125)
#' sample[40:60] <- 1
#' signal.img <- as.antsImage(array(rep(0, 125), dim = c(5, 5, 5)))
#' signal.img[signal.img >= 0] <- sample
#' # plot( signal.img, axis=2, slices='1x5x1') # actual source of signal
#' # compare against first learned regression vector
#' myimgs <- list()
#' for (i in 1:5) {
#'   myarray <- as.array(myregression$eigenanatomyimages[[i]])
#'   myarray <- myarray / max(abs(myarray)) # normalize for visualization
#'   myimgs[[i]] <- antsImageClone(myregression$eigenanatomyimages[[i]])
#'   myimgs[[i]][mymask > 0] <- myarray
#' }
#' # plot(myimgs[[1]], axis=2, slices='1x5x1')
#' # use learned eigenvectors for prediction
#' result <- regressProjections(
#'   input.train, input.test, demog.train,
#'   demog.test, myregression$eigenanatomyimages, mymask, "outcome"
#' )
#' # plot(result$outcome.comparison$real, result$outcome.comparison$predicted)
#'
#' @export sparseRegression
sparseRegression <- function(
    inmatrix, demog, outcome, mask = NULL, sparseness = 0.05,
    nvecs = 10, its = 5, cthresh = 250, statdir = NA, z = 0, smooth = 0) {
  if (missing(inmatrix)) {
    stop("Missing input image matrix.")
  }
  if (missing(demog)) {
    stop("Missing demographics.")
  }
  if (missing(outcome)) {
    stop("Missing outcome.")
  }
  if (is.na(statdir)) {
    statdir <- paste(tempdir(), "/", sep = "")
  }
  outfn <- paste(statdir, "spca.nii.gz", sep = "")
  decomp <- paste(statdir, "spcaprojectionsView1vec.csv", sep = "")
  matname <- paste(statdir, "spcamatrix.mha", sep = "")
  antsImageWrite(as.antsImage(inmatrix), matname)
  demog.name <- paste(statdir, "demog.csv", sep = "")
  write.csv(demog[, outcome], demog.name, row.names = FALSE)
  mfn <- NA
  if (!is.null(mask)) {
    mask <- check_ants(mask)
    mfn <- paste(statdir, "spcamask.nii.gz", sep = "")
    antsImageWrite(mask, mfn)
  }
  args <- list(
    "--svd", paste("network[", matname, ",", mfn, ",", sparseness, ",",
      demog.name, "]",
      sep = ""
    ), "--l1", 1, "-i", its, "--PClusterThresh", cthresh,
    "-n", nvecs, "-o", outfn, "-z", z, "-s", smooth
  )
  ANTsRCore::sccanX(.int_antsProcessArguments(c(args)))
  mydecomp <- read.csv(decomp)
  if (!is.null(mask)) {
    glb <- paste("spca*View1vec*.nii.gz", sep = "")
    fnl <- list.files(
      path = statdir, pattern = glob2rx(glb), full.names = T,
      recursive = TRUE
    )[1:nvecs]
    fnll <- list()
    for (i in 1:length(fnl)) {
      img <- antsImageRead(fnl[i], length(dim(mask)))
      fnll <- lappend(fnll, img)
    }
    fnl <- fnll
  }
  if (is.null(mask)) {
    glb <- paste("spcaprojectionsView1vec.csv", sep = "")
    fnl <- list.files(
      path = statdir, pattern = glob2rx(glb),
      full.names = TRUE,
      recursive = TRUE
    )
    fnl <- read.csv(fnl)
  }
  glb <- paste("spcaprojectionsView1vec.csv", sep = "")
  fnu <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, recursive = T)
  fnu <- read.csv(fnu)
  return(list(projections = mydecomp, eigenanatomyimages = fnl, umatrix = fnu))
}
