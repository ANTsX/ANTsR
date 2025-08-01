% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{simlr}
\alias{simlr}
\title{Similarity-driven multiview linear reconstruction model (simlr) for N modalities}
\usage{
simlr(
  voxmats,
  smoothingMatrices,
  iterations = 10,
  sparsenessQuantiles,
  positivities,
  initialUMatrix,
  mixAlg = c("svd", "ica", "avg", "rrpca-l", "rrpca-s", "pca", "stochastic"),
  orthogonalize = FALSE,
  repeatedMeasures = NA,
  lineSearchRange = c(-1e+10, 1e+10),
  lineSearchTolerance = 1e-08,
  randomSeed,
  constraint = c("Grassmannx1000x1000", "Stiefelx1000x1000", "orthox1000x1000", "none"),
  energyType = c("cca", "regression", "normalized", "ucca", "lowRank",
    "lowRankRegression", "rv_coefficient"),
  vmats,
  connectors = NULL,
  optimizationStyle = c("lineSearch", "mixed", "greedy"),
  scale = c("centerAndScale", "sqrtnp", "np", "center", "norm", "none", "impute",
    "eigenvalue", "robust", "whiten", "lowrank", "rank"),
  expBeta = 0,
  jointInitialization = TRUE,
  sparsenessAlg = NA,
  verbose = FALSE
)
}
\arguments{
\item{voxmats}{A list that contains the named matrices.  Note: the optimization will likely perform much more smoothly if the input matrices are each scaled to zero mean unit variance e.g. by the \code{scale} function.}

\item{smoothingMatrices}{list of (sparse) matrices that allow parameter smoothing/regularization.  These should be square and same order and size of input matrices.}

\item{iterations}{number of gradient descent iterations}

\item{sparsenessQuantiles}{vector of quantiles to control sparseness - higher is sparser}

\item{positivities}{vector that sets for each matrix if we restrict to positive or negative solution (beta) weights.
choices are positive, negative or either as expressed as a string.}

\item{initialUMatrix}{list of initialization matrix size \code{n} by \code{k} for each modality.  Otherwise, pass a single scalar to control the
number of basis functions in which case random initialization occurs. One
may also pass a single initialization matrix to be used for all matrices.
If this is set to a scalar, or is missing, a random matrix will be used.}

\item{mixAlg}{'svd', 'ica', 'rrpca-l', 'rrpca-s', 'stochastic', 'pca' or 'avg' denotes the algorithm employed when estimating the mixed modality bases}

\item{orthogonalize}{boolean to control whether we orthogonalize the solutions explicitly}

\item{repeatedMeasures}{list of repeated measurement identifiers. this will
allow estimates of per identifier intercept.}

\item{lineSearchRange}{lower and upper limit used in \code{optimize}}

\item{lineSearchTolerance}{tolerance used in \code{optimize}, will be multiplied by each matrix norm such that it scales appropriately with input data}

\item{randomSeed}{controls repeatability of ica-based decomposition}

\item{constraint}{one of none, Grassmann, GrassmannInv or Stiefel}

\item{energyType}{one of regression, normalized, lowRank, cca or ucca}

\item{vmats}{optional initial \code{v} matrix list}

\item{connectors}{a list ( length of projections or number of modalities )
that indicates which modalities should be paired with current modality}

\item{optimizationStyle}{one of \code{c("mixed","greedy","linesearch")}}

\item{scale}{options to standardize each matrix. e.g. divide by the square root
of its number of variables (Westerhuis, Kourti, and MacGregor 1998), divide
by the number of variables or center or center and scale or ... (see code).
can be a vector which will apply each strategy in order.}

\item{expBeta}{if greater than zero, use exponential moving average on gradient.}

\item{jointInitialization}{boolean for initialization options, default TRUE}

\item{sparsenessAlg}{NA is default otherwise basic, spmp or orthorank}

\item{verbose}{boolean to control verbosity of output - set to level \code{2}
in order to see more output, specifically the gradient descent parameters.}
}
\value{
A list of u, x, y, z etc related matrices.
}
\description{
simlr minimizes reconstruction error across related modalities.  That is,
simlr will reconstruct each modality matrix from a basis set derived from
the other modalities.  The basis set can be derived from SVD, ICA or a
simple sum of basis representations.
This function produces dataset-wide multivariate beta maps for each of the
related matrices.  The multivariate beta maps are regularized by user
input matrices that encode relationships between variables.  The idea is
overall similar to canonical correlation analysis but generalizes the basis
construction and to arbitrary numbers of modalities.
}
\examples{

\dontrun{
set.seed(1500)
nsub <- 25
npix <- c(100, 200, 133)
nk <- 5
outcome <- matrix(rnorm(nsub * nk), ncol = nk)
outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
outcome3 <- matrix(rnorm(nsub * nk), ncol = nk)
view1tx <- matrix(rnorm(npix[1] * nk), nrow = nk)
view2tx <- matrix(rnorm(npix[2] * nk), nrow = nk)
view3tx <- matrix(rnorm(npix[3] * nk), nrow = nk)
mat1 <- (outcome \%*\% t(outcome1) \%*\% (outcome1)) \%*\% view1tx
mat2 <- (outcome \%*\% t(outcome2) \%*\% (outcome2)) \%*\% view2tx
mat3 <- (outcome \%*\% t(outcome3) \%*\% (outcome3)) \%*\% view3tx
matlist <- list(vox = mat1, vox2 = mat2, vox3 = mat3)
result <- simlr(matlist)
p1 <- mat1 \%*\% (result$v[[1]])
p2 <- mat2 \%*\% (result$v[[2]])
p3 <- mat3 \%*\% (result$v[[3]])
regs <- regularizeSimlr(matlist)
result2 <- simlr(matlist)
pred1 <- predictSimlr(matlist, result)
pred2 <- predictSimlr(matlist, result2)

# compare to permuted data
s1 <- sample(1:nsub)
s2 <- sample(1:nsub)
resultp <- simlr(list(vox = mat1, vox2 = mat2[s1, ], vox3 = mat3[s2, ]))
p1p <- mat1 \%*\% (resultp$v[[1]])
p2p <- mat2[s1, ] \%*\% (resultp$v[[2]])
p3p <- mat3[s2, ] \%*\% (resultp$v[[3]])

# compare to SVD
svd1 <- svd(mat1, nu = nk, nv = 0)$u
svd2 <- svd(mat2, nu = nk, nv = 0)$u
svd3 <- svd(mat3, nu = nk, nv = 0)$u

# real
range(cor(p1, p2))
range(cor(p1, p3))
range(cor(p3, p2))

# permuted
range(cor(p1p, p2p))
range(cor(p1p, p3p))
range(cor(p3p, p2p))

# svd
print(range(cor(svd1, svd2)))

resultp <- simlr(list(vox = mat1, vox2 = mat2[s1, ], vox3 = mat3[s2, ]),
  initialUMatrix = nk, verbose = TRUE, iterations = 5,
  energyType = "normalized"
)
}
}
\seealso{
\code{\link{milr}} \code{\link{mild}} \code{\link{simlrU}}
}
\author{
BB Avants.
}
