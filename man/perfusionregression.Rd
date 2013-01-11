\name{perfusionregression}
\alias{perfusionregression}

\title{
Perfusion Regression
}

\description{
Estimate CBF using standard regression and optionally robust regression.
}

\usage{
perfusionregression( mask_img , mat , xideal , nuis , m0, dorobust = 0 )
}

\arguments{
\item{mask_img}{
Mask image selects the voxels where CBF will be estimated. Voxels
corresponding to logical FALSE are not computed.
}
\item{mat}{
Matrix with a column for every time-series voxel. Number of rows equals
the number of time units in the series.
}
\item{xideal}{
1D time-series signal to be used a ideal or model for regression.
}
\item{nuis}{
Nuisance parameters obtained from 'get_perfusion_predictors'.
}
\item{m0}{
m0 antsImage passed in according to needs of specific ASL sequence.
}
\item{dorobust}{
Real value in interval from 0 to 1.  If greater than 0, then robust regression will be performed.  A typical value would be 0.95 i.e. use voxels with 95 percent confidence.
}
}

\value{
Success -- An object of type 'antsImage' containing the CBF estimate for
voxels corresponding to the mask input\cr
}

\author{
Shrinidhi KL
Avants BB
}

\examples{
\dontrun{
# predictors -- result of calling 'get_perfusion_predictors'
cbf <- perfusionregression( mask_img, mat , predictors$xideal , predictors$nuis )
}
}
