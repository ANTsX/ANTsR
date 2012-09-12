\name{perfusionregression}
\alias{perfusionregression}

\title{
Perfusion Regression
}

\description{
Estimate CBF using standard regression and optionally robust regression.
}

\usage{
perfusionregression( mask_img , mat , xideal , nuis , dorobust = FALSE )
}

\arguments{
\item{mask_img}{
Mask image used select the voxels receive CBF estimate. Voxels
corresponding to logical FALSE are not computed.
values 
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
\item{dorobust}{
Boolean, if TRUE, robust regression will be performed.
}
}

\value{
Success -- An object of type 'antsImage' containing the CBF estimate for
voxels corresponding to the mask input\cr
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# predictors -- result of calling 'get_perfusion_predictors'
cbf <- perfusionregression( mask_img, mat , predictors$xideal , predictors$nuis )
}
}
