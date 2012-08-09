\name{perfusion-predictors}
\alias{get_perfusion_predictors}

\title{
Get perfusion predictors
}

\description{
Get the predictors for perfusion regression from time-series voxels and motion-parameters.
}

\usage{
get_perfusion_predictors( mat , moco_params , xideal = NULL , labelfirst = 1 , ncompcorparameters = 4 )
}

\arguments{
\item{mat}{
Matrix with a column for every time-series voxel. Number of rows equals
the number of time units in the series.
}
\item{mask}{
  Motion Parameters obtained as results after motion-correction.
}
\item{xideal}{
  [optional] 1D time-series signal to be used a ideal or model for regression.
}
\item{labelfirst}{
  [optional] Boolean indicating if the first value is label in every time-series.
}
\item{ncompcorparameters}{
  [optional] Number of compcorr parameters.
}
}

\value{
Success -- An R list of named elements containing xideal ( named
'xideal' ) and nuisance parameters ( named 'nuis' )\cr
Failure -- NULL
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# mat -- matrix with each column being a time-series voxel
# moco_results -- result of calling 'motion_correction'
# moco_results$moco_params -- motion-correction parameters
predictors <- get_perfusion_predictors( mat , moco_results$moco_params )
predictors$xideal
predictors$nuis
}
}
