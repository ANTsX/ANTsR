\name{phantom_population_study}
\alias{phantom_population_study}

\title{
ANTsR population study using R linear regression.
}

\description{
This example illustrates how to perform a voxel-wise statistical analysis.  We assume population data has been normalized to a template and now you want to do a statistical study. Note that the procedure for a first level fMRI study , a 2nd level fMRI study or a morphometry / volumetry / correlation study is very similar. Also, 2D and 3D are equally easy though not equally fast.  We assume the mask is in the same space as the input images.
}

\usage{
pathtoANTsR<-"~/ANTsR" # location of ANTsR source 
pathtoexample<-paste(pathtoANTsR,'/demo/phantom_population_study.R',sep='')
source(pathtoexample)
}

\arguments{
None
}

\author{
Avants BB
}

\examples{
source('demo/phantom_population_study.R')
}
