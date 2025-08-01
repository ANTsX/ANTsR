% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{construct_sim_names}
\alias{construct_sim_names}
\title{Construct SiMLR Feature Names}
\usage{
construct_sim_names(base_names, indices)
}
\arguments{
\item{base_names}{A character vector of modality prefixes (e.g., c("t1", "dt", "rsf")).}

\item{indices}{An integer vector of component indices (e.g., 1:10).}
}
\value{
A character vector of combined names (e.g., "t1PC1", "dtPC1", ... "rsfPC10").
}
\description{
A helper function to generate a character vector of SiMLR feature names
based on a set of modality prefixes and numeric indices.
}
\examples{
modalities <- c("t1", "dt", "rsf")
components <- 1:3
construct_sim_names(modalities, components)
}
