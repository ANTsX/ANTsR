% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{write_simlr_data_frames}
\alias{write_simlr_data_frames}
\title{Write a list of data frames to disk with SiMLR-specific naming convention}
\usage{
write_simlr_data_frames(data_list, file_prefix)
}
\arguments{
\item{data_list}{A list of data frames to write to disk.}

\item{file_prefix}{A character string to use as the prefix for the filenames.}
}
\value{
No return value, called for side effects.
}
\description{
This function writes each data frame in a list to a separate CSV file on disk,
using the names of each data frame to create unique filenames.
}
\examples{
mysim <- list(simlrX = list(v = list(
  data1 = data.frame(matrix(rnorm(147 * 171), nrow = 147, ncol = 171)),
  data2 = data.frame(matrix(rnorm(156 * 171), nrow = 156, ncol = 171))
)))
write_simlr_data_frames(mysim$simlrX$v, "output")
}
