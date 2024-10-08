% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{read_simlr_data_frames}
\alias{read_simlr_data_frames}
\title{Read a list of data frames from disk with SiMLR-specific naming convention}
\usage{
read_simlr_data_frames(file_prefix, data_names, verbose = FALSE)
}
\arguments{
\item{file_prefix}{A character string used as the prefix for the filenames.}

\item{data_names}{A character vector of names for the data frames.}

\item{verbose}{boolean}
}
\value{
A list of data frames read from disk with the column named `X` set as row names.
}
\description{
This function reads a list of data frames from disk into a list,
assuming the files are named with a common prefix and the names of the data frames.
It converts the column named `X` to the row names of the read data frame.
}
\examples{
# data_names <- c("data1", "data2")
# data_list <- read_simlr_data_frames(file_prefix = "output", data_names = data_names)
# dim(data_list[[1]])
# dim(data_list[[2]])
}
