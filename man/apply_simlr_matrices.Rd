% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{apply_simlr_matrices}
\alias{apply_simlr_matrices}
\title{Apply simlr matrices to an existing data frame and combine the results}
\usage{
apply_simlr_matrices(
  existing_df,
  matrices_list,
  n_limit = NULL,
  robust = FALSE,
  center = FALSE,
  scale = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{existing_df}{An existing data frame to which the matrices will be applied.}

\item{matrices_list}{A list of matrices read from CSV files.}

\item{n_limit}{NULL or integer that can limit the number of projections}

\item{robust}{boolean}

\item{center}{boolean center the data before applying}

\item{scale}{boolean scale the data before applying}

\item{verbose}{boolean}
}
\value{
A list including (entry one) data frame with the original data frame combined with the projections (entry two) the new column names
}
\description{
This function takes a list of matrices, applies each matrix via matrix multiplication
to an existing data frame, and combines the resulting projections with the original data frame.
}
\examples{
matrices_list <- list(
  matrix1 = matrix(rnorm(147 * 171), nrow = 147, ncol = 171),
  matrix2 = matrix(rnorm(147 * 156), nrow = 147, ncol = 156)
)
existing_df <- data.frame(matrix(rnorm(147 * 5), nrow = 147, ncol = 5))
# combined_df <- apply_simlr_matrices(existing_df, matrices_list)
}
