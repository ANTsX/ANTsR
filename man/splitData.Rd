% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitData.R
\name{splitData}
\alias{splitData}
\title{Split data for testing and training}
\usage{
splitData(data.source, ratio, return.rows = FALSE)
}
\arguments{
\item{data.source}{Data frame or matrix of input data to be split.}

\item{ratio}{If greater than one, number of folds to split data into.
Otherwise, proportion of rows in training data.  See \code{Value}.}

\item{return.rows}{If \code{TRUE}, row numbers of testing and training data
are also returned.}
}
\value{
A list containing the input data, split into testing and training
sets.  If \code{ratio} is greater than one, returns a list with \code{ratio}
entries, each with 1/\code{ratio} of the input data in the testing set
(\code{data.out}) and the rest in the training set (\code{data.in}).
Otherwise, returns a list with one split of the data, with \code{ratio} of
the input data in the testing set and the rest in the training set.
}
\description{
Split data into testing and training sets for cross-validation.
}
\examples{
\dontrun{
n <- 30
ratio <- 2 / 3
data.source <- data.frame(value = 1:n)
out <- splitData(data.source, ratio)
}

}
\author{
Kandel BM and Avants B
}
