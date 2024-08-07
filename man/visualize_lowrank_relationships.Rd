% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{visualize_lowrank_relationships}
\alias{visualize_lowrank_relationships}
\title{Visualize Low-Rank Relationships}
\usage{
visualize_lowrank_relationships(
  X1,
  X2,
  V1,
  V2,
  plot_title,
  nm1 = "X1",
  nm2 = "X2"
)
}
\arguments{
\item{X1}{First matrix}

\item{X2}{Second matrix}

\item{V1}{First feature matrix}

\item{V2}{Second feature matrix}

\item{plot_title}{Title of the plot (optional)}

\item{nm1}{Name of the first matrix (default: "X1")}

\item{nm2}{Name of the second matrix (default: "X2")}
}
\value{
A list containing the heatmap, pairs plot, correlation matrix, and RV coefficient
}
\description{
Compute low-rank projections, pairwise correlations, and RV coefficient, and visualize the relationships using a heatmap and pairs plot.
}
\examples{
set.seed(123)
X1 <- matrix(rnorm(100), nrow = 10, ncol = 10)
X2 <- matrix(rnorm(100), nrow = 10, ncol = 10)
V1 <- matrix(rnorm(100), nrow = 10, ncol = 10)
V2 <- matrix(rnorm(100), nrow = 10, ncol = 10)
# result <- visualize_lowrank_relationships(X1, X2, V1, V2)
}
