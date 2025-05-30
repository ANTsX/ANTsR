% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{visualize_permutation_test}
\alias{visualize_permutation_test}
\title{Visualize Permutation Test Results}
\usage{
visualize_permutation_test(
  permutation_results,
  original_stat,
  stat_name,
  plot_title,
  bin_width = 0.1
)
}
\arguments{
\item{permutation_results}{A numeric vector of permutation test statistics.}

\item{original_stat}{A numeric value representing the original unpermuted test statistic.}

\item{stat_name}{A character string representing the name of the test statistic.}

\item{plot_title}{string for plot title}

\item{bin_width}{optional bin width for the histogram}
}
\value{
A ggplot object showing the histogram of permutation test statistics with the original
test statistic marked.
}
\description{
This function visualizes the results of a permutation test by plotting a histogram of the
permutation test statistics. A red dotted line indicates the location of the original unpermuted
test statistic.
}
\examples{
\dontrun{
set.seed(123)
n_perms <- 1000
permutation_results <- rnorm(n_perms, mean = 0, sd = 1)
original_stat <- 2
visualize_permutation_test(permutation_results, original_stat, "Simulated Statistic")
}
}
