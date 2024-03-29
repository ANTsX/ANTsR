% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eanatDef.R
\name{testEanat}
\alias{testEanat}
\title{Test eigenanatomy in order}
\usage{
testEanat(mymdl, myvar, sigthresh = 0.05, method = "BH")
}
\arguments{
\item{mymdl}{input model from \code{lm} with eigenanatomy on left}

\item{myvar}{name of variable in model to test}

\item{sigthresh}{significance threshold, for example 0.05}

\item{method}{a method for \code{p.adjust}}
}
\value{
nvecs is output, analogous to \code{nvecs} in
\code{svd(mat,nu=0,nv=nvecs)}
}
\description{
This tests each eigenanatomy region in order invisibly to the user and
stops when testing stops conserving power.  The algorithm returns NA if
there is no good testing procedure, given the statistical target. The basic
idea is to test the variables that explain the most variance first and
continue testing as long as (a) there is no significant relationship yet
found or (b) the existing significant relationship remains significant,
given the correction for multiple comparisons.
}
\examples{
mat <- matrix(rnorm(300), ncol = 3)
n <- nrow(mat)
g <- factor(c(rep(1, n / 2), rep(0, n / 2)))
mydf <- data.frame(gen = g, a = rnorm(n))
mymdl <- lm(mat ~ a + g, data = mydf)
nv <- testEanat(mymdl, myvar = "g1")
mat[1:(n / 2), 3] <- mat[1:(n / 2), 3] + 2
mymdl <- lm(mat ~ a + g, data = mydf)
nv <- testEanat(mymdl, myvar = "g1")

}
\references{
Avants BB, Hackman D, LM Betancourt, GM Lawson, H Hurt, MJ Farah #' Relation of Childhood Home Environment to Cortical Thickness in
Late Adolescence: Specificity of Experience and Timing, PloS one, 2015
}
\author{
Avants BB, Tustison NJ
}
