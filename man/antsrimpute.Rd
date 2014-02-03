\name{antsrimpute}
\alias{antsrimpute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Impute NA's on data frame.  
}
\description{
Imputes NA's on data frame (column-wise), using a user-specified function (mean is default).  also works on a vector.
}
\usage{
antsrimpute(mydat, FUN=mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydat}{Data frame to be imputed.}
  \item{FUN}{Method to be used for imputation (defaults to mean). }
  \item{...}{Additional parameters to pass to FUN.}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns mydat with NA's replaced by imputed numbers.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Kandel BM, Avants BB
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mydat <- data.frame(A=c(1,2,4,5), B=c(1,NA,4,5))
mean.impute <- antsrimpute(mydat)
median.impute <- antsrimpute(mydat, median)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
