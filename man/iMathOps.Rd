\name{iMathOps}
\alias{iMathOps}
\docType{data}
\title{iMathOps}
\description{
Examples, description and categorization of iMath operations.
}
\usage{data(iMathOps)}
\format{
  A data frame listing the following variables.
  \describe{
    \item{\code{Operation}}{Name of operation}
    \item{\code{OperationType}}{Enumerated type of the operation (filter, etc)}
    \item{\code{Parameters}}{descriptive parameters}
    \item{\code{Example}}{working example code}
    \item{\code{Description}}{free text description}
    \item{\code{OutputDimensionalityChange}}{NA if not image, otherwise
      increment to output image dimensionality relative to input image}
  }
}
\examples{
i<-antsImageRead( getANTsRData('r16') , 2 )
roiImg<-getMask(i)
data( "iMathOps", package = "ANTsR", envir = environment() ) #
for ( j in c(13,18) )
  {
  op <- as.character( iMathOps$Operation[j] )
  k  <- eval( parse( text = toString( iMathOps$Example[j] ) ) )
  }
}
\keyword{datasets}
