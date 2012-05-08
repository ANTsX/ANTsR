\name{Comparison}
\alias{Comparison}
\alias{"=="}

\title{
Relational operators for antsImage
}

\description{
Relational operators to compare pixel values of an \code{antsImage} with a scalar value within a given \code{antsRegion}.
}

\usage{
img == value
img != value
img <= value
img >= value
img < value
img > value

img == lst
img != lst
img <= lst
img >= lst
img < lst
img > lst
}

\arguments{
\item{img}{
Image object of S4 class 'antsImage' to be indexed.
}

\item{value}{
Scalar \code{numeric} to be compared to the pixel values.
}

\item{lst}{
\code{list} of named arguments containing names 'value' and 'region'. \code{lst$value} is used as 'value' and \code{lst$region} is used as the \code{antsRegion} within the image for comparison.
}
}

\details{
\describe{
\item{\code{img == value}}{
Image-region considered is the largest-possible-region( entire image ) of the image.
}
}
}

\value{
\code{logical vector} of dimensions equal to that of 'region' considered -- Success ; NA -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# compare all the values in the region starting at index 'c(1,1,1)' with size 'c(10,10,10)' with value '20' ;
# here the logical vector 'vect' will have dimensions '10*10*10'
reg = new( "antsRegion" , index = c(1,1,1) , size = c(10,10,10) )
vect = ( img = list( value = 20 , region = reg ) )
# compare all the pixels in the image with value '30' ;
# here the logical vector 'vect' will have dimensions of the image
vect = ( img == 30 )
}
}
