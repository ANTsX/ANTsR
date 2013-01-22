\name{KellyKapowski}
\alias{KellyKapowski}

\title{
Compute cortical thickness using the DiReCT algorithm.
}

\description{
Diffeomorphic registration-based cortical thickness based on probabilistic segmentation of an image.  This is an optimization algorithm.
}

\usage{
KellyKapowski( d=3, s=simg, g=gimg, w=wimg, c="[45,0.0,10]", r=0.5, m=1, o=oimg )
}

\arguments{
\item{imageDimension 2|3|4 }{
Number of dimensions of the input image
}

\item{inputImage}{
Input image to operate on
}

\item{s}{
segmentation image
}

\item{g}{
gray matter probability image
}

\item{w}{
white matter probability image
}

\item{c}{
convergence params - first controls iterations
}

\item{r}{
gradient descent update parameter
}

\item{m}{
gradient field smoothing parameter 
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Shrinidhi KL, Avants BB
}

\examples{
\dontrun{
KellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=45,r=0.5,m=1,o=oimg )
}
}
