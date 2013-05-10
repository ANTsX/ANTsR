\name{aslPerfusion}
\alias{aslPerfusion}

\title{
ASL-based Perfusion from PASL, CASL or pCASL.
}

\description{
This function will estimate perfusion from an ASL time series using a (robust) regression approach. It will do motion correction, compcorr, factor out nuisance variables and use regression to estimate the perfusion itself.  It will mask the image too based on a simple procedure ( should fix this in the future by allowing the user to optionally pass a mask in from the outside ).   WARNING: the function will estimate the m0 image from the mean of the control tags assuming that the data is acquired T C T C as is most of JJ's data.  Quantitative CBF can be obtained by mutiplying the output of this function by a scalar. 
}

\usage{
aslPerfusion( asl_antsr_image_or_filename, maskThresh=500,  moreaccurate = TRUE , dorobust = 0 )
}

\arguments{
\item{asl_antsr_image_or_filename}{The filename to an antsr image or pointer to an antsr image}
\item{maskThresh = <value>}{A thresholding value for the mask ... may need to be adjusted up or down for your data although the mask does not need to be perfect.}
\item{dorobust = <value in 0 to 1>}{Controls whether you use a robust regression when estimating perfusion values}
}

\value{
output is an antsImage containing perfusion values 

or

1 -- Failure
}

\author{
Avants BB
}

\examples{
\dontrun{
myperf<-aslPerfusion( asl )
}
}
