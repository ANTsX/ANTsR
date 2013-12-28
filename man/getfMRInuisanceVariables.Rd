\name{getfMRInuisanceVariables}
\alias{getfMRInuisanceVariables}

\title{
Extract generic fMRI nuisance variables for ASL or BOLD
}

\description{
Will motion correct, run compcorr and estimate global signal. Outputs a list with the time series data matrix (time by voxels), motion and other nuisance variables, global signal (for BOLD or ASL), the mask and the average time series image.  Meant to be used before filterfMRIforNetworkAnalysis or any other results-oriented processing.  
}

\usage{
output<-getfMRInuisanceVariables( boldImageOrFileName, maskThresh=100 , moreaccurate=T, mask = myfMRImask )
}

\arguments{
\item{boldImageOrFileName}{
  input antsImage or filename 
}
\item{maskThresh}{
  will use this intensity threshold to estimate a mask otherwise will use the mask passed in
}
\item{mask}{
  binary image masking the intput image
}
}

\value{
outputs list described above.
}

\author{
Avants BB
}

\examples{
\dontrun{
# set moreaccurate=T for final results 
dd<-getfMRInuisanceVariables( bold, maskThresh=100 , moreaccurate=F )
tsResid<-residuals( lm( dd$matrixTimeSeries ~ dd$globalsignal + dd$nuisancevariables ))
mynetwork<-filterfMRIforNetworkAnalysis( tsResid , tr=4, mask=dd$mask ,cbfnetwork = "BOLD", labels = aalImageLabels , graphdensity = 0.25 )
# use colnames( dd$nuisancevariables ) to see nuisance variables 

ee<-getfMRInuisanceVariables( pcasl, mask = pcaslmask , moreaccurate=F )
tsResid<-residuals( lm( ee$matrixTimeSeries ~ ee$globalsignalASL + ee$nuisancevariables ))
myASLnetwork<-filterfMRIforNetworkAnalysis( tsResid , tr=4, mask=ee$mask ,cbfnetwork = "ASLCBF", labels = aal2asl , graphdensity = 0.25 )
# use colnames( dd$nuisancevariables ) to see nuisance variables 
}
}
