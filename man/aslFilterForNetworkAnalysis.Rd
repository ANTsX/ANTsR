\name{aslFilterForNetworkAnalysis}
\alias{aslFilterForNetworkAnalysis}

\title{
Basic pre-processing for ASL-based network analysis using either bold or cbf signal.
}

\description{
Some ASL MR sequences allow network analysis of either BOLD or ASL signal.  See "Implementation of Quantitative Perfusion Imaging Techniques for Functional Brain Mapping using Pulsed Arterial Spin Labeling" by Wong et al, 1997 for an overview.  This function employs "surround" techniques for deriving either CBF or BOLD signal from the input ASL.  It performs motion-correction, factoring out motion and compcor nuisance paramters, frequency filtering and masking.  The output contains the filtered time series (matrix form), the mask and a vector of temporal signal variance. This is a WIP.
}

\usage{
aslFilterForNetworkAnalysis( asl_antsr_image_or_filename, tr, freqLo=0.01, freqHi = 0.1, cbfnetwork=TRUE , maskThresh=500, outputprefix = NULL )
}

\arguments{
\item{asl_antsr_image_or_filename}{The filename to an antsr image or pointer to an antsr image}
\item{tr = <value>}{The sequence's TR value , typically 3 or 4.}
\item{freqLo = <value>}{The lower frequency limit, e.g. 0.01 in band-pass filter}
\item{freqHi = <value>}{The higher frequency limit, e.g. 0.1 in band-pass filter}
\item{cbfnetwork = TRUE}{Whether to get CBF or BOLD signal from ASL}
\item{maskThresh = <value>}{A thresholding value for the mask ... may need to be adjusted up or down for your data although the mask does not need to be perfect.}
\item{outputprefix = <string>}{ filename prefix - if defined , this function will write out some sanity checking images.}
}

\value{
output is a list containing "filteredTimeSeries" "mask"               "temporalvar"

or

1 -- Failure
}

\author{
Avants BB
}

\examples{
\dontrun{
myres<-aslFilterForNetworkAnalysis("s3_date3_asl.nii.gz",tr=4,0.01,0.1,TRUE,"processing/butt")

in myres, you have:

names(myres)
[1] "filteredTimeSeries" "mask"               "temporalvar"

where the filteredTimeSeries is the frequency filtered BOLD (if FALSE
is set) or CBF ( if TRUE is set ) time series.
}
}
