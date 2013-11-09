\name{blockStimulus}
\alias{blockStimulus}
\title{Block stimulus model for FMRI Data }
\description{
  Create the block BOLD stimulus for a given task indicator function. Current units in terms of volumes.  Sort of a bug, should be in TR. 
}
\usage{
  blockStimulus(scans = 1, onsets = c(1), durations = c(1), tr=2.5 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scans}{ number of scans }
  \item{onsets}{ vector of onset times (in scans) }
  \item{durations}{ vector of duration of ON stimulus in scans or
    seconds (if \code{!is.null(times)})  }
  \item{rt}{ time between scans in seconds (TR) }
}
\details{
  None
}
\value{
  Vector with dimension \code{c(scans, 1)}.
}
\references{ Worsley, K.J., Liao, C., Aston, J., Petre, V., Duncan,
  G.H., Morales, F., Evans, A.C. (2002). A general statistical analysis
  for fMRI data. NeuroImage, 15:1-15. 
}
\author{ b avants \email{stnava@gmail.com} }
\examples{
  # Example 1
  hrf <- blockStimulus(107, c(18, 48, 78), 15, 2)
}
\keyword{ regression }% at least one, from doc/KEYWORDS
\keyword{ design }% __ONLY ONE__ keyword per line

