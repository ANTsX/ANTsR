\name{inspectImageData3D}
\alias{inspectImageData3D}

\title{
Simple inspectImageData3D function.
}

\description{
InspectImageData3D collects basic statistics over a dataset and returns them in a dataframe.
}

\usage{
summat<-inspectImageData3D( filelist  )
}

\arguments{
\item{fn}{
  input list of filenames
}
}

\value{
matrix is output
}

\author{
Avants BB
}

\examples{
\dontrun{
mm<-inspectImageData3D( fnl<-c("r16slice.nii.gz","r64slice.nii.gz" ) ) 
library(fpc)
library(DMwR)
pamres <- pamk(mm) 
plot(pamres$pamobject)
outlier.scores <- lofactor( mm, k=5)
outliers <- order(outlier.scores, decreasing=T)[1:5]
}
}
