\name{subjectDataToGroupDataFrame}
\alias{subjectDataToGroupDataFrame}

\title{
Simple subjectDataToGroupDataFrameing function.
}

\description{
SubjectDataToGroupDataFrames take a list of subject-level csv files of the same type and converts them to a data frame with consistent column naming for the values and where rows correspond to subjects.
}

\usage{
mydf<-subjectDataToGroupDataFrame(  csvlist , colnames=NA )
}

\arguments{
\item{mat}{
  input list of csv files eg by Sys.glob ...
}
}

\value{
data frame is output
}

\author{
Avants BB
}

\examples{
\dontrun{
wmat<-subjectDataToGroupDataFrame( csvlist,1 ) 
}
}
