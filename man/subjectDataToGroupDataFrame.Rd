\name{subjectDataToGroupDataFrame}
\alias{subjectDataToGroupDataFrame}

\title{
Simple subjectDataToGroupDataFrameing function.
}

\description{
SubjectDataToGroupDataFrames take a list of subject-level csv files of the same type and converts them to a data frame with consistent column naming for the values and where rows correspond to subjects.
}

\usage{
mydf<-subjectDataToGroupDataFrame(   csvlist, usecol, mycolname=NA, datarownames=NA  )
}

\arguments{
\item{csvlist}{
  input list of csv files eg by Sys.glob ...
}
\item{usecol}{
  a column name or number e.g. 1 or "Volume"
}
\item{mycolname}{
  rename the column by this string (optional)
}
\item{datarownames}{
  the desired now names (optional)
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
data(aal,package='ANTsR')
csvlist<-Sys.glob("*md*csv")
mypopulationdataframe<-subjectDataToGroupDataFrame( csvlist , "Mean" , datarownames=aal$label_name )
# should have each ROI value for each subject listed in a large data frame
}
}
