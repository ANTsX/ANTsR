\name{regressionNetworkViz}
\alias{regressionNetworkViz}

\title{
Visualize a regression result by a d3 network visualization.
}

\description{
Use either a force directed graph or a Sankey graph to show relationships between predictors and outcome variables. 
 correlateMyOutcomes should correspond to the outcome variables ... 
}

\usage{
regressionNetworkViz( mylm , sigthresh=0.05, whichviz="Sankey", outfile="temp2.html", correlateMyOutcomes = brainpreds) 
}

\arguments{
\item{myLM}{
  lm model output from bigLMStats
}
}

\value{
html file is output
}

\author{
Avants BB
}

\examples{
\dontrun{
colnames(brainpreds)<-paste("Vox",c(1:ncol(brainpreds)),sep='')
colnames( mylm$beta.pval )<-colnames(brainpreds)
demognames<-rownames(mylm$beta.pval)
regressionNetworkViz( mylm , sigthresh=0.05, outfile="temp2.html") 
}
}
