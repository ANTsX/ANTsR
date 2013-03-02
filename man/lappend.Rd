\name{lappend}
\alias{lappend}

\title{
Simle list append tool
}

\description{
Append an item to a list , creating a new list or combine two lists
}

\usage{
mynewlist<-lappend( mylist, myitem  )
}

\arguments{
\item{inlist}{
 list to which you will append the 2nd item
}

\item{myitem}{
to be added to the list
}


}

\value{
outputs the appended list
}

\author{
Avants BB
}

\examples{
\dontrun{
mat<-replicate(100, rnorm(20)) 
mat2<-replicate(100, rnorm(20))
mylist<-list(mat)
mylist<-lappend( mylist, mat2 ) 
}
}
