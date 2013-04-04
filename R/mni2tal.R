mni2tal <- function( x = 0 )
  {
  if ( nargs() == 0 | length(x) != 3 )
    {
    print("Usage:  talPoint<-mni2tal( mniPoint ) ")
    print("mni2tal for converting from ch2/mni space to tal - very approximate")
    return(1)
    }
  xout<-x
  if ( x[3] >= 0 )
    {
    xout[1]<-x[1] * 0.99
    xout[2]<-x[2] * 0.9688     + 0.046  * x[3]
    xout[3]<-x[2] * (-0.0485 ) + 0.9189 * x[3]
    }
  if ( x[3] < 0 )
    {
    xout[1]<-x[1] * 0.99
    xout[2]<-x[2] * 0.9688     + 0.042 * x[3]
    xout[3]<-x[2] * (-0.0485 ) + 0.839 * x[3]
    }
  return( xout )
  }


