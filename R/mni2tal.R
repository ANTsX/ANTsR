mni2tal <- function( xin = 0 )
  {
  if ( nargs() == 0 | length(xin) != 3 )
    {
    print("Usage:  talPoint<-mni2tal( mniPoint ) ")
    print("mni2tal for converting from ch2/mni space to tal - very approximate")
    return(1)
    }
  x<-xin
#  The input image is in RAS coordinates but we use ITK which returns LPS coordinates.
#  So we need to flip the coordinates such that L => R and  P => A to get RAS (MNI) coordinates
  x[1]<-x[1] * ( -1 ) # flip X
  x[2]<-x[2] * ( -1 ) # flip Y 
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


