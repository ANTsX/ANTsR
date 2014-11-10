timeseriesN3 <- function( boldimg, mask,
  ncorrections=c(4,2,2) )
{
  dim<-4
  ismatrix<-TRUE
  if ( class(boldimg) != 'matrix')
  {
  dim<-boldimg@dimension
  if ( dim != 4 ) {
    return( NA )
  }
  mat<-timeseries2matrix( boldimg, mask )
  ismatrix<-FALSE
  }
  if ( class(boldimg) == 'matrix')
  {
  mat<-boldimg
  }
  for ( i in 1:nrow(mat) )
    {
    perf<-makeImage( mask , mat[i,] )
    for ( nc in ncorrections )
      N3BiasFieldCorrection(dim-1,perf,perf,nc)
    mat[i,]<-perf[ mask == 1 ]
    }
  if ( ismatrix ) return(mat)
  if ( ! ismatrix )
  return( matrix2timeseries( boldimg, mask, mat ) )
}
