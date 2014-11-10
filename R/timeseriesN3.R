timeseriesN3 <- function( boldimg, mask,
  ncorrections=c(4,2,2) )
{
  dim<-boldimg@dimension
  if ( dim != 4 ) {
    return( NA )
  }
  mat<-timeseries2matrix( boldimg, mask )
  for ( i in 1:nrow(mat) )
    {
    perf<-makeImage( mask , mat[i,] )
    for ( nc in ncorrections )
      N3BiasFieldCorrection(dim-1,perf,perf,nc)
    mat[i,]<-perf[ mask == 1 ]
    }
  return( matrix2timeseries( boldimg, mask, mat ) )
}
