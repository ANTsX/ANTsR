clusterTimeSeries <- function(  img, mask, krange=2:10,
  nsvddims=NA ) {
  if (nargs() == 0) {
    print( args( clusterTimeSeries ) )
    return(1)
  }
  if ( is.na(nsvddims) ) nsvddims<-(max(krange)*2)
  mat<-timeseries2matrix( img, mask )
  if ( nsvddims > nrow(mat) ) nsvddims<-nrow(mat)/2
  matsvd<-svd( mat, nu=nsvddims, nv=0 )
  pk<-pamk( mat, krange=krange )
  clusters<-pk$pamobject$clustering
  tsimagelist<-list()
  for ( i in 1:max(clusters) )
    {
    kmat<-mat[ clusters==i  ,  ]
    tsimagelist[[i]]<-matrix2timeseries( img, mask, kmat  )
    }
  return( list(kimgs=tsimagelist, clusters=clusters) )
}
