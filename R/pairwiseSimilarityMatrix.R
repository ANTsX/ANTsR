pairwiseSimilarityMatrix <- function( dim , myFileList, nclusters = NA )
  {
    fnl<-length( myFileList )
    mymat<- matrix( rep( NA, fnl*fnl) , nrow = fnl, ncol = fnl )
    tct<-0
    for ( ct in 1:fnl )
      {
      for ( ct2 in 1:fnl )
        {
        if ( ct != ct2 )
          {
          i1<-antsImageRead( myFileList[ct] , dim ) 
          i2<-antsImageRead( myFileList[ct2] , dim )
          toutfn<-paste(tempdir(),"/Z",sep='')
          sink( toutfn ) 
          mytx<-antsRegistration(fixed=i1 , moving=i2 , typeofTransform = c("Affine"), outprefix=toutfn )
          mywarpedimage<-antsApplyTransforms(fixed=i1,moving=i2,transformlist=mytx$fwdtransforms)
          sink(NULL)
          metric<-capture.output(ImageMath(dim,"j","PearsonCorrelation",i1,mywarpedimage))[1]
          mymat[ct,ct2]<-as.numeric( metric )
          tct<-tct+1
          print( paste( 100 * tct / (fnl*fnl) , "%") )
          }
        }
      }
    clusters<-rep( NA, fnl ) 
    if ( ! is.na( nclusters ) )
      {
      library( cluster )
      pamx <- pam( mymat , nclusters )
      clusters<-summary(pamx)$clustering
      }
    return( list(rawMatrix=mymat, symmMatrix= 0.5*(mymat+t(mymat)), clusters=clusters ) )
  }
