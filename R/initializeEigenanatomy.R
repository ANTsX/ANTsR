initializeEigenanatomy <- function( initmat  , mask=NA, nreps=1 )
{
  nclasses<-nrow( initmat )
  classlabels<-rownames(initmat)
  if ( is.null(classlabels) ) classlabels<-paste("init",1:nclasses,sep='')
  initlist<-list()
  if ( is.na(mask) ) {
    maskmat<-initmat*0
    maskmat[1,]<-1
    mask<-as.antsImage( maskmat )
  }
  eanatnames<-rep(as.character("A"),nclasses*nreps)
  ct<-1
  for ( i in 1:nclasses ) {
    vecimg<-antsImageClone( mask )
    initf<-initmat[i,] 
    vecimg[ mask == 1 ]<-initf # eanatsparsify( initf , sparval[1] )
    for (  nr in 1:nreps )
      {
      initlist<-lappend(initlist,vecimg)
      eanatnames[ct+nr-1]<-toString(classlabels[i])
      }
    ct<-ct+nreps
  }

 return( list(initlist=initlist, mask=mask, enames=eanatnames ) )
}
