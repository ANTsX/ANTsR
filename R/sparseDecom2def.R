sparseDecom2def <- function(inmatrix, inmask = c(NA, NA), sparseness = c(0.01, 0.01), nvecs = 5, its = 5, cthresh = c(0, 0), statdir = NA, perms = 0, uselong = 0, z = 0, smooth = 0, robust = 0, mycoption = 1, initializationList = list(), 
  initializationList2 = list(), ell1 = 0.05)
{
  basecca<-sparseDecom2( inmatrix=inmatrix, inmask=inmask, sparseness=sparseness, nvecs=2, its=its, cthresh=cthresh, statdir=statdir, perms=perms, uselong=uselong, z=z, smooth=smooth, robust=robust, mycoption=mycoption,  initializationList=initializationList, initializationList2=initializationList2, ell1=ell1 )
  deflatemat1<-matrix(  rep(0, nrow(inmatrix[[1]])*nvecs ) , nrow=nrow(inmatrix[[1]]) )
  deflatemat2<-matrix(  rep(0, nrow(inmatrix[[2]])*nvecs ) , nrow=nrow(inmatrix[[2]]) )
  deflatemat1[,1]<-basecca$projections[,1]
  deflatemat2[,1]<-basecca$projections2[,1]
  eig1<-basecca$eig1[,1]
  eig2<-basecca$eig2[,1]
  for ( j in 2:nvecs ) {
    inmat1r<-as.matrix( residuals( lm( inmatrix[[1]] ~  deflatemat1 ) ) )
    inmat2r<-inmatrix[[2]] # as.matrix( residuals( lm( inmatrix[[2]] ~  deflatemat2 ) ) )
    basecca<-sparseDecom2( inmatrix=list(inmat1r,inmat2r), inmask=inmask, sparseness=sparseness, nvecs=2, its=its, cthresh=cthresh, statdir=statdir, perms=perms, uselong=uselong, z=z, smooth=smooth, robust=robust, mycoption=mycoption,  initializationList=initializationList, initializationList2=initializationList2, ell1=ell1 )
    deflatemat1[,j]<-basecca$projections[,1]
    deflatemat2[,j]<-basecca$projections2[,1]
    if ( typeof(basecca$eig1[[1]]) != "double" )  {
      print("...")
    }
    eig1<-cbind(eig1,basecca$eig1[,1])
    eig2<-cbind(eig1,basecca$eig2[,1])
  }
  projections<-inmatrix[[1]] %*% as.matrix(eig1)
  projections2<-inmatrix[[2]] %*% as.matrix(eig2)
  return(list( eig1 = eig1, eig2=eig2, projections=projections, projections2=projections2 ) )
} 
