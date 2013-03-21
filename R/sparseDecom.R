sparseDecom <- function( inmatrix=NA,  inmask=NA , sparseness=0.01, nvecs=50 , its=5 , cthresh=250 , statdir = NA )
{
  numargs<-nargs()
  if ( numargs < 1 | missing(inmatrix) )
    {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 ) \n")
    return(0);
    }
  if ( is.na(statdir) ) statdir<-paste(tempdir(),"/",sep='')
  outfn<-paste(statdir,'spca.nii.gz',sep='')
  decomp<-paste(statdir,'spcaprojectionsView1vec.csv',sep='')
  matname<-paste(statdir,'spcamatrix.mha',sep='')
  antsImageWrite( as.antsImage(inmatrix), matname )
  mfn<-NA
  if ( !is.na(inmask) )
    {
    mfn<-paste(statdir,'spcamask.nii.gz',sep='')
    antsImageWrite( inmask, mfn )
    } 
  args<-list("--svd",paste("recon[",matname,",",mfn,",",sparseness,"]",sep=''),"--l1",1,"-i",its,"--PClusterThresh",cthresh,"-n",nvecs,"-o",outfn)
  .Call( "sccan", int_antsProcessArguments( c(args) ) ) ;
  mydecomp<-read.csv(decomp)
  if ( !is.na(inmask) )
    {
    glb<-paste("spca*View1vec*.nii.gz",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnll<-list()
    for ( i in 1:length(fnl) )
      {
      img<-antsImageRead( fnl[i] , dim[1] )
      fnll<-lappend( fnll, img )
      }
    fnl<-fnll
    }
  if ( is.na(inmask) )
    {
    glb<-paste("spca*_Variate_View1vec.csv",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnl<-read.csv(fnl)
    }
  return( list( projections=mydecomp, eigenanatomyimages=fnl ) )
}

