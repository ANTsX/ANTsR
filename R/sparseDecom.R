sparseDecom <- function( inmatrix=NA,  inmask=NA , sparseness=0.01, nvecs=50 , its=5 , cthresh=250 )
{
  numargs<-nargs()
  if ( numargs < 1 | missing(inmatrix) )
    {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 ) \n")
    return(0);
    }
  statdir<-paste(tempdir(),"/",sep='')
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
  spca<-paste("sccan --svd recon[",matname,",",mfn,",",sparseness,"] --l1 1 -i ",its," --PClusterThresh ",cthresh," -n ",nvecs," -o ",outfn,sep='') # must have ANTSPATH in system
  print(spca)
  system(spca)
  mydecomp<-read.csv(decomp)
  if ( !is.na(inmask) )
    {
    glb<-paste("spca*View1vec*.nii.gz",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    }
  if ( is.na(inmask) )
    {
    glb<-paste("spca*_Variate_View1vec.csv",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    }
  return( list( projections=mydecomp, eigenanatomyimages=fnl ) )
}

