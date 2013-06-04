sparseRegression <- function(inmatrix, demog, outcome, mask=NA , sparseness=0.05, nvecs=10 , its=5 , cthresh=250 , statdir = NA, z=0 , smooth=0)
{
  if (missing(inmatrix)) 
    stop("Missing input image matrix.")
  if (missing(demog)) 
    stop("Missing demographics.")
  if (missing(outcome)) 
    stop("Missing outcome.")
  if (is.na(statdir)) 
    statdir<-paste(tempdir(),"/",sep='')
  outfn<-paste(statdir,'spca.nii.gz',sep='')
  decomp<-paste(statdir,'spcaprojectionsView1vec.csv',sep='')
  matname<-paste(statdir,'spcamatrix.mha',sep='')
  antsImageWrite(as.antsImage(inmatrix), matname)
  demog.name <- paste(statdir, "demog.csv", sep='')
  write.csv(demog[, outcome], demog.name, row.names=FALSE)
  mfn<-NA
  if (!is.na(mask)){
    mfn<-paste(statdir, 'spcamask.nii.gz', sep='')
    antsImageWrite(mask, mfn)
  } 
  args<-list("--svd", paste("network[", matname, ",", mfn, ",", sparseness, ",", demog.name, "]", sep=''),
	     "--l1", 1, "-i", its, "--PClusterThresh", cthresh, "-n", nvecs, "-o", 
	     outfn, "-z", z, "-s", smooth)
  .Call( "sccan", int_antsProcessArguments( c(args) ) , PACKAGE = "libRsccan" ) ;
  mydecomp<-read.csv(decomp)
  if (!is.na(mask)){
    glb<-paste("spca*View1vec*.nii.gz",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)[1:nvecs]
    fnll<-list()
    for ( i in 1:length(fnl) ){
      img<-antsImageRead( fnl[i], length(dim(mask)) )
      fnll<-lappend( fnll, img )
    }
    fnl<-fnll
  }
  if ( is.na(mask) ){
    glb<-paste("spcaprojectionsView1vec.csv",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnl<-read.csv(fnl)
  }

  glb<-paste("spcaprojectionsView1vec.csv",sep='')
  fnu<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
  fnu<-read.csv(fnu)
  return( list( projections=mydecomp, eigenanatomyimages=fnl, umatrix=fnu ) )
}

