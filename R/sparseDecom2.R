sparseDecom2 <- function( inmatrix,  inmask=c(NA,NA) , sparseness=c(0.01,0.01) , nvecs=50 , its=5 , cthresh=c(250,250) , statdir = NA , perms=0, uselong=0 , z=0)
{
  numargs<-nargs()
  if ( numargs < 1 | missing(inmatrix) )
    {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=c(0.01,0.01) , nvecs=50 , cthresh=c(250,250),  its=5  ) \n")
    cat(" each input should be a list with 2 entries e.g. sparseness=c(0.01,0.02) \n" )
    return(0);
    }
  post<-c(1,2)
  if ( is.na(statdir) ) statdir<-paste(tempdir(),"/",sep='')
  mfn<-paste(statdir,'sccamask',post,'.nii.gz',sep='')
  outfn<-paste(statdir,'scca.nii.gz',sep='')
  decomp<-paste(statdir,'sccaprojectionsView',post,'vec.csv',sep='')
  matname<-paste(statdir,'sccamatrix',post,'.mha',sep='')
  antsImageWrite( as.antsImage(inmatrix[[1]]), matname[1] )
  antsImageWrite( as.antsImage(inmatrix[[2]]), matname[2] )
  if ( !is.na(inmask[[1]]) )
    {
    m1<-inmask[[1]]
    dim<-as.numeric(m1@dimension)
    antsImageWrite( inmask[[1]], mfn[1] )
    } else mfn[1]<-NA
  if ( !is.na(inmask[[2]]) )
    {
    m2<-inmask[[2]]
    dim2<-as.numeric(m2@dimension)
    antsImageWrite( inmask[[2]], mfn[2] )
    } else mfn[2]<-NA
  args<-list("--scca",paste("two-view[",matname[1],",",matname[2],",",
              mfn[1],",",mfn[2],",",sparseness[1],",",sparseness[2],
              "]",sep=''),"--l1","0.05","-i",its,"--PClusterThresh",cthresh[1],"-p", perms,
              "--QClusterThresh",cthresh[2],"-n",nvecs,"-o",outfn,"-g",uselong,"-z",z )
  .Call( "sccan", int_antsProcessArguments( c(args) ) , PACKAGE = "libRsccan" ) ;
  mydecomp<-read.csv(decomp[1])
  if ( !is.na(inmask[[1]]) )
    {
    glb<-paste("scca*View1vec*.nii.gz",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnll<-list()
    for ( i in 1:length(fnl) )
      {
      img<-antsImageRead( fnl[i] , dim[1] )
      fnll<-lappend( fnll, img )
      }
    fnl<-fnll
    }
  mydecomp2<-read.csv(decomp[2])
  if ( !is.na(inmask[[2]]) )
    {
    glb<-paste("scca*View2vec*.nii.gz",sep='')
    fnl2<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnll2<-list()
    for ( i in 1:length(fnl2) )
      {
      img<-antsImageRead( fnl2[i] , dim2[1] )
      fnll2<-lappend( fnll2, img )
      }
    fnl2<-fnll2
    }
  if ( is.na(inmask[[1]]) )
    {
    glb<-paste("*scca*_Variate_View1vec.csv",sep='')
    fnl<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnl<-read.csv(fnl)
    }
  if ( is.na(inmask[[2]]) )
    {
    glb<-paste("scca*_Variate_View2vec.csv",sep='')
    fnl2<-list.files(path=statdir, pattern = glob2rx(glb),full.names = T,recursive = T)
    fnl2<-read.csv(fnl2)
    }
  pvfn<-paste(statdir,'scca_summary.csv',sep='')
  ccasummary<-NA
  if ( file.exists( pvfn ) )
    {
    ccasummary<-read.csv( pvfn )
    }
  return( list( projections=mydecomp,projections2=mydecomp2, eig1=fnl, eig2=fnl2 , ccasummary=ccasummary ) )
}
