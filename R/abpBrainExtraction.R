abpBrainExtraction <- function( img = NA,  tem = NA , temmask=NA , tempriors=NA )
{
  if ( missing( img ) | missing( tem ) | missing( temmask ) )
    {
    cat('usage: abpBrainExtraction( img=imgToBExtract, tem = template, temmask = mask, tempriors=c(img1,img2,...,imgN) \n')
    cat(" if no priors are passed, or a numerical prior is passed, then use kmeans \n")
    return(NULL)
    }
  if  ( missing( tempriors ) ) { tempriors<-3 ; npriors<-3 }  else  {  npriors<-length( tempriors ) }

  # file I/O - all stored in temp dir 
  tdir<-tempdir()
  initafffn<-tempfile( pattern = "antsr", tmpdir = tdir, fileext = "_InitialAff.mat" )
  EXTRACTION_WARP_OUTPUT_PREFIX<-tempfile( pattern = "antsr", tmpdir = tdir, fileext = "_PriorMap")

  # ANTs parameters begin
  ANTS_MAX_ITERATIONS<-"100x100x70x20"
  ANTS_TRANSFORMATION<-"SyN[0.1,3,0]"
  ANTS_LINEAR_METRIC_PARAMS<-"1,32,Regular,0.25"
  ANTS_LINEAR_CONVERGENCE<-"[1000x1000x1000x1000,1e-8,15]"
  ANTS_METRIC<-"CC"
  ANTS_METRIC_PARAMS<-"1,4"
  # ANTs parameters end

  # Atropos params
  ATROPOS_BRAIN_EXTRACTION_INITIALIZATION<-"kmeans[3]"
  ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD<-"Gaussian"
  ATROPOS_BRAIN_EXTRACTION_CONVERGENCE<-"[3,0.0001]"
  ATROPOS_BRAIN_EXTRACTION_MRF<-paste("[0.2,1x1x1]")
  if ( img@dimension == 2 ) ATROPOS_BRAIN_EXTRACTION_MRF<-paste("[0.2,1x1]")

  ATROPOS_SEGMENTATION_INITIALIZATION<-"PriorProbabilityImages"
  ATROPOS_SEGMENTATION_PRIOR_WEIGHT<-0.0
  ATROPOS_SEGMENTATION_LIKELIHOOD<-"Gaussian"
  ATROPOS_SEGMENTATION_CONVERGENCE<-"[12,0.0001]"
  ATROPOS_SEGMENTATION_POSTERIOR_FORMULATION<-"Socrates"
  ATROPOS_SEGMENTATION_MRF<-"[0.11,1x1x1]";
  if ( img@dimension == 2 ) ATROPOS_SEGMENTATION_MRF<-paste("[0.11,1x1]")
  # Atropos params end
  
  imgsmall<-antsImageClone(img)
  ResampleImageBySpacing(img@dimension,img,imgsmall,as.character(rep(4,img@dimension)),1)
  temsmall<-antsImageClone(tem)
  ResampleImageBySpacing(tem@dimension,tem,temsmall,as.character(rep(4,tem@dimension)),1)
# careful initialization of affine mapping , result stored in initafffn
  antsAffineInitializer(img@dimension,temsmall,imgsmall,initafffn,15,0.1,0,10)
  # FIXME - should add mask in above call 

  # get laplacian images 
  lapi<-antsImageClone( img )
  ImageMath(img@dimension,lapi,"Laplacian",img,1.5,1)
  lapt<-antsImageClone( tem )
  ImageMath(tem@dimension,lapt,"Laplacian",tem,1.5,1)
  # FIXME should add mask to below via -x option
  print( EXTRACTION_WARP_OUTPUT_PREFIX )
  dtem<-antsImageClone( tem, 'double')
  dimg<-antsImageClone( img, 'double')
  antsregparams<-list(  d=img@dimension, u=1,  o=EXTRACTION_WARP_OUTPUT_PREFIX,
                      r=initafffn, z=1 ,  w="[0.025,0.975]",
      m=paste("mattes[",antsrGetPointerName(dtem),",",antsrGetPointerName(dimg),",",
        ANTS_LINEAR_METRIC_PARAMS,"]",sep='') ,
      c=ANTS_LINEAR_CONVERGENCE ,t="Affine[0.1]" ,f="8x4x2x1", s="4x2x1x0",
      m=paste("CC[",antsrGetPointerName(dtem),",",antsrGetPointerName(dimg),",",
        "0.5,4]",sep=''),
#      m=paste("CC[",antsrGetPointerName(lapt),",",antsrGetPointerName(lapi),",",
#        "0.5,4]",sep=''), FIXME
      c="[50x10x0,1e-9,15]",t="SyN[0.1,3,0]",f="4x2x1",s="2x1x0")
  antsRegistration( antsregparams )
  tx<-paste(EXTRACTION_WARP_OUTPUT_PREFIX,c("1InverseWarp.nii.gz","0GenericAffine.mat"),sep='')
  temmaskwarped<-antsImageClone( img, "double" )
  aatparams<-list( d=img@dimension, i=antsImageClone(temmask,'double'), o=temmaskwarped, r=dimg, n="Gaussian", t=paste("[",tx[2],",1]",sep=''), t=tx[1])
  antsApplyTransforms( aatparams )
  ftemmaskwarped<-antsImageClone( temmaskwarped, "float" )
  ThresholdImage(img@dimension,ftemmaskwarped,ftemmaskwarped,0.5,1)
  tmp<-antsImageClone(ftemmaskwarped)
  ImageMath(img@dimension,tmp,"MD",ftemmaskwarped,2)
  ImageMath(img@dimension,tmp,"GetLargestComponent",tmp,2)
  ImageMath(img@dimension,tmp,"FillHoles",tmp)
  seg<-antsImageClone( img , "unsigned int")
  atroparams<-list( d=img@dimension, a=img,  m=ATROPOS_BRAIN_EXTRACTION_MRF, o=seg,  x=antsImageClone(tmp,"unsigned int"), i=ATROPOS_BRAIN_EXTRACTION_INITIALIZATION, c=ATROPOS_BRAIN_EXTRACTION_CONVERGENCE, k=ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD )
  print( atroparams )
  Atropos( atroparams )
  seg<-antsImageClone( seg, 'float') 
  segwm<-antsImageClone(img) 
  ThresholdImage(img@dimension,seg,segwm,3,3)
  seggm<-antsImageClone(img) 
  ThresholdImage(img@dimension,seg,seggm,2,2)
  segcsf<-antsImageClone(img) 
  ThresholdImage(img@dimension,seg,segcsf,1,1)
  ImageMath(img@dimension,segwm,"GetLargestComponent",segwm)
  ImageMath(img@dimension,segwm,"GetLargestComponent",segwm)
  tmp<-antsImageClone( img )
  ImageMath(img@dimension,tmp,"FillHoles",seggm)
  ImageMath(img@dimension,seggm,"m",seggm,tmp)  
  ImageMath(img@dimension,segwm,"m",segwm,3)  
  ImageMath(img@dimension,tmp,"ME",segcsf,10)  
  ImageMath(img@dimension,seggm,"addtozero",seggm,tmp)  
  ImageMath(img@dimension,seggm,"m",seggm,2)  
  finalseg<-antsImageClone( img )
  ImageMath(img@dimension,finalseg,"m",finalseg,0)  
  ImageMath(img@dimension,finalseg,"addtozero",seggm,segwm)  
  ImageMath(img@dimension,finalseg,"addtozero",finalseg,segcsf)
                                        # BA - finalseg looks good! could stop here
  ThresholdImage(img@dimension,finalseg,tmp,2,3)
  ImageMath(img@dimension,tmp,"ME",tmp,2)  
  ImageMath(img@dimension,tmp,"GetLargestComponent",tmp,2)  
  ImageMath(img@dimension,tmp,"MD",tmp,4)  
  ImageMath(img@dimension,tmp,"FillHoles",tmp)  
  ImageMath(img@dimension,tmp,"addtozero",tmp, antsImageClone( temmaskwarped, "float" ) )  
  ImageMath(img@dimension,tmp,"MD",tmp,5)  
  ImageMath(img@dimension,tmp,"ME",tmp,5)
  brain<-antsImageClone(img)
  ImageMath(img@dimension,brain,"m",brain,tmp)  
  return( list( brain=brain, bmask=tmp ) )
}
