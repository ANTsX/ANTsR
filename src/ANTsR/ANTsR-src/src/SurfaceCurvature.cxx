
#include <Rcpp.h>


//#include "curvatureapp.h"

#include "itkSurfaceCurvatureBase.h"
#include "itkSurfaceImageCurvature.h"

#include "ReadWriteImage.h"

/*
void test1()
{

typedef itk::SurfaceCurvatureBase<ImageType>  ParamType;
  ParamType::Pointer Parameterizer=ParamType::New();

//  Parameterizer->TestEstimateTangentPlane(p); 
  Parameterizer->FindNeighborhood();
//  Parameterizer->WeightedEstimateTangentPlane(  Parameterizer->GetOrigin() ); 
  
  Parameterizer->EstimateTangentPlane( 
    Parameterizer->GetAveragePoint());
  Parameterizer->PrintFrame();
  

//  Parameterizer->SetOrigin(Parameterizer->GetAveragePoint());

  for(int i=0; i<3; i++){
  Parameterizer->ComputeWeightsAndDirectionalKappaAndAngles
    (Parameterizer->GetOrigin());
  Parameterizer->ComputeFrame(Parameterizer->GetOrigin());
  Parameterizer->EstimateCurvature();
  Parameterizer->PrintFrame();
  }

  Parameterizer->ComputeJoshiFrame(Parameterizer->GetOrigin());  Parameterizer->PrintFrame();
  Rcpp::Rcout << " err 1 " << Parameterizer->ErrorEstimate(Parameterizer->GetOrigin()) << 
    " err 2 " << Parameterizer->ErrorEstimate(Parameterizer->GetOrigin(),-1) << std::endl;
         
}
      

*/
  
RcppExport SEXP SurfaceCurvature( SEXP r_args )
try
{
  // put the arguments coming from R into standard (argc,argv) format;
  // arguments coming from R don't have the command name as first, argument, so add it manually;
  // arguments coming from R may have adjacent arguments concatenated into one argument, 
  // which the parser should handle
  std::deque<std::string> args = Rcpp::as< std::deque<std::string> >( r_args ) ;
  args.push_front( "antsRegistration" ) ;
  
  int argc = args.size() ;
  char** argv = new char*[args.size()+1] ;
  for( int i = 0 ; i < args.size() ; ++i )
    {
      // allocate space for the string plus a null character
      argv[i] = new char[args[i].length()+1] ;
      std::strncpy( argv[i] , args[i].c_str() , args[i].length() ) ;
      // place the null character in the end
      argv[i][args[i].length()] = '\0' ;
    }
  argv[argc] = 0 ;


    if (argc < 3) 
    {
      Rcpp::Rcout << " usage :  SurfaceCurvature FileNameIn FileNameOut sigma option  " << std::endl;
      Rcpp::Rcout << " e.g  :   SurfaceCurvature    BrainIn.nii BrainOut.nii   3  0 " << std::endl;
      Rcpp::Rcout << " option 0 means just compute mean curvature from intensity " << std::endl;
      Rcpp::Rcout << " option 5 means characterize surface from intensity " << std::endl;
      Rcpp::Rcout << " option 6 means compute gaussian curvature " << std::endl;
      Rcpp::Rcout << " ... " << std::endl;
      Rcpp::Rcout << " for surface characterization " << std::endl;
      Rcpp::Rcout << " 1 == (+) bowl "<<std::endl;
      Rcpp::Rcout << " 2 == (-) bowl  "<<std::endl;
      Rcpp::Rcout << " 3 == (+) saddle "<<std::endl;
      Rcpp::Rcout << " 4 == (-) saddle "<<std::endl;
      Rcpp::Rcout << " 5 == (+) U "<<std::endl;
      Rcpp::Rcout << " 6 == (-) U "<<std::endl;
      Rcpp::Rcout << " 7 == flat "<<std::endl;
      Rcpp::Rcout << " 8 == a perfectly even saddle (rare) "<<std::endl;
      Rcpp::Rcout << " " << std::endl;
      Rcpp::Rcout << " we add 128 to mean curvature results s.t. they are differentiated from background (zero) " <<std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
    }
    
  typedef itk::Image<float, 3> ImageType; 
  typedef itk::Image<float, 3> floatImageType; 
  enum { ImageDimension = ImageType::ImageDimension };
  typedef itk::SurfaceImageCurvature<ImageType>  ParamType;
  ParamType::Pointer Parameterizer=ParamType::New(); 
  typedef  ImageType::PixelType PixType;
      

  int opt = 0;
  float sig=1.0;
  if (argc > 3) sig = atof( argv[3]);
  Rcpp::Rcout << " sigma " << sig << std::endl;
  if (argc > 4) opt = (int) atoi(argv[4]);   

  if ( opt < 0)
  {
    Rcpp::Rcout << " error " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }
                  
  ImageType::Pointer input;
  ReadImage<ImageType>(input,argv[1]);
  Rcpp::Rcout << " done reading " << std::string(argv[1]) << std::endl;

  //  float ballradius = 2.0;
  // if (argc >= 6) ballradius = (float) atof(argv[5]);   
  // if (ballradius > 0 && thresh > 0) input = SegmentImage<ImageType>(input, thresh, ballradius);
  
  Parameterizer->SetInput(input);
 
   
  //  Parameterizer->ProcessLabelImage();
  Parameterizer->SetNeighborhoodRadius( 1. );      
//  Rcpp::Rcout << " set sig " ;  std::cin >> sig; 
  if (sig <= 0.5) sig=1.66;
  Rcpp::Rcout << " sigma " << sig << " option " << opt << std::endl;
  Parameterizer->SetSigma(sig);  
    
  if (opt == 1)
    {
      Parameterizer->SetUseLabel(true);
      Parameterizer->SetUseGeodesicNeighborhood(false);
    }
  else
    {
      Parameterizer->SetUseLabel(false);
      Parameterizer->SetUseGeodesicNeighborhood(false);
      float sign=1.0;
      if (opt == 3) sign=-1.0;
      Rcpp::Rcout << " setting outward direction as " << sign;
      Parameterizer->SetkSign(sign);
      Parameterizer->SetThreshold(0);
    }
//  Parameterizer->ComputeSurfaceArea();  
//  Parameterizer->IntegrateFunctionOverSurface();  
//  Parameterizer->IntegrateFunctionOverSurface(true);  
   
  Rcpp::Rcout << " computing frame " << std::endl;         
  if (opt != 5 && opt != 6 ) Parameterizer->ComputeFrameOverDomain( 3 );    
  else Parameterizer->ComputeFrameOverDomain( opt );    
  
  //   Parameterizer->SetNeighborhoodRadius( 2 );                     
  //  Parameterizer->LevelSetMeanCurvature();     
  //  Parameterizer->SetNeighborhoodRadius( 2.9   );        
  //  Parameterizer->IntegrateFunctionOverSurface(false);    
  //  Parameterizer->SetNeighborhoodRadius( 1.5  );       
  //  Parameterizer->IntegrateFunctionOverSurface(true);   
  //   for (int i=0; i<1; i++) Parameterizer->PostProcessGeometry();  
  

  ImageType::Pointer output=NULL;  
  
  //  Parameterizer->GetFunctionImage()->SetSpacing( input->GetSpacing() );
  //  Parameterizer->GetFunctionImage()->SetDirection( input->GetDirection() );
  //  Parameterizer->GetFunctionImage()->SetOrigin( input->GetOrigin() );
  //  smooth->SetSpacing(reader->GetOutput()->GetSpacing());
  // SmoothImage(Parameterizer->GetFunctionImage(),smooth,3);
  // NormalizeImage(smooth,output,mn);          
  //  NormalizeImage(Parameterizer->GetFunctionImage(),output,mn);

  WriteImage<floatImageType>(Parameterizer->GetFunctionImage() , argv[2]);
    
  Rcpp::Rcout << " done writing ";

  // cleanup of argv
  for( int i = 0 ; i < args.size() ; ++i )
    {
      delete[] argv[i] ;
    }
  delete[] argv ;

  return Rcpp::wrap( EXIT_SUCCESS ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( EXIT_FAILURE ) ;
   }

