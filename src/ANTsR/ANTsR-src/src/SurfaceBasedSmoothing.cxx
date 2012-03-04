
#include <Rcpp.h>

#include "itkBinaryThresholdImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "ReadWriteImage.h"
#include "itkSurfaceImageCurvature.h"

RcppExport SEXP SurfaceBasedSmoothing( SEXP r_args )
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
      Rcpp::Rcout << " usage :  " << argv[0] << " ImageToSmooth  sigma SurfaceImage  outname  {numrepeatsofsmoothing}" << std::endl;
      Rcpp::Rcout << " We assume the SurfaceImage has a label == 1 that defines the surface " << std::endl;
      Rcpp::Rcout <<   " sigma  defines the geodesic n-hood radius --- numrepeats allows one to use " << std::endl;
      Rcpp::Rcout << " a small geodesic n-hood repeatedly applied many times -- faster computation, same effect " << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
    }
  typedef itk::Image<float, 3> ImageType; 
  enum { ImageDimension = ImageType::ImageDimension };
  typedef itk::Image<float, ImageDimension> floatImageType; 
  typedef itk::SurfaceImageCurvature<ImageType>  ParamType;
  ParamType::Pointer Parameterizer=ParamType::New(); 

  typedef  itk::ImageFileReader< ImageType >      ReaderType;
  typedef  ImageType::PixelType PixType;
      
//  std::string fn="C://Data//brain15labelimage.img";  
  std::string ext=".nii";

  float opt = 0;
  float sig=1.0;
  //  float thresh=0.0;
  if (argc > 2) sig = atof( argv[2]);
  unsigned int numrepeats=0;
  if (argc > 5) numrepeats=atoi(argv[5]);
         
  ImageType::Pointer input;
  ReadImage<ImageType>(input, argv[1]);
  ImageType::Pointer surflabel;
  ReadImage<ImageType>(surflabel,argv[3]);

  Parameterizer->SetInput(surflabel);
  Parameterizer->SetFunctionImage(input);
  Parameterizer->SetNeighborhoodRadius( sig );      
  if (sig <= 0) sig=1.0;
  Rcpp::Rcout << " sigma " << sig << " thresh " << opt << std::endl;
  Parameterizer->SetSigma(sig);  
  Parameterizer->SetUseGeodesicNeighborhood(true);
  Parameterizer->SetUseLabel(true);
  Parameterizer->SetThreshold(0.5);
  //  Parameterizer->ComputeSurfaceArea();  
  //  Parameterizer->IntegrateFunctionOverSurface();

  Parameterizer->SetNeighborhoodRadius( sig );	 
  Rcpp::Rcout <<" begin integration NOW " << std::endl;  
  Parameterizer->IntegrateFunctionOverSurface(true);     
  for (unsigned int i=0; i<numrepeats; i++) 
    Parameterizer->IntegrateFunctionOverSurface(true);    
  Rcpp::Rcout <<" end integration  " << std::endl;
  // Parameterizer->PostProcessGeometry();  
  
  //  double mn=0.0;          
  ImageType::Pointer output=NULL;  
  floatImageType::Pointer smooth=NULL; 
  smooth=Parameterizer->GetFunctionImage();

  std::string fnname = std::string(argv[1]).substr(0,std::string(argv[1]).length()-4);
  std::string ofn = std::string(argv[4]);
  Rcpp::Rcout << " writing result " << ofn <<  std::endl;      
  //writer->SetFileName(ofn.c_str());   
  //  writer->SetInput( smooth );     
  WriteImage<ImageType>(smooth,ofn.c_str());
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

