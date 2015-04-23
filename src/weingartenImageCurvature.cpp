#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "ants.h"


template< class ImageType >
SEXP imagek( SEXP r_antsimage, SEXP r_sigma, SEXP r_opt )
{
  typedef typename ImageType::Pointer       ImagePointerType;
  enum { ImageDimension = ImageType::ImageDimension };
  typedef itk::SurfaceImageCurvature<ImageType> ParamType;
  typename ParamType::Pointer Parameterizer = ParamType::New();
  int   opt = Rcpp::as< int >( r_opt );
  float sig = Rcpp::as< float >( r_sigma );
  typename ImageType::Pointer input = Rcpp::as<ImagePointerType>( r_antsimage );
  typename ImageType::DirectionType imgdir = input->GetDirection();
  typename ImageType::DirectionType iddir = input->GetDirection();
  iddir.SetIdentity();
  input->SetDirection( iddir );
  Parameterizer->SetInputImage(input);
  Parameterizer->SetNeighborhoodRadius( 1. );
  if( sig <= 0.5 )
      {
      sig = 1.66;
      }
  Parameterizer->SetSigma(sig);
  Parameterizer->SetUseLabel(false);
  Parameterizer->SetUseGeodesicNeighborhood(false);
  float sign = 1.0;
  Parameterizer->SetkSign(sign);
  Parameterizer->SetThreshold(0);
  if( opt != 5 && opt != 6 )
      {
      Parameterizer->ComputeFrameOverDomain( 3 );
      }
  else
      {
      Parameterizer->ComputeFrameOverDomain( opt );
      }
  typename ImageType::Pointer output = Parameterizer->GetFunctionImage();
  output->SetDirection( imgdir );
  return Rcpp::wrap( output );
}


RcppExport SEXP weingartenImageCurvature( SEXP r_antsimage,
  SEXP r_sigma, SEXP r_opt )
{
try
{
  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if ( ( pixeltype == "float" ) & ( dimension == 3 ) )
    {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image<PixelType,dim>       ImageType;
    return imagek<ImageType>( r_antsimage, r_sigma, r_opt );
    }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  Rcpp::Rcout << "STD ExceptionObject caught !" << std::endl;
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
