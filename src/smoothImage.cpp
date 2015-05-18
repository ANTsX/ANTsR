#include <exception>
#include <algorithm>
#include <vector>
#include "RcppANTsR.h"
#include <ants.h>
#include "itkDiscreteGaussianImageFilter.h"

template< class ImageType >
SEXP smoothImageHelper(
    SEXP r_inimg,
    SEXP r_outimg,
    Rcpp::NumericVector sigma,
    bool sigmaInPhysicalCoordinates)
{
  typedef typename ImageType::Pointer ImagePointerType;
  typename ImageType::Pointer inimg =
    Rcpp::as< ImagePointerType >( r_inimg );
  typename ImageType::Pointer outimg =
    Rcpp::as< ImagePointerType >( r_outimg );
  typedef itk::DiscreteGaussianImageFilter< ImageType, ImageType >
    discreteGaussianImageFilterType;
  typename discreteGaussianImageFilterType::Pointer filter =
    discreteGaussianImageFilterType::New();
  const unsigned int ImageDimension = ImageType::ImageDimension;
  if ( !sigmaInPhysicalCoordinates )
  {
    filter->SetUseImageSpacingOff();
  }
  else
  {
    filter->SetUseImageSpacingOn();
  }
  if ( sigma.size() == 1 )
  {
    filter->SetVariance( sigma[0] * sigma[0]);
  }
  else if ( sigma.size() == ImageDimension )
  {
    typename discreteGaussianImageFilterType::ArrayType varianceArray;
    for( unsigned int d = 0; d < ImageDimension; d++ )
    {
      varianceArray[d] = sigma[d] * sigma[d];
    }
    filter->SetVariance( varianceArray );
  }
  filter->SetMaximumError( 0.01f );
  filter->SetInput( inimg );
  filter->Update();
  outimg = filter->GetOutput();
  r_outimg = Rcpp::wrap( outimg );
  return( r_outimg );
}

RcppExport SEXP smoothImage( SEXP r_inimg,
    SEXP r_outimg,
    SEXP r_sigma,
    SEXP sigmaInPhysicalCoordinates )
{
try
{
  Rcpp::S4 antsImage( r_inimg );
  std::string pixeltype = Rcpp::as< std::string >( antsImage.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< int >( antsImage.slot( "dimension" ) );
  bool physicalSpacing = Rcpp::as< bool >( sigmaInPhysicalCoordinates );
  Rcpp::NumericVector sigma( r_sigma );
  if ( (pixeltype == "float") & ( dimension == 2 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 2;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = smoothImageHelper< ImageType >(
        r_inimg, r_outimg, sigma, physicalSpacing);
    return( outimg );
  }
  else if ( ( pixeltype == "float" ) & ( dimension == 3 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = smoothImageHelper< ImageType >(
        r_inimg, r_outimg, sigma, physicalSpacing);
    return( outimg );
  }
  else if ( ( pixeltype == "float" ) & ( dimension  == 4 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = smoothImageHelper< ImageType >(
        r_inimg, r_outimg, sigma, physicalSpacing);
    return (outimg);
  }
  else
  {
    Rcpp::stop("Unsupported image dimension or pixel type.");
  }
}

catch( itk::ExceptionObject & err )
{
  Rcpp::Rcout << "ITK ExceptionObject caught!" << std::endl;
  forward_exception_to_r( err );
}
catch( const std::exception& exc )
{
  Rcpp::Rcout << "STD ExceptionObject caught!" << std::endl;
  forward_exception_to_r( exc );
}
catch( ... )
{
  Rcpp::stop( "C++ exception (unknown reason)");
}
 return Rcpp::wrap(NA_REAL); // should not be reached
}
