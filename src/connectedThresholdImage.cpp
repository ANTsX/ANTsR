#include <exception>
#include <algorithm>
#include <vector>
#include "RcppANTsR.h"
#include <ants.h>
#include "itkConnectedThresholdImageFilter.h"

template< class ImageType >
SEXP connectedThresholdImageHelper(
    SEXP r_inimg,
    SEXP r_outimg,
    Rcpp::IntegerVector seed,
    Rcpp::NumericVector upper,
    Rcpp::NumericVector lower
    )
{
  
  typedef typename ImageType::Pointer ImagePointerType;
  typename ImageType::Pointer inimg =
    Rcpp::as< ImagePointerType >( r_inimg );
  typename ImageType::Pointer outimg =
    Rcpp::as< ImagePointerType >( r_outimg );
  typedef itk::ConnectedThresholdImageFilter< ImageType, ImageType >
    ConnectedThresholdImageFilterType; 
  typename ConnectedThresholdImageFilterType::Pointer filter =
    ConnectedThresholdImageFilterType::New();

  typedef typename ImageType::PixelType     PixelType;

  filter->SetUpper( (PixelType) upper[0]);
  filter->SetLower( (PixelType) lower[0]);

  typename ImageType::IndexType seed2;
  for( int i = 0 ; i < seed.size(); ++i )
    {
    seed2[i] = seed[i] - 1; //0 indexed
  }
  filter->SetReplaceValue(1);
  filter->SetSeed(seed2);
  filter->SetInput( inimg );
  filter->Update();
  outimg = filter->GetOutput();
  r_outimg = Rcpp::wrap( outimg );
  return( r_outimg );
}

RcppExport SEXP connectedThresholdImage( 
    SEXP r_inimg,
    SEXP r_outimg,
    SEXP r_seed,
    SEXP r_upper,
    SEXP r_lower)
{
try
{
  Rcpp::S4 antsImage( r_inimg );
  std::string pixeltype = Rcpp::as< std::string >( antsImage.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< int >( antsImage.slot( "dimension" ) );
  Rcpp::IntegerVector seed( r_seed );
  Rcpp::NumericVector upper( r_upper );
  Rcpp::NumericVector lower( r_lower );
  

  if ( (pixeltype == "float") & ( dimension == 2 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 2;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = connectedThresholdImageHelper< ImageType >(
        r_inimg, r_outimg, seed, upper, lower);
    return( outimg );
  }
  else if ( ( pixeltype == "float" ) & ( dimension == 3 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = connectedThresholdImageHelper< ImageType >(
        r_inimg, r_outimg, seed, upper, lower);
    return( outimg );
  }
  else if ( ( pixeltype == "float" ) & ( dimension  == 4 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 4;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = connectedThresholdImageHelper< ImageType >(
        r_inimg, r_outimg, seed, upper, lower);
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
