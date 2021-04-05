#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"

#include "itkHausdorffDistanceImageFilter.h"

#include "RcppANTsR.h"


template<class PrecisionType, unsigned int Dimension>
SEXP HausdorffDistanceHelper(
  SEXP r_inputImage1,
  SEXP r_inputImage2 )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;

  using ImagePointerType = typename ImageType::Pointer;

  typename ImageType::Pointer inputImage1 = Rcpp::as<ImagePointerType>( r_inputImage1 );
  typename ImageType::Pointer inputImage2 = Rcpp::as<ImagePointerType>( r_inputImage2 );

  using HausdorffFilterType = itk::HausdorffDistanceImageFilter<ImageType, ImageType>;
  typename HausdorffFilterType::Pointer hausdorff = HausdorffFilterType::New();
  hausdorff->SetInput1( inputImage1 );
  hausdorff->SetInput2( inputImage2 );
  hausdorff->SetUseImageSpacing( true );
  hausdorff->Update();

  typename HausdorffFilterType::RealType hausdorffDistance = hausdorff->GetHausdorffDistance();
  typename HausdorffFilterType::RealType averageHausdorffDistance = hausdorff->GetAverageHausdorffDistance();

  Rcpp::DataFrame hausdorffDistances =
    Rcpp::DataFrame::create( Rcpp::Named( "Distance" ) = hausdorffDistance,
                             Rcpp::Named( "AverageDistance" ) = averageHausdorffDistance
                           );
  return( hausdorffDistances );
}

RcppExport SEXP HausdorffDistanceR(
  SEXP r_inputImage1,
  SEXP r_inputImage2 )
{
try
  {
  Rcpp::S4 inputImage1( r_inputImage1 );
  Rcpp::S4 inputImage2( r_inputImage2 );

  unsigned int imageDimension = Rcpp::as<int>( inputImage1.slot( "dimension" ) );
  std::string pixelType = Rcpp::as<std::string>( inputImage1.slot( "pixeltype" ) );

  if( imageDimension == 2 )
    {
    const unsigned int ImageDimension = 2;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      }
    } else if( imageDimension == 3 ) {
    const unsigned int ImageDimension = 3;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      }
    } else if( imageDimension == 4 ) {
    const unsigned int ImageDimension = 4;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP H = HausdorffDistanceHelper<PrecisionType, ImageDimension>( inputImage1, inputImage2 );
      return( H );
      }
    }
  else
    {
    Rcpp::stop( "Unsupported image dimension or pixeltype." );
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
  Rcpp::stop( "C++ exception (unknown reason)" );
  }

return Rcpp::wrap( NA_REAL ); // should not be reached
}
