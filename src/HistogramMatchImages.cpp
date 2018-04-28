#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkHistogramMatchingImageFilter.h"
#include "RcppANTsR.h"


template<class ImageType>
SEXP histogramMatchImageHelper(
  SEXP r_sourceImage,
  SEXP r_referenceImage,
  SEXP r_outputImage,
  unsigned int numberOfHistogramBins,
  unsigned int numberOfMatchPoints,
  bool useThresholdAtMeanIntensity )
{
  enum { ImageDimension = ImageType::ImageDimension };
  
  typedef typename ImageType::Pointer            ImagePointerType;

  typename ImageType::Pointer sourceImage = Rcpp::as<ImagePointerType>( r_sourceImage );
  typename ImageType::Pointer referenceImage = Rcpp::as<ImagePointerType>( r_referenceImage );
  typename ImageType::Pointer outputImage = Rcpp::as< ImagePointerType >( r_outputImage );

  typedef itk::HistogramMatchingImageFilter<ImageType, ImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetSourceImage( sourceImage );
  filter->SetReferenceImage( referenceImage );
  filter->ThresholdAtMeanIntensityOff();
  if( useThresholdAtMeanIntensity )
    {
    filter->ThresholdAtMeanIntensityOn();
    }
  filter->SetNumberOfHistogramLevels( numberOfHistogramBins );
  filter->SetNumberOfMatchPoints( numberOfMatchPoints );

  outputImage = filter->GetOutput();
  outputImage->Update();
  outputImage->DisconnectPipeline();

  r_outputImage = Rcpp::wrap( outputImage  );
  return( r_outputImage );
}

RcppExport SEXP histogramMatchImageR(
  SEXP r_sourceImage,
  SEXP r_referenceImage,
  SEXP r_numberOfHistogramBins,
  SEXP r_numberOfMatchPoints,
  SEXP r_useThresholdAtMeanIntensity )
{

try
  {
  Rcpp::S4 sourceImage( r_sourceImage );
  Rcpp::S4 referenceImage( r_referenceImage );
  Rcpp::S4 r_outputImage( r_sourceImage );

  std::string pixeltype = Rcpp::as< std::string >( sourceImage.slot( "pixeltype" ) );

  unsigned int imageDimension = Rcpp::as<int>( sourceImage.slot( "dimension" ) );
  unsigned int numberOfHistogramBins = Rcpp::as<int>( r_numberOfHistogramBins );
  unsigned int numberOfMatchPoints = Rcpp::as<int>( r_numberOfMatchPoints );
  bool useThresholdAtMeanIntensity = Rcpp::as<bool>( r_useThresholdAtMeanIntensity );

  if ( ( pixeltype == "float" ) & ( imageDimension == 2 ) )
    {
    typedef float PixelType;
    const unsigned int imageDimension = 2;
    typedef itk::Image<PixelType, imageDimension> ImageType;
    SEXP outputImage = histogramMatchImageHelper<ImageType>( sourceImage, referenceImage, 
      r_outputImage, numberOfHistogramBins, numberOfMatchPoints, useThresholdAtMeanIntensity );
    return( outputImage );
    }
  else if ( ( pixeltype == "float") & ( imageDimension == 3 ) )
    {
    typedef float PixelType;
    const unsigned int imageDimension = 3;
    typedef itk::Image<PixelType, imageDimension> ImageType;
    SEXP outputImage = histogramMatchImageHelper<ImageType>( sourceImage, referenceImage, 
      r_outputImage, numberOfHistogramBins, numberOfMatchPoints, useThresholdAtMeanIntensity );
    return( outputImage );
    }
  else if ( ( pixeltype == "float" ) & ( imageDimension == 4 ) )
    {
    typedef float PixelType;
    const unsigned int imageDimension = 4;
    typedef itk::Image<PixelType, imageDimension> ImageType;
    SEXP outputImage = histogramMatchImageHelper<ImageType>( sourceImage, referenceImage, 
      r_outputImage, numberOfHistogramBins, numberOfMatchPoints, useThresholdAtMeanIntensity );
    return( outputImage );
    }
  else
    {
    Rcpp::stop( "Unsupported image dimension or pixel type." );
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
