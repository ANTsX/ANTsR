#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"

#include "itkAdditiveGaussianNoiseImageFilter.h"
#include "itkSaltAndPepperNoiseImageFilter.h"
#include "itkShotNoiseImageFilter.h"
#include "itkSpeckleNoiseImageFilter.h"

#include "RcppANTsR.h"


template<class PrecisionType, unsigned int Dimension>
SEXP AdditiveGaussianNoiseImageHelper(
  SEXP r_inputImage,
  SEXP r_outputImage,
  float mean,
  float standardDeviation )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;

  using ImagePointerType = typename ImageType::Pointer;

  typename ImageType::Pointer inputImage = Rcpp::as< ImagePointerType >( r_inputImage );
  typename ImageType::Pointer outputImage = Rcpp::as< ImagePointerType >( r_outputImage );

  using NoiseFilterType = itk::AdditiveGaussianNoiseImageFilter<ImageType, ImageType>;
  typename NoiseFilterType::Pointer noiser = NoiseFilterType::New();
  noiser->SetInput( inputImage );
  noiser->SetMean( mean );
  noiser->SetStandardDeviation( standardDeviation );

  outputImage = noiser->GetOutput();
  outputImage->Update();
  outputImage->DisconnectPipeline();

  r_outputImage = Rcpp::wrap( outputImage  );
  return( r_outputImage );
}

template<class PrecisionType, unsigned int Dimension>
SEXP SaltAndPepperNoiseImageHelper(
  SEXP r_inputImage,
  SEXP r_outputImage,
  float probability,
  float saltValue,
  float pepperValue )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;

  using ImagePointerType = typename ImageType::Pointer;

  typename ImageType::Pointer inputImage = Rcpp::as< ImagePointerType >( r_inputImage );
  typename ImageType::Pointer outputImage = Rcpp::as< ImagePointerType >( r_outputImage );

  using NoiseFilterType = itk::SaltAndPepperNoiseImageFilter<ImageType, ImageType>;
  typename NoiseFilterType::Pointer noiser = NoiseFilterType::New();
  noiser->SetInput( inputImage );
  noiser->SetProbability( probability );
  noiser->SetSaltValue( saltValue );
  noiser->SetPepperValue( pepperValue );

  outputImage = noiser->GetOutput();
  outputImage->Update();
  outputImage->DisconnectPipeline();

  r_outputImage = Rcpp::wrap( outputImage  );
  return( r_outputImage );
}

template<class PrecisionType, unsigned int Dimension>
SEXP ShotNoiseImageHelper(
  SEXP r_inputImage,
  SEXP r_outputImage,
  float scale )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;

  using ImagePointerType = typename ImageType::Pointer;

  typename ImageType::Pointer inputImage = Rcpp::as< ImagePointerType >( r_inputImage );
  typename ImageType::Pointer outputImage = Rcpp::as< ImagePointerType >( r_outputImage );

  using NoiseFilterType = itk::ShotNoiseImageFilter<ImageType, ImageType>;
  typename NoiseFilterType::Pointer noiser = NoiseFilterType::New();
  noiser->SetInput( inputImage );
  noiser->SetScale( scale );

  outputImage = noiser->GetOutput();
  outputImage->Update();
  outputImage->DisconnectPipeline();

  r_outputImage = Rcpp::wrap( outputImage  );
  return( r_outputImage );
}

template<class PrecisionType, unsigned int Dimension>
SEXP SpeckleNoiseImageHelper(
  SEXP r_inputImage,
  SEXP r_outputImage,
  float standardDeviation )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;

  using ImagePointerType = typename ImageType::Pointer;

  typename ImageType::Pointer inputImage = Rcpp::as< ImagePointerType >( r_inputImage );
  typename ImageType::Pointer outputImage = Rcpp::as< ImagePointerType >( r_outputImage );

  using NoiseFilterType = itk::SpeckleNoiseImageFilter<ImageType, ImageType>;
  typename NoiseFilterType::Pointer noiser = NoiseFilterType::New();
  noiser->SetInput( inputImage );
  noiser->SetStandardDeviation( standardDeviation );

  outputImage = noiser->GetOutput();
  outputImage->Update();
  outputImage->DisconnectPipeline();

  r_outputImage = Rcpp::wrap( outputImage  );
  return( r_outputImage );
}

RcppExport SEXP addNoiseToImageR(
  SEXP r_inputImage,
  SEXP r_whichNoiseModel,
  SEXP r_parameters )
{
try
  {
  Rcpp::S4 inputImage( r_inputImage );
  Rcpp::S4 s4_outputImage( r_inputImage );
  SEXP outputImage;

  unsigned int imageDimension = Rcpp::as<int>( inputImage.slot( "dimension" ) );
  std::string pixelType = Rcpp::as<std::string>( inputImage.slot( "pixeltype" ) );

  unsigned int whichNoiseModel = Rcpp::as<int>( r_whichNoiseModel );
  Rcpp::NumericVector parameters( r_parameters );

  if( pixelType.compare( "float" ) == 0 && imageDimension == 2 )
    {
    typedef float PrecisionType;
    const unsigned int ImageDimension = 2;

    switch( whichNoiseModel )
      {
      case 0:  // additive gaussian
        outputImage = AdditiveGaussianNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0], parameters[1] );
        return( outputImage );  
        break;
      case 1:  // salt and pepper
        outputImage = SaltAndPepperNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0], parameters[1], parameters[2] );
        return( outputImage );  
        break;
      case 2:  // shot
        outputImage = ShotNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0] );
        return( outputImage );  
        break;
      case 3:  // speckle
        outputImage = SpeckleNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0] );
        return( outputImage );  
        break;
      default:
        Rcpp::stop( "Unsupported image dimension." );
        break;
      }  
    }
  else if( pixelType.compare( "float" ) == 0 && imageDimension == 3 )
    {
    typedef float PrecisionType;
    const unsigned int ImageDimension = 3;

    switch( whichNoiseModel )
      {
      case 0:  // additive gaussian
        outputImage = AdditiveGaussianNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0], parameters[1] );
        return( outputImage );  
        break;
      case 1:  // salt and pepper
        outputImage = SaltAndPepperNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0], parameters[1], parameters[2] );
        return( outputImage );  
        break;
      case 2:  // shot
        outputImage = ShotNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0] );
        return( outputImage );  
        break;
      case 3:  // speckle
        outputImage = SpeckleNoiseImageHelper<PrecisionType, ImageDimension>( 
          inputImage, s4_outputImage, parameters[0] );
        return( outputImage );  
        break;
      default:
        Rcpp::stop( "Unsupported image dimension." );
        break;
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
