#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkSimulatedExponentialDisplacementFieldSource.h"
#include "RcppANTsR.h"


template<class PrecisionType, unsigned int Dimension>
SEXP simulateExponentialDisplacementFieldHelper(
  SEXP r_domainImage,
  SEXP r_outputDisplacementField,
  unsigned int numberOfRandomPoints,
  float standardDeviationDisplacementField,
  bool enforceStationaryBoundary,
  float standardDeviationSmoothing )
{
  using PixelType = PrecisionType;
  using ImageType = itk::Image<PixelType, Dimension>;
  using VectorType = itk::Vector<PrecisionType, Dimension>;
  using DisplacementFieldType = itk::Image<VectorType, Dimension>;
  using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
  using DisplacementFieldPointerType = typename DisplacementFieldType::Pointer;
  using ANTsrFieldType = itk::VectorImage<PrecisionType, Dimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<DisplacementFieldType>;

  using ImagePointerType = typename ImageType::Pointer;
  using DisplacementFieldPointerType = typename DisplacementFieldType::Pointer;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  ImagePointerType domainImage = Rcpp::as<ImagePointerType>( r_domainImage );
  ANTsRFieldPointerType outputDisplacementField = Rcpp::as<ANTsRFieldPointerType>( r_outputDisplacementField );

  using ExponentialSimulatorType = itk::SimulatedExponentialDisplacementFieldSource<DisplacementFieldType>;

  typename ExponentialSimulatorType::Pointer exponentialSimulator = ExponentialSimulatorType::New();
  exponentialSimulator->SetDisplacementFieldDomainFromImage( domainImage );
  exponentialSimulator->SetNumberOfRandomPoints( numberOfRandomPoints );
  exponentialSimulator->SetEnforceStationaryBoundary( enforceStationaryBoundary );
  exponentialSimulator->SetDisplacementNoiseStandardDeviation( standardDeviationDisplacementField );
  exponentialSimulator->SetSmoothingStandardDeviation( standardDeviationSmoothing );

  typename DisplacementFieldType::Pointer itkField = exponentialSimulator->GetOutput();
  itkField->Update();
  itkField->DisconnectPipeline();

  IteratorType It( itkField, itkField->GetLargestPossibleRegion() );
  while( !It.IsAtEnd() )
    {
    VectorType itkVector = itkField->GetPixel( It.GetIndex() );

    typename ANTsrFieldType::PixelType antsrVector;
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      antsrVector[d] = itkVector[d];
      }
    outputDisplacementField->SetPixel( It.GetIndex(), antsrVector );
    ++It;
    }

  r_outputDisplacementField = Rcpp::wrap( outputDisplacementField );
  return( r_outputDisplacementField );
}

RcppExport SEXP simulateExponentialDisplacementFieldR(
  SEXP r_domainImage,
  SEXP r_numberOfRandomPoints,
  SEXP r_standardDeviationDisplacementField,
  SEXP r_enforceStationaryBoundary,
  SEXP r_standardDeviationSmoothing )
{
try
  {
  Rcpp::S4 domainImage( r_domainImage );
  Rcpp::S4 r_outputDisplacementField( r_domainImage );

  unsigned int imageDimension = Rcpp::as<int>( domainImage.slot( "dimension" ) );
  unsigned int numberOfRandomPoints = Rcpp::as<int>( r_numberOfRandomPoints );
  float standardDeviationDisplacementField = Rcpp::as<float>( r_standardDeviationDisplacementField );
  bool enforceStationaryBoundary = Rcpp::as<bool>( r_enforceStationaryBoundary );
  float standardDeviationSmoothing = Rcpp::as<int>( r_standardDeviationSmoothing );

  if( imageDimension == 2 )
    {
    using PrecisionType = double;
    const unsigned int imageDimension = 2;
    SEXP outputDisplacementField =
      simulateExponentialDisplacementFieldHelper<PrecisionType, imageDimension>( domainImage,
      r_outputDisplacementField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, standardDeviationSmoothing );
    return( outputDisplacementField );
    }
  else if( imageDimension == 3 )
    {
    using PrecisionType = double;
    const unsigned int imageDimension = 3;
    SEXP outputDisplacementField =
      simulateExponentialDisplacementFieldHelper<PrecisionType, imageDimension>( domainImage,
      r_outputDisplacementField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, standardDeviationSmoothing );
    return( outputDisplacementField );
    }
  else
    {
    Rcpp::stop( "Unsupported image dimension." );
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
