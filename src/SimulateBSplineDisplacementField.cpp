#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkSimulatedBSplineDisplacementFieldSource.h"
#include "RcppANTsR.h"


template<class PrecisionType, unsigned int Dimension>
SEXP simulateBSplineDisplacementFieldHelper(
  SEXP r_domainImage,
  SEXP r_outputDisplacementField,
  unsigned int numberOfRandomPoints,
  float standardDeviationDisplacementField,
  bool enforceStationaryBoundary,
  unsigned int numberOfFittingLevels,
  SEXP r_numberOfControlPoints )
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

  using BSplineSimulatorType = itk::SimulatedBSplineDisplacementFieldSource<DisplacementFieldType>;

  typename BSplineSimulatorType::ArrayType ncps;
  Rcpp::NumericVector numberOfControlPoints( r_numberOfControlPoints );
  for( unsigned int d = 0; d < numberOfControlPoints.size(); ++d )
    {
    ncps = numberOfControlPoints[d];
    }

  typename BSplineSimulatorType::Pointer bsplineSimulator = BSplineSimulatorType::New();
  bsplineSimulator->SetDisplacementFieldDomainFromImage( domainImage );
  bsplineSimulator->SetNumberOfRandomPoints( numberOfRandomPoints );
  bsplineSimulator->SetEnforceStationaryBoundary( enforceStationaryBoundary );
  bsplineSimulator->SetDisplacementNoiseStandardDeviation( standardDeviationDisplacementField );
  bsplineSimulator->SetNumberOfFittingLevels( numberOfFittingLevels );
  bsplineSimulator->SetNumberOfControlPoints( ncps );

  typename DisplacementFieldType::Pointer itkField = bsplineSimulator->GetOutput();
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

RcppExport SEXP simulateBSplineDisplacementFieldR(
  SEXP r_domainImage,
  SEXP r_numberOfRandomPoints,
  SEXP r_standardDeviationDisplacementField,
  SEXP r_enforceStationaryBoundary,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints )
{
try
  {
  Rcpp::S4 domainImage( r_domainImage );
  Rcpp::S4 r_outputDisplacementField( r_domainImage );

  unsigned int imageDimension = Rcpp::as<int>( domainImage.slot( "dimension" ) );
  unsigned int numberOfRandomPoints = Rcpp::as<int>( r_numberOfRandomPoints );
  float standardDeviationDisplacementField = Rcpp::as<float>( r_standardDeviationDisplacementField );
  bool enforceStationaryBoundary = Rcpp::as<bool>( r_enforceStationaryBoundary );
  unsigned int numberOfFittingLevels = Rcpp::as<int>( r_numberOfFittingLevels );

  if( imageDimension == 2 )
    {
    using PrecisionType = double;
    const unsigned int imageDimension = 2;
    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, imageDimension>( domainImage,
      r_outputDisplacementField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, numberOfFittingLevels, r_numberOfControlPoints );
    return( outputDisplacementField );
    }
  else if( imageDimension == 3 )
    {
    using PrecisionType = double;
    const unsigned int imageDimension = 3;
    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, imageDimension>( domainImage,
      r_outputDisplacementField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, numberOfFittingLevels, r_numberOfControlPoints );
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
