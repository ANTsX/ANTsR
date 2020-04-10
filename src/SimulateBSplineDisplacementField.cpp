#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkCastImageFilter.h"
#include "itkSimulatedBSplineDisplacementFieldSource.h"
#include "RcppANTsR.h"


template<class PrecisionType, unsigned int Dimension>
SEXP simulateBSplineDisplacementFieldHelper(
  SEXP r_domainImage,
  SEXP r_antsrField,
  unsigned int numberOfRandomPoints,
  float standardDeviationDisplacementField,
  bool enforceStationaryBoundary,
  unsigned int numberOfFittingLevels,
  SEXP r_numberOfControlPoints )
{
  using ImageType = itk::Image<PrecisionType, Dimension>;
  using VectorType = itk::Vector<PrecisionType, Dimension>;
  using DisplacementFieldType = itk::Image<VectorType, Dimension>;
  using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<DisplacementFieldType>;

  using ImagePointerType = typename ImageType::Pointer;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  ImagePointerType domainImage = Rcpp::as<ImagePointerType>( r_domainImage );
  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

  using BSplineSimulatorType = itk::SimulatedBSplineDisplacementFieldSource<DisplacementFieldType>;

  typename BSplineSimulatorType::ArrayType ncps;
  Rcpp::NumericVector numberOfControlPoints( r_numberOfControlPoints );
  for( unsigned int d = 0; d < numberOfControlPoints.size(); ++d )
    {
    ncps = numberOfControlPoints[d];
    }

  using RealImageType = typename BSplineSimulatorType::RealImageType;
  using CastImageFilterType = itk::CastImageFilter<ImageType, RealImageType>;
  typename CastImageFilterType::Pointer caster = CastImageFilterType::New();
  caster->SetInput( domainImage );
  caster->Update();

  typename BSplineSimulatorType::Pointer bsplineSimulator = BSplineSimulatorType::New();
  bsplineSimulator->SetDisplacementFieldDomainFromImage( caster->GetOutput() );
  bsplineSimulator->SetNumberOfRandomPoints( numberOfRandomPoints );
  bsplineSimulator->SetEnforceStationaryBoundary( enforceStationaryBoundary );
  bsplineSimulator->SetDisplacementNoiseStandardDeviation( standardDeviationDisplacementField );
  bsplineSimulator->SetNumberOfFittingLevels( numberOfFittingLevels );
  bsplineSimulator->SetNumberOfControlPoints( ncps );
  bsplineSimulator->Update();

  IteratorType It( bsplineSimulator->GetOutput(), 
    bsplineSimulator->GetOutput()->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    VectorType itkVector = It.Value();

    typename ANTsRFieldType::PixelType antsrVector( Dimension );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      antsrVector[d] = itkVector[d];
      }
    antsrField->SetPixel( It.GetIndex(), antsrVector );
    }

  r_antsrField = Rcpp::wrap( antsrField );
  return( r_antsrField );
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
  Rcpp::S4 s4_domainImage( r_domainImage );
  unsigned int imageDimension = Rcpp::as<int>( s4_domainImage.slot( "dimension" ) );
  std::string pixelType = Rcpp::as< std::string >( s4_domainImage.slot( "pixeltype" ) );

  unsigned int numberOfRandomPoints = Rcpp::as<int>( r_numberOfRandomPoints );
  float standardDeviationDisplacementField = Rcpp::as<float>( r_standardDeviationDisplacementField );
  bool enforceStationaryBoundary = Rcpp::as<bool>( r_enforceStationaryBoundary );
  unsigned int numberOfFittingLevels = Rcpp::as<int>( r_numberOfFittingLevels );

  if( pixelType.compare( "float" ) == 0 && imageDimension == 2 )
    {
    using PrecisionType = float;
    const unsigned int Dimension = 2;

    using ImageType = itk::Image<PrecisionType, Dimension>;
    using ImagePointerType = typename ImageType::Pointer;
    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ImagePointerType domainImage = Rcpp::as<ImagePointerType>( s4_domainImage );

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->CopyInformation( domainImage );
    antsrField->SetRegions( domainImage->GetRequestedRegion() );
    antsrField->SetVectorLength( imageDimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, Dimension>( s4_domainImage,
      s4_antsrField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, numberOfFittingLevels, r_numberOfControlPoints );
    return( outputDisplacementField );
    }
  else if( pixelType.compare( "float" ) == 0 && imageDimension == 3 )
    {
    using PrecisionType = float;
    const unsigned int Dimension = 3;

    using ImageType = itk::Image<PrecisionType, Dimension>;
    using ImagePointerType = typename ImageType::Pointer;
    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ImagePointerType domainImage = Rcpp::as<ImagePointerType>( s4_domainImage );

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->CopyInformation( domainImage );
    antsrField->SetRegions( domainImage->GetRequestedRegion() );
    antsrField->SetVectorLength( imageDimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, Dimension>( s4_domainImage,
      s4_antsrField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, numberOfFittingLevels, r_numberOfControlPoints );
    return( outputDisplacementField );
    }
  else if( pixelType.compare( "double" ) == 0 && imageDimension == 2 )
    {
    using PrecisionType = double;
    const unsigned int Dimension = 2;

    using ImageType = itk::Image<PrecisionType, Dimension>;
    using ImagePointerType = typename ImageType::Pointer;
    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ImagePointerType domainImage = Rcpp::as<ImagePointerType>( s4_domainImage );

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->CopyInformation( domainImage );
    antsrField->SetRegions( domainImage->GetRequestedRegion() );
    antsrField->SetVectorLength( imageDimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, Dimension>( s4_domainImage,
      s4_antsrField, numberOfRandomPoints, standardDeviationDisplacementField,
      enforceStationaryBoundary, numberOfFittingLevels, r_numberOfControlPoints );
    return( outputDisplacementField );
    }
  else if( pixelType.compare( "double" ) == 0 && imageDimension == 3 )
    {
    using PrecisionType = double;
    const unsigned int Dimension = 3;

    using ImageType = itk::Image<PrecisionType, Dimension>;
    using ImagePointerType = typename ImageType::Pointer;
    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ImagePointerType domainImage = Rcpp::as<ImagePointerType>( s4_domainImage );

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->CopyInformation( domainImage );
    antsrField->SetRegions( domainImage->GetRequestedRegion() );
    antsrField->SetVectorLength( imageDimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputDisplacementField =
      simulateBSplineDisplacementFieldHelper<PrecisionType, Dimension>( s4_domainImage,
      s4_antsrField, numberOfRandomPoints, standardDeviationDisplacementField,
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
