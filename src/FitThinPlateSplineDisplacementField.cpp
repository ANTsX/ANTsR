#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "RcppANTsR.h"

template<unsigned int Dimension>
SEXP fitThinPlateSplineVectorImageHelper(
  SEXP r_displacementOrigins,
  SEXP r_displacements,
  SEXP r_antsrField,
  SEXP r_origin,
  SEXP r_spacing,
  SEXP r_size,
  SEXP r_direction )
{
  using RealType = float;

  using ANTsRFieldType = itk::VectorImage<RealType, Dimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  using VectorType = itk::Vector<RealType, Dimension>;

  using ITKFieldType = itk::Image<VectorType, Dimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<ITKFieldType>;

  using CoordinateRepType = float;
  using TransformType = itk::ThinPlateSplineKernelTransform<CoordinateRepType, Dimension>;
  using PointType = itk::Point<CoordinateRepType, Dimension>;
  using PointSetType = typename TransformType::PointSetType;

  auto tps = TransformType::New();

  ////////////////////////////
  //
  //  Define the output B-spline field domain
  //

  auto field = ITKFieldType::New();

  if( Rf_isNull( r_origin ) || Rf_isNull( r_size ) || Rf_isNull( r_spacing ) || Rf_isNull( r_direction ) )
    {
    Rcpp::stop( "Thin-plate spline domain is not specified." );
    }
  else
    {
    Rcpp::NumericVector origin( r_origin );
    Rcpp::NumericVector spacing( r_spacing );
    Rcpp::NumericVector size( r_size );
    Rcpp::NumericMatrix direction( r_direction );

    typename ITKFieldType::PointType fieldOrigin;
    typename ITKFieldType::SpacingType fieldSpacing;
    typename ITKFieldType::SizeType fieldSize;
    typename ITKFieldType::DirectionType fieldDirection;

    for( unsigned int d = 0; d < Dimension; d++ )
      {
      fieldOrigin[d] = origin[d];
      fieldSpacing[d] = spacing[d];
      fieldSize[d] = size[d];
      for( unsigned int e = 0; e < Dimension; e++ )
        {
        fieldDirection(d, e) = direction(d, e);
        }
      }
    field->SetRegions( fieldSize );
    field->SetOrigin( fieldOrigin );
    field->SetSpacing( fieldSpacing );
    field->SetDirection( fieldDirection );
    field->Allocate();
    }

  auto sourceLandmarks = PointSetType::New();
  auto targetLandmarks = PointSetType::New();
  typename PointSetType::PointsContainer::Pointer sourceLandmarkContainer = sourceLandmarks->GetPoints();
  typename PointSetType::PointsContainer::Pointer targetLandmarkContainer = targetLandmarks->GetPoints();

  PointType sourcePoint;
  PointType targetPoint;

  Rcpp::NumericMatrix displacementOrigins( r_displacementOrigins );
  Rcpp::NumericMatrix displacements( r_displacements );

  unsigned int numberOfPoints = displacements.nrow();

  for( unsigned int n = 0; n < numberOfPoints; n++ )
    {
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      sourcePoint[d] = displacementOrigins(n, d);
      targetPoint[d] = displacementOrigins(n, d) + displacements(n, d);
      }
    sourceLandmarkContainer->InsertElement( n, sourcePoint );
    targetLandmarkContainer->InsertElement( n, targetPoint );
    }

  tps->SetSourceLandmarks( sourceLandmarks );
  tps->SetTargetLandmarks( targetLandmarks );
  tps->ComputeWMatrix();

  //////////////////////////
  //
  //  Now convert back to vector image type.
  //

  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

  typename TransformType::InputPointType  source;
  typename TransformType::OutputPointType target;

  IteratorType It( field, field->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    field->TransformIndexToPhysicalPoint( It.GetIndex(), source );
    target = tps->TransformPoint( source );

    typename ANTsRFieldType::PixelType antsrVector( Dimension );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      antsrVector[d] = target[d] - source[d];
      }
    antsrField->SetPixel( It.GetIndex(), antsrVector );
    }

  r_antsrField = Rcpp::wrap( antsrField );
  return( r_antsrField );
}

RcppExport SEXP fitThinPlateSplineDisplacementField(
  SEXP r_dimensionality,
  SEXP r_displacementOrigins,
  SEXP r_displacements,
  SEXP r_origin,
  SEXP r_spacing,
  SEXP r_size,
  SEXP r_direction )
{
try
  {

  using PrecisionType = float;

  unsigned int dimensionality = Rcpp::as<int>( r_dimensionality );

  // 2-D vector field
  if( dimensionality == 2 )
    {
    const unsigned int Dimension = 2;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetVectorLength( Dimension );

    if( Rf_isNull( r_origin ) || Rf_isNull( r_size ) || Rf_isNull( r_spacing ) || Rf_isNull( r_direction ) )
      {
      throw std::invalid_argument( "one or more thin-plate spline domain definitions are not specified." );
      }
    else
      {
      Rcpp::NumericVector origin( r_origin );
      Rcpp::NumericVector spacing( r_spacing );
      Rcpp::NumericVector size( r_size );
      Rcpp::NumericMatrix direction( r_direction );

      typename ANTsRFieldType::PointType fieldOrigin;
      typename ANTsRFieldType::SpacingType fieldSpacing;
      typename ANTsRFieldType::SizeType fieldSize;
      typename ANTsRFieldType::DirectionType fieldDirection;

      for( unsigned int d = 0; d < Dimension; d++ )
        {
        fieldOrigin[d] = origin[d];
        fieldSpacing[d] = spacing[d];
        fieldSize[d] = size[d];
        for( unsigned int e = 0; e < Dimension; e++ )
          {
          fieldDirection(d, e) = direction(d, e);
          }
        }
      antsrField->SetOrigin( fieldOrigin );
      antsrField->SetRegions( fieldSize );
      antsrField->SetSpacing( fieldSpacing );
      antsrField->SetDirection( fieldDirection );
      antsrField->Allocate();
      }

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputThinPlateSplineField = fitThinPlateSplineVectorImageHelper<Dimension>(
      r_displacementOrigins, r_displacements, 
      s4_antsrField, r_origin, r_spacing, r_size, r_direction );
    return( outputThinPlateSplineField );
    }
  else if( dimensionality == 3 )
    {
    const unsigned int Dimension = 3;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetVectorLength( Dimension );

    if( Rf_isNull( r_origin ) || Rf_isNull( r_size ) || Rf_isNull( r_spacing ) || Rf_isNull( r_direction ) )
      {
      throw std::invalid_argument( "one or more thin-plate spline domain definitions are not specified." );
      }
    else
      {
      Rcpp::NumericVector origin( r_origin );
      Rcpp::NumericVector spacing( r_spacing );
      Rcpp::NumericVector size( r_size );
      Rcpp::NumericMatrix direction( r_direction );

      typename ANTsRFieldType::PointType fieldOrigin;
      typename ANTsRFieldType::SpacingType fieldSpacing;
      typename ANTsRFieldType::SizeType fieldSize;
      typename ANTsRFieldType::DirectionType fieldDirection;

      for( unsigned int d = 0; d < Dimension; d++ )
        {
        fieldOrigin[d] = origin[d];
        fieldSpacing[d] = spacing[d];
        fieldSize[d] = size[d];
        for( unsigned int e = 0; e < Dimension; e++ )
          {
          fieldDirection(d, e) = direction(d, e);
          }
        }
      antsrField->SetOrigin( fieldOrigin );
      antsrField->SetRegions( fieldSize );
      antsrField->SetSpacing( fieldSpacing );
      antsrField->SetDirection( fieldDirection );
      antsrField->Allocate();
      }

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputThinPlateSplineField = fitThinPlateSplineVectorImageHelper<Dimension>(
      r_displacementOrigins, r_displacements, 
      s4_antsrField, r_origin, r_spacing, r_size, r_direction );
    return( outputThinPlateSplineField );
    }
  else
    {
    Rcpp::stop( "Untemplated dimension." );
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
