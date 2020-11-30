#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkDisplacementFieldToBSplineImageFilter.h"
#include "RcppANTsR.h"



template<unsigned int Dimension>
SEXP fitBSplineVectorImageHelper(
  SEXP r_displacementField,
  SEXP r_displacementFieldWeightImage,
  SEXP r_displacementOrigins,
  SEXP r_displacements,
  SEXP r_displacementWeights,
  SEXP r_antsrField,
  SEXP r_origin,
  SEXP r_spacing,
  SEXP r_size,
  SEXP r_direction,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
{
  using RealType = float;

  using ANTsRFieldType = itk::VectorImage<RealType, Dimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  using VectorType = itk::Vector<RealType, Dimension>;
  using PointSetType = itk::PointSet<VectorType, Dimension>;

  using ITKFieldType = itk::Image<VectorType, Dimension>;
  using ITKFieldPointerType = typename ITKFieldType::Pointer;
  using IteratorType = itk::ImageRegionIteratorWithIndex<ITKFieldType>;

  using BSplineFilterType = itk::DisplacementFieldToBSplineImageFilter<ITKFieldType, PointSetType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;

  typename BSplineFilterType::Pointer bsplineFilter = BSplineFilterType::New();

  ////////////////////////////
  //
  //  Add the inputs (if they are specified)
  //

  if( ! Rf_isNull( r_displacementField ) )
    {
    ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );

    ITKFieldPointerType inputITKField = ITKFieldType::New();
    inputITKField->CopyInformation( inputANTsRField );
    inputITKField->SetRegions( inputANTsRField->GetRequestedRegion() );
    inputITKField->Allocate();

    IteratorType It( inputITKField, inputITKField->GetRequestedRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      VectorType vector;

      typename ANTsRFieldType::PixelType antsrVector = inputANTsRField ->GetPixel( It.GetIndex() );
      for( unsigned int d = 0; d < Dimension; d++ )
        {
        vector[d] = antsrVector[d];
        }
      It.Set( vector );
      }
    bsplineFilter->SetDisplacementField( inputITKField );
    }

  if( ! Rf_isNull( r_displacementFieldWeightImage ) )
    {
    using WeightImageType = typename BSplineFilterType::RealImageType;
    using WeightImagePointerType = typename WeightImageType::Pointer;

    WeightImagePointerType weightImage = Rcpp::as<WeightImagePointerType>( r_displacementFieldWeightImage );

    bsplineFilter->SetConfidenceImage( weightImage );
    }

  if( ! Rf_isNull( r_displacements ) )
    {
    typename PointSetType::Pointer pointSet = PointSetType::New();
    pointSet->Initialize();
    typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

    Rcpp::NumericMatrix displacementOrigins( r_displacementOrigins );
    Rcpp::NumericMatrix displacements( r_displacements );
    Rcpp::NumericVector displacementWeights( r_displacementWeights );

    unsigned int numberOfPoints = displacements.nrow();

    for( unsigned int n = 0; n < numberOfPoints; n++ )
      {
      typename PointSetType::PointType point;
      for( unsigned int d = 0; d < Dimension; d++ )
        {
        point[d] = displacementOrigins(n, d);
        }
      pointSet->SetPoint( n, point );

      VectorType data( 0.0 );
      for( unsigned int d = 0; d < Dimension; d++ )
        {
        data[d] = displacements(n, d);
        }
      pointSet->SetPointData( n, data );

      weights->InsertElement( n, displacementWeights[n] );
      }
    bsplineFilter->SetPointSet( pointSet );
    bsplineFilter->SetPointSetConfidenceWeights( weights );
    }

  ////////////////////////////
  //
  //  Define the output B-spline field domain
  //

  if( Rf_isNull( r_origin ) || Rf_isNull( r_size ) || Rf_isNull( r_spacing ) || Rf_isNull( r_direction ) )
    {
    if( Rf_isNull( r_displacementField ) )
      {
      Rcpp::stop( "B-spline domain is not specified." );
      }
    else
      {
      bsplineFilter->SetUseInputFieldToDefineTheBSplineDomain( true );
      }
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
    bsplineFilter->SetBSplineDomain( fieldOrigin, fieldSpacing, fieldSize, fieldDirection );
    }

  unsigned int numberOfFittingLevels = Rcpp::as<int>( r_numberOfFittingLevels );
  Rcpp::NumericVector numberOfControlPoints( r_numberOfControlPoints );
  unsigned int splineOrder = Rcpp::as<int>( r_splineOrder );

  typename BSplineFilterType::ArrayType ncps;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    ncps[d] = numberOfControlPoints[d];
    }

  bsplineFilter->SetNumberOfControlPoints( ncps );
  bsplineFilter->SetSplineOrder( splineOrder );
  bsplineFilter->SetNumberOfFittingLevels( numberOfFittingLevels );
  bsplineFilter->Update();

  //////////////////////////
  //
  //  Now convert back to vector image type.
  //

  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

  IteratorType It( bsplineFilter->GetOutput(),
    bsplineFilter->GetOutput()->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    VectorType data = It.Value();

    typename ANTsRFieldType::PixelType antsrVector( Dimension );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      antsrVector[d] = data[d];
      }
    antsrField->SetPixel( It.GetIndex(), antsrVector );
    }

  r_antsrField = Rcpp::wrap( antsrField );
  return( r_antsrField );
}

RcppExport SEXP fitBsplineDisplacementField(
  SEXP r_dimensionality,
  SEXP r_displacementField,
  SEXP r_displacementFieldWeightImage,
  SEXP r_displacementOrigins,
  SEXP r_displacements,
  SEXP r_displacementWeights,
  SEXP r_origin,
  SEXP r_spacing,
  SEXP r_size,
  SEXP r_direction,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
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

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetOrigin( fieldOrigin );
    antsrField->SetRegions( fieldSize );
    antsrField->SetSpacing( fieldSpacing );
    antsrField->SetVectorLength( Dimension );
    antsrField->SetDirection( fieldDirection );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputBSplineField = fitBSplineVectorImageHelper<Dimension>(
      r_displacementField, r_displacementFieldWeightImage,
      r_displacementOrigins, r_displacements, r_displacementWeights,
      s4_antsrField,
      r_origin, r_spacing, r_size, r_direction,
      r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
    return( outputBSplineField );
    }
  else if( dimensionality == 3 )
    {
    const unsigned int Dimension = 3;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

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

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetOrigin( fieldOrigin );
    antsrField->SetRegions( fieldSize );
    antsrField->SetSpacing( fieldSpacing );
    antsrField->SetVectorLength( Dimension );
    antsrField->SetDirection( fieldDirection );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP outputBSplineField = fitBSplineVectorImageHelper<Dimension>(
      r_displacementField, r_displacementFieldWeightImage,
      r_displacementOrigins, r_displacements, r_displacementWeights,
      s4_antsrField,
      r_origin, r_spacing, r_size, r_direction,
      r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
    return( outputBSplineField );
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
