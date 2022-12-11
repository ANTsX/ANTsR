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
  SEXP r_splineOrder,
  SEXP r_enforceStationaryBoundary,
  SEXP r_estimateInverse,
  SEXP r_rasterizePoints )
{
  using RealType = float;

  using ANTsRFieldType = itk::VectorImage<RealType, Dimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  using VectorType = itk::Vector<RealType, Dimension>;
  using PointSetType = itk::PointSet<VectorType, Dimension>;

  using ITKFieldType = itk::Image<VectorType, Dimension>;
  using ITKFieldPointerType = typename ITKFieldType::Pointer;
  using IteratorType = itk::ImageRegionIteratorWithIndex<ITKFieldType>;

  using ITKFieldPointType = typename ITKFieldType::PointType;
  using ContinuousIndexType = itk::ContinuousIndex<typename ITKFieldPointType::CoordRepType, Dimension>;

  using BSplineFilterType = itk::DisplacementFieldToBSplineImageFilter<ITKFieldType, PointSetType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;
  using WeightImageType = typename BSplineFilterType::RealImageType;
  using WeightImagePointerType = typename WeightImageType::Pointer;

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
    WeightImagePointerType weightImage = Rcpp::as<WeightImagePointerType>( r_displacementFieldWeightImage );

    bsplineFilter->SetConfidenceImage( weightImage );
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

  if( ! Rf_isNull( r_displacements ) )
    {
    typename PointSetType::Pointer pointSet = PointSetType::New();
    pointSet->Initialize();
    typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

    Rcpp::NumericMatrix displacementOrigins( r_displacementOrigins );
    Rcpp::NumericMatrix displacements( r_displacements );
    Rcpp::NumericVector displacementWeights( r_displacementWeights );

    bool rasterizePoints = Rcpp::as<bool>( r_rasterizePoints );

    if( rasterizePoints )
      {
      if( ! Rf_isNull( r_displacementField ) )
        {
        Rcpp::stop( "Rasterizing input is only for the case where a displacement field is not specified." );
        }

      // First, distribute the weights and displacements to an image the same size as the b-spline domain.
 
      WeightImagePointerType weightImage = WeightImageType::New();
      weightImage->SetOrigin( bsplineFilter->GetBSplineDomainOrigin() );
      weightImage->SetSpacing( bsplineFilter->GetBSplineDomainSpacing() );
      weightImage->SetDirection( bsplineFilter->GetBSplineDomainDirection() );
      weightImage->SetRegions( bsplineFilter->GetBSplineDomainSize() );
      weightImage->Allocate();
      weightImage->FillBuffer( 0.0 );

      ITKFieldPointerType rasterizedField = ITKFieldType::New();
      rasterizedField->SetOrigin( bsplineFilter->GetBSplineDomainOrigin() );
      rasterizedField->SetSpacing( bsplineFilter->GetBSplineDomainSpacing() );
      rasterizedField->SetDirection( bsplineFilter->GetBSplineDomainDirection() );
      rasterizedField->SetRegions( bsplineFilter->GetBSplineDomainSize() );
      rasterizedField->Allocate();

      unsigned int numberOfPoints = displacements.nrow();

      for( unsigned int n = 0; n < numberOfPoints; n++ )
        {
        typename ITKFieldType::PointType imagePoint;
        VectorType imageDisplacement;
        for( unsigned int d = 0; d < Dimension; d++ )
          {
          imagePoint[d] = displacementOrigins(n, d);
          imageDisplacement[d] = displacements(n, d);
          }
        typename ITKFieldType::IndexType imageIndex = 
          weightImage->TransformPhysicalPointToIndex( imagePoint );
        weightImage->SetPixel( imageIndex, displacementWeights[n] );
        rasterizedField->SetPixel( imageIndex, imageDisplacement );
        }

      // Second, iterate through the weight image and pull those indices/points which have non-zero weights.

      unsigned count = 0;  

      typename itk::ImageRegionIteratorWithIndex<WeightImageType> 
        ItW( weightImage, weightImage->GetLargestPossibleRegion() );
      for( ItW.GoToBegin(); ! ItW.IsAtEnd(); ++ItW )
        {
        if( ItW.Get() > 0.0 )
          {
          typename ITKFieldType::PointType imagePoint;
          weightImage->TransformIndexToPhysicalPoint( ItW.GetIndex(), imagePoint );
          typename PointSetType::PointType point;
          point.CastFrom( imagePoint );
          pointSet->SetPoint( count, point );
          pointSet->SetPointData( count, rasterizedField->GetPixel( ItW.GetIndex() ) );
          weights->InsertElement( count, ItW.Get() );
          count++;
          }
        } 
      } 
    else 
      {
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
      }
    bsplineFilter->SetPointSet( pointSet );
    bsplineFilter->SetPointSetConfidenceWeights( weights );
    }

  unsigned int numberOfFittingLevels = Rcpp::as<int>( r_numberOfFittingLevels );
  Rcpp::NumericVector numberOfControlPoints( r_numberOfControlPoints );
  unsigned int splineOrder = Rcpp::as<int>( r_splineOrder );
  bool enforceStationaryBoundary = Rcpp::as<bool>( r_enforceStationaryBoundary );
  bool estimateInverse = Rcpp::as<bool>( r_estimateInverse );

  typename BSplineFilterType::ArrayType ncps;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    ncps[d] = numberOfControlPoints[d];
    }

  bsplineFilter->SetNumberOfControlPoints( ncps );
  bsplineFilter->SetSplineOrder( splineOrder );
  bsplineFilter->SetNumberOfFittingLevels( numberOfFittingLevels );
  bsplineFilter->SetEnforceStationaryBoundary( enforceStationaryBoundary );
  bsplineFilter->SetEstimateInverse( estimateInverse );
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
  SEXP r_splineOrder,
  SEXP r_enforceStationaryBoundary,
  SEXP r_estimateInverse,
  SEXP r_rasterizePoints )
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
      if( ! Rf_isNull( r_displacementField ) )
        {
        ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );
        antsrField->CopyInformation( inputANTsRField );
        antsrField->SetRegions( inputANTsRField->GetRequestedRegion() );
        antsrField->Allocate();
        }
      else
        {
        throw std::invalid_argument( "one or more b-spline domain definitions are not specified." );
        }
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

    SEXP outputBSplineField = fitBSplineVectorImageHelper<Dimension>(
      r_displacementField, r_displacementFieldWeightImage,
      r_displacementOrigins, r_displacements, r_displacementWeights,
      s4_antsrField,
      r_origin, r_spacing, r_size, r_direction,
      r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder,
      r_enforceStationaryBoundary, r_estimateInverse, r_rasterizePoints );
    return( outputBSplineField );
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
      if( ! Rf_isNull( r_displacementField ) )
        {
        ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );
        antsrField->CopyInformation( inputANTsRField );
        antsrField->SetRegions( inputANTsRField->GetRequestedRegion() );
        antsrField->Allocate();
        }
      else
        {
        throw std::invalid_argument( "one or more b-spline domain definitions are not specified." );
        }
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

    SEXP outputBSplineField = fitBSplineVectorImageHelper<Dimension>(
      r_displacementField, r_displacementFieldWeightImage,
      r_displacementOrigins, r_displacements, r_displacementWeights,
      s4_antsrField,
      r_origin, r_spacing, r_size, r_direction,
      r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder,
      r_enforceStationaryBoundary, r_estimateInverse, r_rasterizePoints );
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
