#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "RcppANTsR.h"

template<unsigned int DataDimension>
SEXP fitBSplineCurveHelper(
  SEXP r_scatteredData,
  SEXP r_parametricData,
  SEXP r_dataWeights,
  SEXP r_parametricDomainOrigin,
  SEXP r_parametricDomainSpacing,
  SEXP r_parametricDomainSize,
  SEXP r_isParametricDimensionClosed,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
{
  const unsigned int ParametricDimension = 1;

  using RealType = float;
  using ScatteredDataType = itk::Vector<RealType, DataDimension>;
  using PointSetType = itk::PointSet<ScatteredDataType, ParametricDimension>;
  using OutputImageType = itk::Image<ScatteredDataType, ParametricDimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  using BSplineFilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, OutputImageType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;

  typename PointSetType::Pointer pointSet = PointSetType::New();
  pointSet->Initialize();
  typename WeightsContainerType::Pointer weights = WeightsContainerType::New();

  Rcpp::NumericMatrix scatteredData( r_scatteredData );
  Rcpp::NumericMatrix parametricData( r_parametricData );
  Rcpp::NumericVector dataWeights( r_dataWeights );

  unsigned int numberOfPoints = scatteredData.nrow();

  for( unsigned int n = 0; n < numberOfPoints; n++ )
    {
    typename PointSetType::PointType point;
    point[0] = parametricData(n, 0);
    pointSet->SetPoint( n, point );

    ScatteredDataType data( 0.0 );
    for( unsigned int d = 0; d < DataDimension; d++ )
      {
      data[d] = scatteredData(n, d);
      }
    pointSet->SetPointData( n, data );

    weights->InsertElement( n, dataWeights[n] );
    }

  typename BSplineFilterType::Pointer bsplineFilter = BSplineFilterType::New();
  bsplineFilter->SetInput( pointSet );
  bsplineFilter->SetPointWeights( weights );
  bsplineFilter->SetGenerateOutputImage( true );

  Rcpp::NumericVector parametricDomainOrigin( r_parametricDomainOrigin );
  Rcpp::NumericVector parametricDomainSpacing( r_parametricDomainSpacing );
  Rcpp::NumericVector parametricDomainSize( r_parametricDomainSize );
  Rcpp::NumericVector isParametricDimensionClosed( r_isParametricDimensionClosed );
  unsigned int numberOfFittingLevels = Rcpp::as<int>( r_numberOfFittingLevels );
  Rcpp::NumericVector numberOfControlPoints( r_numberOfControlPoints );
  unsigned int splineOrder = Rcpp::as<int>( r_splineOrder );

  typename OutputImageType::PointType origin;
  typename OutputImageType::SpacingType spacing;
  typename OutputImageType::SizeType size;
  typename BSplineFilterType::ArrayType ncps;
  typename BSplineFilterType::ArrayType isClosed;

  for( unsigned int d = 0; d < ParametricDimension; d++ )
    {
    origin[d] = parametricDomainOrigin[d];
    spacing[d] = parametricDomainSpacing[d];
    size[d] = parametricDomainSize[d];
    ncps[d] = numberOfControlPoints[d];
    isClosed[d] = static_cast<bool>( isParametricDimensionClosed[d] );
    }
  bsplineFilter->SetOrigin( origin );
  bsplineFilter->SetSpacing( spacing );
  bsplineFilter->SetSize( size );
  bsplineFilter->SetNumberOfControlPoints( ncps );
  bsplineFilter->SetSplineOrder( splineOrder );
  bsplineFilter->SetNumberOfLevels( numberOfFittingLevels );
  bsplineFilter->SetCloseDimension( isClosed );
  bsplineFilter->Update();

  Rcpp::NumericMatrix bsplineCurve(parametricDomainSize[0], DataDimension);

  unsigned int count = 0;
  IteratorType It( bsplineFilter->GetOutput(),
    bsplineFilter->GetOutput()->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    ScatteredDataType data = It.Value();
    for( unsigned int d = 0; d < DataDimension; d++ )
      {
      bsplineCurve(count, d) = data[d];
      }
    count++;  
    }
  return( Rcpp::wrap( bsplineCurve ) );
}

// parametric dimension = 2;  data dimension = 3
// closed ?
// return image

// template<class PrecisionType>
// SEXP fitBSplineSurfaceHelper()
// {

// }

// parametric dimension = 2/3/4;  data dimension = 1
// return antsr image

// template<unsigned int ParametricDimension>
// SEXP fitBSplineScalarFieldHelper()
// {

// }

// parametric dimension = 2/3;  data dimension = 2/3
// return antsr vector field

// template<unsigned int ParametricDimension, unsigned int DataDimension>
// SEXP fitBSplineDisplacementFieldHelper()
// {

// }

RcppExport SEXP fitBsplineObjectToScatteredData(
  SEXP r_scatteredData,
  SEXP r_parametricData,
  SEXP r_dataWeights,
  SEXP r_parametricDomainOrigin,
  SEXP r_parametricDomainSpacing,
  SEXP r_parametricDomainSize,
  SEXP r_isParametricDimensionClosed,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
{
try
  {
  Rcpp::NumericMatrix scatteredData( r_scatteredData );
  Rcpp::NumericMatrix parametricData( r_parametricData );
  Rcpp::NumericVector weights( r_dataWeights );

  unsigned int dataDimension = scatteredData.ncol();
  unsigned int parametricDimension = parametricData.ncol();
  unsigned int numberOfPoints = scatteredData.nrow();

  if( parametricData.nrow() != numberOfPoints )
    {
    Rcpp::stop( "Number of parametric points does not equal the number of data points." );
    }
  if( weights.size() != numberOfPoints )
    {
    Rcpp::stop( "The number of weight values does not equal the number of data points." );
    }

  // curve
  if( parametricDimension == 1 )
    {
    if( dataDimension == 1 )
      {
      SEXP outputBSplineCurve = fitBSplineCurveHelper<1>( r_scatteredData, r_parametricData,
        r_dataWeights, r_parametricDomainOrigin, r_parametricDomainSpacing, r_parametricDomainSize,
        r_isParametricDimensionClosed, r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineCurve );
      } else if( dataDimension == 2 ) {
      SEXP outputBSplineCurve = fitBSplineCurveHelper<2>( r_scatteredData, r_parametricData,
        r_dataWeights, r_parametricDomainOrigin, r_parametricDomainSpacing, r_parametricDomainSize,
        r_isParametricDimensionClosed, r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineCurve );
      } else if( dataDimension == 3 ) {
      SEXP outputBSplineCurve = fitBSplineCurveHelper<3>( r_scatteredData, r_parametricData,
        r_dataWeights, r_parametricDomainOrigin, r_parametricDomainSpacing, r_parametricDomainSize,
        r_isParametricDimensionClosed, r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineCurve );
      } else if( dataDimension == 4 ) {
      SEXP outputBSplineCurve = fitBSplineCurveHelper<4>( r_scatteredData, r_parametricData,
        r_dataWeights, r_parametricDomainOrigin, r_parametricDomainSpacing, r_parametricDomainSize,
        r_isParametricDimensionClosed, r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineCurve );
      } else {
      Rcpp::stop( "Untemplated data dimension." );
      }
    } else {
    Rcpp::stop( "Untemplated parametric dimension." );
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
