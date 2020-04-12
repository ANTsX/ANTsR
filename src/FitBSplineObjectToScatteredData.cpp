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
    for( unsigned int d = 0; d < ParametricDimension; d++ )
      {
      point[d] = parametricData(n, d);
      }
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

  //////////////////////////
  //
  //  Only difference between the Curve, Image, and Object function
  //  is the return type.
  //

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

template<unsigned int ParametricDimension>
SEXP fitBSplineImageHelper(
  SEXP r_scatteredData,
  SEXP r_parametricData,
  SEXP r_dataWeights,
  SEXP r_image,
  SEXP r_parametricDomainOrigin,
  SEXP r_parametricDomainSpacing,
  SEXP r_parametricDomainSize,
  SEXP r_isParametricDimensionClosed,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
{
  using RealType = float;
  const unsigned int DataDimension = 1;

  using ScatteredDataType = itk::Vector<RealType, DataDimension>;
  using PointSetType = itk::PointSet<ScatteredDataType, ParametricDimension>;
  using OutputImageType = itk::Image<ScatteredDataType, ParametricDimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  using ImageType = itk::Image<RealType, ParametricDimension>;
  using ImagePointerType = typename ImageType::Pointer;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_image );

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
    for( unsigned int d = 0; d < ParametricDimension; d++ )
      {
      point[d] = parametricData(n, d);
      }
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

  //////////////////////////
  //
  //  Only difference between the Curve, Image, and Object function
  //  is the return type.
  //

  IteratorType It( bsplineFilter->GetOutput(),
    bsplineFilter->GetOutput()->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    ScatteredDataType data = It.Value();
    image->SetPixel( It.GetIndex(), data[0] );
    }

  r_image = Rcpp::wrap( image );
  return( r_image );
}

template<unsigned int ParametricDimension, unsigned int DataDimension>
SEXP fitBSplineVectorImageHelper(
  SEXP r_scatteredData,
  SEXP r_parametricData,
  SEXP r_dataWeights,
  SEXP r_antsrField,
  SEXP r_parametricDomainOrigin,
  SEXP r_parametricDomainSpacing,
  SEXP r_parametricDomainSize,
  SEXP r_isParametricDimensionClosed,
  SEXP r_numberOfFittingLevels,
  SEXP r_numberOfControlPoints,
  SEXP r_splineOrder )
{
  using RealType = float;
  using ScatteredDataType = itk::Vector<RealType, DataDimension>;
  using PointSetType = itk::PointSet<ScatteredDataType, ParametricDimension>;
  using OutputImageType = itk::Image<ScatteredDataType, ParametricDimension>;
  using IteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  using ANTsRFieldType = itk::VectorImage<RealType, ParametricDimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;
  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

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
    for( unsigned int d = 0; d < ParametricDimension; d++ )
      {
      point[d] = parametricData(n, d);
      }
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

  //////////////////////////
  //
  //  Only difference between the Curve and Object function
  //  is the return type.
  //

  IteratorType It( bsplineFilter->GetOutput(),
    bsplineFilter->GetOutput()->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    ScatteredDataType data = It.Value();

    typename ANTsRFieldType::PixelType antsrVector( DataDimension );
    for( unsigned int d = 0; d < DataDimension; d++ )
      {
      antsrVector[d] = data[d];
      }
    antsrField->SetPixel( It.GetIndex(), antsrVector );
    }

  r_antsrField = Rcpp::wrap( antsrField );
  return( r_antsrField );
}

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
  using PrecisionType = float;

  Rcpp::NumericMatrix scatteredData( r_scatteredData );
  Rcpp::NumericMatrix parametricData( r_parametricData );
  Rcpp::NumericVector weights( r_dataWeights );

  Rcpp::NumericVector parametricDomainOrigin( r_parametricDomainOrigin );
  Rcpp::NumericVector parametricDomainSpacing( r_parametricDomainSpacing );
  Rcpp::NumericVector parametricDomainSize( r_parametricDomainSize );

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
      Rcpp::stop( "Untemplated data dimension for parametric dimension = 1." );
      }
    }    
  else if( parametricDimension == 2 )
    { 
    const unsigned int ParametricDimension = 2;
    // 2-D scalar field  
    if( dataDimension == 1 ) 
      {
      using ImageType = itk::Image<PrecisionType, ParametricDimension>;
      using ImagePointerType = typename ImageType::Pointer;

      typename ImageType::PointType origin;
      typename ImageType::SpacingType spacing;
      typename ImageType::SizeType size;
      typename ImageType::DirectionType direction;

      for( unsigned int d = 0; d < ParametricDimension; d++ )
        {
        origin[d] = parametricDomainOrigin[d];
        spacing[d] = parametricDomainSpacing[d];
        size[d] = parametricDomainSize[d];
        }
      direction.SetIdentity();

      ImagePointerType image = ImageType::New();
      image->SetOrigin( origin );
      image->SetRegions( size );
      image->SetSpacing( spacing );
      image->SetDirection( direction );
      image->Allocate();

      Rcpp::S4 s4_image( Rcpp::wrap( image ) );

      SEXP outputBSplineObject = fitBSplineImageHelper<ParametricDimension>( 
        r_scatteredData, r_parametricData, r_dataWeights, s4_image, r_parametricDomainOrigin, 
        r_parametricDomainSpacing, r_parametricDomainSize, r_isParametricDimensionClosed, 
        r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineObject );
      }
    // 2-D vector field  
    else if( dataDimension == 2 ) 
      {
      const unsigned int DataDimension = 2;  
      using ANTsRFieldType = itk::VectorImage<PrecisionType, ParametricDimension>;
      using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

      typename ANTsRFieldType::PointType origin;
      typename ANTsRFieldType::SpacingType spacing;
      typename ANTsRFieldType::SizeType size;
      typename ANTsRFieldType::DirectionType direction;

      for( unsigned int d = 0; d < ParametricDimension; d++ )
        {
        origin[d] = parametricDomainOrigin[d];
        spacing[d] = parametricDomainSpacing[d];
        size[d] = parametricDomainSize[d];
        }
      direction.SetIdentity();

      ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
      antsrField->SetOrigin( origin );
      antsrField->SetRegions( size );
      antsrField->SetSpacing( spacing );
      antsrField->SetVectorLength( DataDimension );
      antsrField->SetDirection( direction );
      antsrField->Allocate();

      Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

      SEXP outputBSplineObject = fitBSplineVectorImageHelper<ParametricDimension, DataDimension>( 
        r_scatteredData, r_parametricData, r_dataWeights, s4_antsrField, r_parametricDomainOrigin, 
        r_parametricDomainSpacing, r_parametricDomainSize, r_isParametricDimensionClosed, 
        r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineObject );
      } else {
      Rcpp::stop( "Untemplated data dimension for parametric dimension = 2." );
      }
    }
  else if( parametricDimension == 3 )
    { 
    const unsigned int ParametricDimension = 3;
    // 3-D scalar field  
    if( dataDimension == 1 ) 
      {
      using ImageType = itk::Image<PrecisionType, ParametricDimension>;
      using ImagePointerType = typename ImageType::Pointer;

      typename ImageType::PointType origin;
      typename ImageType::SpacingType spacing;
      typename ImageType::SizeType size;
      typename ImageType::DirectionType direction;

      for( unsigned int d = 0; d < ParametricDimension; d++ )
        {
        origin[d] = parametricDomainOrigin[d];
        spacing[d] = parametricDomainSpacing[d];
        size[d] = parametricDomainSize[d];
        }
      direction.SetIdentity();

      ImagePointerType image = ImageType::New();
      image->SetOrigin( origin );
      image->SetRegions( size );
      image->SetSpacing( spacing );
      image->SetDirection( direction );
      image->Allocate();

      Rcpp::S4 s4_image( Rcpp::wrap( image ) );

      SEXP outputBSplineObject = fitBSplineImageHelper<ParametricDimension>( 
        r_scatteredData, r_parametricData, r_dataWeights, s4_image, r_parametricDomainOrigin, 
        r_parametricDomainSpacing, r_parametricDomainSize, r_isParametricDimensionClosed, 
        r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineObject );
      }
    // 3-D vector field  
    else if( dataDimension == 3 ) 
      {
      const unsigned int DataDimension = 2;  
      using ANTsRFieldType = itk::VectorImage<PrecisionType, ParametricDimension>;
      using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

      typename ANTsRFieldType::PointType origin;
      typename ANTsRFieldType::SpacingType spacing;
      typename ANTsRFieldType::SizeType size;
      typename ANTsRFieldType::DirectionType direction;

      for( unsigned int d = 0; d < ParametricDimension; d++ )
        {
        origin[d] = parametricDomainOrigin[d];
        spacing[d] = parametricDomainSpacing[d];
        size[d] = parametricDomainSize[d];
        }
      direction.SetIdentity();

      ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
      antsrField->SetOrigin( origin );
      antsrField->SetRegions( size );
      antsrField->SetSpacing( spacing );
      antsrField->SetVectorLength( DataDimension );
      antsrField->SetDirection( direction );
      antsrField->Allocate();

      Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

      SEXP outputBSplineObject = fitBSplineVectorImageHelper<ParametricDimension, DataDimension>( 
        r_scatteredData, r_parametricData, r_dataWeights, s4_antsrField, r_parametricDomainOrigin, 
        r_parametricDomainSpacing, r_parametricDomainSize, r_isParametricDimensionClosed, 
        r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineObject );
      } else {
      Rcpp::stop( "Untemplated data dimension for parametric dimension = 3." );
      }
    }
  else if( parametricDimension == 4 )
    { 
    const unsigned int ParametricDimension = 4;
    // 4-D scalar field  
    if( dataDimension == 1 ) 
      {
      using ImageType = itk::Image<PrecisionType, ParametricDimension>;
      using ImagePointerType = typename ImageType::Pointer;

      typename ImageType::PointType origin;
      typename ImageType::SpacingType spacing;
      typename ImageType::SizeType size;
      typename ImageType::DirectionType direction;

      for( unsigned int d = 0; d < ParametricDimension; d++ )
        {
        origin[d] = parametricDomainOrigin[d];
        spacing[d] = parametricDomainSpacing[d];
        size[d] = parametricDomainSize[d];
        }
      direction.SetIdentity();

      ImagePointerType image = ImageType::New();
      image->SetOrigin( origin );
      image->SetRegions( size );
      image->SetSpacing( spacing );
      image->SetDirection( direction );
      image->Allocate();

      Rcpp::S4 s4_image( Rcpp::wrap( image ) );

      SEXP outputBSplineObject = fitBSplineImageHelper<ParametricDimension>( 
        r_scatteredData, r_parametricData, r_dataWeights, s4_image, r_parametricDomainOrigin, 
        r_parametricDomainSpacing, r_parametricDomainSize, r_isParametricDimensionClosed, 
        r_numberOfFittingLevels, r_numberOfControlPoints, r_splineOrder );
      return( outputBSplineObject );
      } else {
      Rcpp::stop( "Untemplated data dimension for parametric dimension = 4." );
      }
    }
  else 
    {
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
