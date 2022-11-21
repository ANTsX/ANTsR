#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"

#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageFileWriter.h"

#include "RcppANTsR.h"

template<unsigned int Dimension>
SEXP integrateVelocityFieldHelper(
  SEXP r_velocityField,
  SEXP r_antsrField,
  SEXP r_lowerBound,
  SEXP r_upperBound,
  SEXP r_numberOfIntegrationSteps )
{
  using RealType = float;

  using ANTsRVelocityFieldType = itk::VectorImage<RealType, Dimension+1>;
  using ANTsRVelocityFieldPointerType = typename ANTsRVelocityFieldType::Pointer;

  using ANTsRFieldType = itk::VectorImage<RealType, Dimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  using VectorType = itk::Vector<RealType, Dimension>;

  using ITKVelocityFieldType = itk::Image<VectorType, Dimension+1>;
  using ITKVelocityFieldPointerType = typename ITKVelocityFieldType::Pointer;
  using ITKFieldType = itk::Image<VectorType, Dimension>;

  using IteratorType = itk::ImageRegionIteratorWithIndex<ITKVelocityFieldType>;
  using ConstIteratorType = itk::ImageRegionConstIteratorWithIndex<ITKFieldType>;

  ANTsRVelocityFieldPointerType inputANTsRVelocityField = Rcpp::as<ANTsRVelocityFieldPointerType>( r_velocityField );

  ITKVelocityFieldPointerType inputITKVelocityField = ITKVelocityFieldType::New();
  inputITKVelocityField->CopyInformation( inputANTsRVelocityField );
  inputITKVelocityField->SetRegions( inputANTsRVelocityField->GetRequestedRegion() );
  inputITKVelocityField->Allocate();

  IteratorType It( inputITKVelocityField,
                   inputITKVelocityField->GetRequestedRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    VectorType vector;

    typename ANTsRFieldType::PixelType antsrVector = inputANTsRVelocityField ->GetPixel( It.GetIndex() );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      vector[d] = antsrVector[d];
      }
    It.Set( vector );
    }

  using IntegratorType = itk::TimeVaryingVelocityFieldIntegrationImageFilter<ITKVelocityFieldType, ITKFieldType>;
  typename IntegratorType::Pointer integrator = IntegratorType::New();

  integrator->SetInput( inputITKVelocityField );
  integrator->SetLowerTimeBound( Rcpp::as<float>( r_lowerBound ) );
  integrator->SetUpperTimeBound( Rcpp::as<float>( r_upperBound ) );
  integrator->SetNumberOfIntegrationSteps( Rcpp::as<int>( r_numberOfIntegrationSteps ) );
  integrator->Update();

  //////////////////////////
  //
  //  Now convert back to vector image type.
  //

  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

  ConstIteratorType It2( integrator->GetOutput(),
    integrator->GetOutput()->GetRequestedRegion() );
  for( It2.GoToBegin(); !It2.IsAtEnd(); ++It2 )
    {
    VectorType data = It2.Value();

    typename ANTsRFieldType::PixelType antsrVector( Dimension );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      antsrVector[d] = data[d];
      }
    antsrField->SetPixel( It2.GetIndex(), antsrVector );
    }

  r_antsrField = Rcpp::wrap( antsrField );
  return( r_antsrField );
}

RcppExport SEXP integrateVelocityField(
  SEXP r_dimensionality,
  SEXP r_velocityField,
  SEXP r_lowerBound,
  SEXP r_upperBound,
  SEXP r_numberOfIntegrationSteps )
{

try
  {
  using PrecisionType = float;

  unsigned int dimensionality = Rcpp::as<int>( r_dimensionality );

  if( dimensionality == 2 )
    {
    const unsigned int Dimension = 2;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    using ANTsRVelocityFieldType = itk::VectorImage<PrecisionType, Dimension+1>;
    using ANTsRVelocityFieldPointerType = typename ANTsRVelocityFieldType::Pointer;

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    ANTsRVelocityFieldPointerType inputANTsRVelocityField = Rcpp::as<ANTsRVelocityFieldPointerType>( r_velocityField );

    typename ANTsRFieldType::PointType fieldOrigin;
    typename ANTsRFieldType::SpacingType fieldSpacing;
    typename ANTsRFieldType::SizeType fieldSize;
    typename ANTsRFieldType::DirectionType fieldDirection;

    for( unsigned int d = 0; d < Dimension; d++ )
      {
      fieldOrigin[d] = inputANTsRVelocityField->GetOrigin()[d];
      fieldSpacing[d] = inputANTsRVelocityField->GetSpacing()[d];
      fieldSize[d] = inputANTsRVelocityField->GetRequestedRegion().GetSize()[d];
      for( unsigned int e = 0; e < Dimension; e++ )
        {
        fieldDirection(d, e) = inputANTsRVelocityField->GetDirection()(d, e);
        }
      }

    antsrField->SetOrigin( fieldOrigin );
    antsrField->SetSpacing( fieldSpacing );
    antsrField->SetRegions( fieldSize );
    antsrField->SetDirection( fieldDirection );
    antsrField->SetVectorLength( Dimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP integratedField = integrateVelocityFieldHelper<Dimension>(
      r_velocityField, s4_antsrField, r_lowerBound, r_upperBound,
      r_numberOfIntegrationSteps );

    return( integratedField );
    }
  else if( dimensionality == 3 )
    {
    const unsigned int Dimension = 3;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    using ANTsRVelocityFieldType = itk::VectorImage<PrecisionType, Dimension+1>;
    using ANTsRVelocityFieldPointerType = typename ANTsRVelocityFieldType::Pointer;

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetVectorLength( Dimension );

    ANTsRVelocityFieldPointerType inputANTsRVelocityField = Rcpp::as<ANTsRVelocityFieldPointerType>( r_velocityField );

    typename ANTsRFieldType::PointType fieldOrigin;
    typename ANTsRFieldType::SpacingType fieldSpacing;
    typename ANTsRFieldType::SizeType fieldSize;
    typename ANTsRFieldType::DirectionType fieldDirection;

    for( unsigned int d = 0; d < Dimension; d++ )
      {
      fieldOrigin[d] = inputANTsRVelocityField->GetOrigin()[d];
      fieldSpacing[d] = inputANTsRVelocityField->GetSpacing()[d];
      fieldSize[d] = inputANTsRVelocityField->GetRequestedRegion().GetSize()[d];
      for( unsigned int e = 0; e < Dimension; e++ )
        {
        fieldDirection(d, e) = inputANTsRVelocityField->GetDirection()(d, e);
        }
      }

    antsrField->SetOrigin( fieldOrigin );
    antsrField->SetSpacing( fieldSpacing );
    antsrField->SetRegions( fieldSize );
    antsrField->SetDirection( fieldDirection );
    antsrField->SetVectorLength( Dimension );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP integratedField = integrateVelocityFieldHelper<Dimension>(
      r_velocityField, s4_antsrField, r_lowerBound, r_upperBound,
      r_numberOfIntegrationSteps );
    return( integratedField );
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
