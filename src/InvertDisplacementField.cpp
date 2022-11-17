#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "RcppANTsR.h"

template<unsigned int Dimension>
SEXP invertDisplacementFieldHelper(
  SEXP r_displacementField,
  SEXP r_inverseFieldInitialEstimate,
  SEXP r_antsrField,
  SEXP r_maxNumberOfIterations,
  SEXP r_meanErrorToleranceThreshold,
  SEXP r_maxErrorToleranceThreshold,
  SEXP r_enforceBoundaryCondition )
{
  using RealType = float;

  using ANTsRFieldType = itk::VectorImage<RealType, Dimension>;
  using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

  using VectorType = itk::Vector<RealType, Dimension>;

  using ITKFieldType = itk::Image<VectorType, Dimension>;
  using ITKFieldPointerType = typename ITKFieldType::Pointer;
  using IteratorType = itk::ImageRegionIteratorWithIndex<ITKFieldType>;

  using InverterType = itk::InvertDisplacementFieldImageFilter<ITKFieldType>;
  typename InverterType::Pointer inverter = InverterType::New();

  ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );
  ANTsRFieldPointerType inputANTsRInverseFieldInitialEstimate = Rcpp::as<ANTsRFieldPointerType>( r_inverseFieldInitialEstimate );

  ITKFieldPointerType inputITKField = ITKFieldType::New();
  inputITKField->CopyInformation( inputANTsRField );
  inputITKField->SetRegions( inputANTsRField->GetRequestedRegion() );
  inputITKField->Allocate();

  ITKFieldPointerType inputITKInverseFieldInitialEstimate = ITKFieldType::New();
  inputITKInverseFieldInitialEstimate->CopyInformation( inputANTsRInverseFieldInitialEstimate );
  inputITKInverseFieldInitialEstimate->SetRegions( inputANTsRInverseFieldInitialEstimate->GetRequestedRegion() );
  inputITKInverseFieldInitialEstimate->Allocate();

  IteratorType It( inputITKField, inputITKField->GetRequestedRegion() );
  IteratorType ItI( inputITKInverseFieldInitialEstimate, inputITKInverseFieldInitialEstimate->GetRequestedRegion() );
  for( It.GoToBegin(), ItI.GoToBegin(); !It.IsAtEnd(); ++It, ++ItI )
    {
    VectorType vector;
    VectorType vectorI;

    typename ANTsRFieldType::PixelType antsrVector = inputANTsRField ->GetPixel( It.GetIndex() );
    typename ANTsRFieldType::PixelType antsrVectorI = inputANTsRField->GetPixel( ItI.GetIndex() );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      vector[d] = antsrVector[d];
      vectorI[d] = antsrVectorI[d];
      }
    It.Set( vector );
    ItI.Set( vectorI );
    }
  inverter->SetInput( inputITKField );
  inverter->SetInverseFieldInitialEstimate( inputITKInverseFieldInitialEstimate );

  unsigned int maximumNumberOfIterations = Rcpp::as<int>( r_maxNumberOfIterations );
  RealType meanErrorToleranceThreshold = Rcpp::as<RealType>( r_meanErrorToleranceThreshold );
  RealType maxErrorToleranceThreshold = Rcpp::as<RealType>( r_maxErrorToleranceThreshold );
  bool enforceBoundaryCondition = Rcpp::as<bool>( r_enforceBoundaryCondition );

  inverter->SetMaximumNumberOfIterations( maximumNumberOfIterations );
  inverter->SetMeanErrorToleranceThreshold( meanErrorToleranceThreshold );
  inverter->SetMaxErrorToleranceThreshold( maxErrorToleranceThreshold );
  inverter->SetEnforceBoundaryCondition( enforceBoundaryCondition );
  inverter->Update();

  //////////////////////////
  //
  //  Now convert back to vector image type.
  //


  ANTsRFieldPointerType antsrField = Rcpp::as<ANTsRFieldPointerType>( r_antsrField );

  IteratorType It2( inverter->GetOutput(),
    inverter->GetOutput()->GetRequestedRegion() );
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

RcppExport SEXP invertDisplacementField(
  SEXP r_dimensionality,  
  SEXP r_displacementField,
  SEXP r_inverseFieldInitialEstimate,
  SEXP r_maxNumberOfIterations,
  SEXP r_meanErrorToleranceThreshold,
  SEXP r_maxErrorToleranceThreshold,
  SEXP r_enforceBoundaryCondition )
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

    ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );
    antsrField->CopyInformation( inputANTsRField );
    antsrField->SetRegions( inputANTsRField->GetRequestedRegion() );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP inverseField = invertDisplacementFieldHelper<Dimension>(
      r_displacementField, r_inverseFieldInitialEstimate, s4_antsrField,
      r_maxNumberOfIterations, r_meanErrorToleranceThreshold,
      r_maxErrorToleranceThreshold, r_enforceBoundaryCondition );
    return( inverseField );
    }
  // 2-D vector field
  else if( dimensionality == 3 )
    {
    const unsigned int Dimension = 3;

    using ANTsRFieldType = itk::VectorImage<PrecisionType, Dimension>;
    using ANTsRFieldPointerType = typename ANTsRFieldType::Pointer;

    ANTsRFieldPointerType antsrField = ANTsRFieldType::New();
    antsrField->SetVectorLength( Dimension );

    ANTsRFieldPointerType inputANTsRField = Rcpp::as<ANTsRFieldPointerType>( r_displacementField );
    antsrField->CopyInformation( inputANTsRField );
    antsrField->SetRegions( inputANTsRField->GetRequestedRegion() );
    antsrField->Allocate();

    Rcpp::S4 s4_antsrField( Rcpp::wrap( antsrField ) );

    SEXP inverseField = invertDisplacementFieldHelper<Dimension>(
      r_displacementField, r_inverseFieldInitialEstimate, s4_antsrField,
      r_maxNumberOfIterations, r_meanErrorToleranceThreshold,
      r_maxErrorToleranceThreshold, r_enforceBoundaryCondition );
    return( inverseField );
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
