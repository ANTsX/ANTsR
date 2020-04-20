#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>

#include "itkLabelOverlapMeasuresImageFilter.h"

#include "antsUtilities.h"
#include "ReadWriteData.h"

#include "RcppANTsR.h"

template<class PrecisionType, unsigned int ImageDimension>
SEXP labelOverlapMeasuresHelper(
  SEXP r_sourceImage,
  SEXP r_targetImage )
{
  using ImageType = itk::Image<PrecisionType, ImageDimension>;
  using ImagePointerType = typename ImageType::Pointer;

  ImagePointerType sourceImage = Rcpp::as<ImagePointerType>( r_sourceImage );
  ImagePointerType targetImage = Rcpp::as<ImagePointerType>( r_targetImage );

  using FilterType = itk::LabelOverlapMeasuresImageFilter<ImageType>;
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetSourceImage( sourceImage );
  filter->SetTargetImage( targetImage );
  filter->Update();

  typename FilterType::MapType labelMap = filter->GetLabelSetMeasures();

  // Sort the labels

  std::vector<PrecisionType> allLabels;
  allLabels.clear();
  for( typename FilterType::MapType::const_iterator it = labelMap.begin();
       it != labelMap.end(); ++it )
    {
    if( (*it).first == 0 )
      {
      continue;
      }

    const int label = (*it).first;
    allLabels.push_back( label );
    }
  std::sort( allLabels.begin(), allLabels.end() );

  // Now put the results in an Rcpp data frame

  unsigned int vectorLength = 1 + allLabels.size();

  Rcpp::NumericVector labels( vectorLength );
  Rcpp::NumericVector totalOrTargetOverlap( vectorLength );
  Rcpp::NumericVector unionOverlap( vectorLength );
  Rcpp::NumericVector meanOverlap( vectorLength );
  Rcpp::NumericVector volumeSimilarity( vectorLength );
  Rcpp::NumericVector falseNegativeError( vectorLength );
  Rcpp::NumericVector falsePositiveError( vectorLength );

  // We'll replace label '0' with "All" in the R wrapper.
  labels[0] = 0;  
  totalOrTargetOverlap[0] = filter->GetTotalOverlap();
  unionOverlap[0] = filter->GetUnionOverlap();
  meanOverlap[0] = filter->GetMeanOverlap();
  volumeSimilarity[0] = filter->GetVolumeSimilarity();
  falseNegativeError[0] = filter->GetFalseNegativeError();
  falsePositiveError[0] = filter->GetFalsePositiveError();

  unsigned int i = 1;
  typename std::vector<PrecisionType>::const_iterator itL = allLabels.begin();
  for( itL = allLabels.begin(); itL != allLabels.end(); ++itL )
    {
    labels[i] = *itL;  
    totalOrTargetOverlap[i] = filter->GetTargetOverlap( *itL );
    unionOverlap[i] = filter->GetUnionOverlap( *itL );
    meanOverlap[i] = filter->GetMeanOverlap( *itL );
    volumeSimilarity[i] = filter->GetVolumeSimilarity( *itL );
    falseNegativeError[i] = filter->GetFalseNegativeError( *itL );
    falsePositiveError[i] = filter->GetFalsePositiveError( *itL );
    i++;
    }

  Rcpp::DataFrame overlapMeasures = 
    Rcpp::DataFrame::create( Rcpp::Named( "Label" ) = labels,        
                             Rcpp::Named( "TotalOrTargetOverlap" ) = totalOrTargetOverlap,
                             Rcpp::Named( "UnionOverlap" ) = unionOverlap,
                             Rcpp::Named( "MeanOverlap" ) = meanOverlap,
                             Rcpp::Named( "VolumeSimilarity" ) = volumeSimilarity,
                             Rcpp::Named( "FalseNegativeError" ) = falseNegativeError,
                             Rcpp::Named( "FalsePositiveError" ) = falsePositiveError
                             ); 
  return( overlapMeasures );
} 

RcppExport SEXP labelOverlapMeasuresR(
  SEXP r_sourceImage,
  SEXP r_targetImage )
{
try
  {
  Rcpp::S4 s4_sourceImage( r_sourceImage );
  Rcpp::S4 s4_targetImage( r_targetImage );

  unsigned int imageDimension = Rcpp::as<int>( s4_sourceImage.slot( "dimension" ) );
  std::string pixelType = Rcpp::as<std::string>( s4_sourceImage.slot( "pixeltype" ) );

  if( imageDimension == 2 )
    {
    const unsigned int ImageDimension = 2;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      }
    } else if( imageDimension == 3 ) {
    const unsigned int ImageDimension = 3;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      }
    } else if( imageDimension == 4 ) {
    const unsigned int ImageDimension = 4;
    if( pixelType.compare( "float" ) == 0 )
      {
      using PrecisionType = float;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "double" ) == 0 ) {
      using PrecisionType = double;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned int" ) == 0 ) {
      using PrecisionType = unsigned int;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      } else if( pixelType.compare( "unsigned char" ) == 0 ) {
      using PrecisionType = unsigned char;
      SEXP overlapMeasures = labelOverlapMeasuresHelper<PrecisionType, ImageDimension>( s4_sourceImage, s4_targetImage );
      return( overlapMeasures );
      }
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


