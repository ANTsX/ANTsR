#include <exception> 
#include <vector>
#include <algorithm>
#include <string> 
#include "RcppANTsR.h"
#include <ants.h>
#include "itkImage.h"
#include "itkWindowedSincInterpolateImageFunction.h"
template< class ImageType >
SEXP timeSeriesSubtractionHelper( SEXP r_antsimage, std::string method )
{
  typedef typename ImageType::Pointer  ImagePointerType; 
  const unsigned int ImageDimension = ImageType::ImageDimension; 
  typedef float                        PixelType; 
  typedef itk::Image< PixelType, ImageDimension > InputImageType; 
  typedef itk::Image< PixelType, ImageDimension - 1 > OutputImageType; 
  typename InputImageType::Pointer filtered;  
  if (method.find("simple") != std::string::npos)
  {
    typedef itk::AlternatingValueSimpleSubtractionImageFilter<InputImageType, 
              InputImageType>              SimpleSubtractionImageFilterType;
    typedef itk::AverageOverDimensionImageFilter<InputImageType, OutputImageType>
                                                    MeanFilterType;
    typename SimpleSubtractionImageFilterType::Pointer subtractFilter = 
      SimpleSubtractionImageFilterType::New(); 
    typename ImageType::Pointer input = Rcpp::as< ImagePointerType >( r_antsimage ); 
    subtractFilter->SetInput( input ); 
    subtractFilter->Update(); 
    filtered = subtractFilter->GetOutput(); 
  } else if( method.find("sinc") != std::string::npos)
  {
    typedef itk::AlternatingValueDifferenceImageFilter< InputImageType, 
            InputImageType > DifferenceFilterType; 
    typedef itk::WindowedSincInterpolateImageFunction< InputImageType, 4 >
      SincInterpolatorType; 
    typedef typename SincInterpolatorType::Pointer   SincInterpolatorPointerType;
    const unsigned int SincRadius = 4; 
    typename DifferenceFilterType::Pointer differenceFilter = 
      DifferenceFilterType::New();
    SincInterpolatorPointerType labelInterp = SincInterpolatorType::New(); 
    SincInterpolatorPointerType controlInterp = SincInterpolatorType::New(); 
    differenceFilter->SetControlInterpolator( controlInterp );
    differenceFilter->SetLabelInterpolator( labelInterp );
    differenceFilter->SetIndexPadding( SincRadius );
    typename ImageType::Pointer input = Rcpp::as< ImagePointerType >( r_antsimage ); 
    differenceFilter->SetInput( input ); 
    differenceFilter->Update(); 
    filtered = differenceFilter->GetOutput(); 
  }  else 
  {
    Rcpp::stop("Unsupported method."); 
  }
  typedef itk::AverageOverDimensionImageFilter<InputImageType, OutputImageType>
                                                  MeanFilterType;
  typename MeanFilterType::Pointer meanFilter = MeanFilterType::New(); 
  meanFilter->SetInput( filtered );
  meanFilter->SetAveragingDimension( ImageDimension - 1 ); 
  meanFilter->SetDirectionCollapseToSubmatrix(); 
  meanFilter->Update(); 
  typename OutputImageType::Pointer output = meanFilter->GetOutput(); 
  SEXP r_out = Rcpp::wrap( output ); 
  return r_out; 
}

RcppExport SEXP timeSeriesSubtraction( SEXP r_antsimage, SEXP method ) 
{
try
{
  Rcpp::S4 antsimage( r_antsimage ); 
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ); 

  if ( ( pixeltype == "float" ) & ( dimension == 4 ) )
  {
    typedef float PixelType; 
    const unsigned int dim = 4; 
    typedef itk::Image< PixelType, dim >  ImageType; 
    SEXP out = timeSeriesSubtractionHelper< ImageType >(r_antsimage, 
        Rcpp::as< std::string >( method ));
    return( out ); 
  }
  else
  {
    Rcpp::stop( "Unsupported image dimension or pixel type." ); 
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
  Rcpp::stop( "C++ exception (unknown reason)"); 
}
 return Rcpp::wrap(NA_REAL); // not reached
}
