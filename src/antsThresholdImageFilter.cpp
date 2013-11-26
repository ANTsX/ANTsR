
#include<vector>
#include<string>
#include<Rcpp.h>

#include "itkImage.h"
#include "itkThresholdImageFilter.h"

RcppExport SEXP antsThresholdImageFilter_New( SEXP r_inputimage_pixeltype , 
						    SEXP r_inputimage_dimension
						    )
try
{
  if( r_inputimage_pixeltype == NULL || r_inputimage_dimension == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  std::string inputimage_pixeltype = Rcpp::as< std::string >( r_inputimage_pixeltype ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( r_inputimage_dimension ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;
      FilterPointerType* filter_ptr_ptr = new FilterPointerType( FilterType::New() ) ;
      Rcpp::XPtr< FilterPointerType > xptr( filter_ptr_ptr , true ) ;
      Rcpp::S4 filter_r( std::string( "antsThresholdImageFilter" ) ) ;
      filter_r.slot( "filter" ) = std::string( "antsThresholdImageFilter" ) ;
      filter_r.slot( "pointer" ) = xptr ;
      filter_r.slot( "inputimage_pixeltype" ) = std::string( "double" ) ;
      filter_r.slot( "inputimage_dimension" ) = 4 ;
      return filter_r ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;
      FilterPointerType* filter_ptr_ptr = new FilterPointerType( FilterType::New() ) ;
      Rcpp::XPtr< FilterPointerType > xptr( filter_ptr_ptr , true ) ;
      Rcpp::S4 filter_r( std::string( "antsThresholdImageFilter" ) ) ;
      filter_r.slot( "filter" ) = std::string( "antsThresholdImageFilter" ) ;
      filter_r.slot( "pointer" ) = xptr ;
      filter_r.slot( "inputimage_pixeltype" ) = std::string( "float" ) ;
      filter_r.slot( "inputimage_dimension" ) = 4 ;
      return filter_r ;
    }
  else
    {
      Rcpp::Rcout << "Unsupported Dimension or PixelType for input-image" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_SetInput( SEXP r_filter , SEXP r_image )
try
{
  if( r_filter == NULL || r_image == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or image" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  Rcpp::S4 image( r_image ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;
  std::string image_pixeltype = Rcpp::as< std::string >( image.slot( "pixeltype" ) ) ;
  unsigned int image_dimension = Rcpp::as< unsigned int >( image.slot( "dimension" ) ) ;

  if( inputimage_pixeltype != image_pixeltype || inputimage_dimension != image_dimension )
    {
      Rcpp::Rcout << "Image incompatible with Filter" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< InputImagePointerType > image_xptr( static_cast< SEXP >( image.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->SetInput( ( *image_xptr ) ) ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< InputImagePointerType > image_xptr( static_cast< SEXP >( image.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->SetInput( ( *image_xptr ) ) ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_GetOutput( SEXP r_filter )
try
{
  if( r_filter == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  Rcpp::S4 outputimage( std::string( "antsImage" ) ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;
  outputimage.slot( "pixeltype" ) = inputimage_pixeltype ;
  outputimage.slot( "dimension" ) = inputimage_dimension ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      InputImagePointerType* outputimage_ptr_ptr = new InputImagePointerType( ( *filter_xptr )->GetOutput() ) ;
      Rcpp::XPtr< InputImagePointerType > outputimage_xptr( outputimage_ptr_ptr , true ) ;
      outputimage.slot( "pointer" ) = outputimage_xptr ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      InputImagePointerType* outputimage_ptr_ptr = new InputImagePointerType( ( *filter_xptr )->GetOutput() ) ;
      Rcpp::XPtr< InputImagePointerType > outputimage_xptr( outputimage_ptr_ptr , true ) ;
      outputimage.slot( "pointer" ) = outputimage_xptr ;
    }

  return outputimage ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_Update( SEXP r_filter )
try
{
  if( r_filter == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->Update() ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType >  FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->Update() ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_SetOutsideValue( SEXP r_filter , SEXP r_outsidevalue )
try
{
  if( r_filter == NULL || r_outsidevalue == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or outside-value" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->SetOutsideValue( Rcpp::as< InputImageType::PixelType >( r_outsidevalue ) ) ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->SetOutsideValue( Rcpp::as< InputImageType::PixelType >( r_outsidevalue ) ) ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_ThresholdBelow( SEXP r_filter , SEXP r_thresholdbelow )
try
{
  if( r_filter == NULL || r_thresholdbelow == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or thresholdbelow" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType >  FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdBelow( Rcpp::as< InputImageType::PixelType >( r_thresholdbelow ) ) ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdBelow( Rcpp::as< InputImageType::PixelType >( r_thresholdbelow ) ) ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_ThresholdAbove( SEXP r_filter , SEXP r_thresholdabove )
try
{
  if( r_filter == NULL || r_thresholdabove == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or thresholdabove" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType >  FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdBelow( Rcpp::as< InputImageType::PixelType >( r_thresholdabove ) ) ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdBelow( Rcpp::as< InputImageType::PixelType >( r_thresholdabove ) ) ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsThresholdImageFilter_ThresholdOutside( SEXP r_filter , SEXP r_lower , SEXP r_upper )
try
{
  if( r_filter == NULL || r_lower == NULL || r_upper == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or lower or upper threshold" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType >  FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdOutside( Rcpp::as< InputImageType::PixelType >( r_lower ) ,
					Rcpp::as< InputImageType::PixelType >( r_upper ) 
					) ;
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;
      typedef itk::ThresholdImageFilter< InputImageType > FilterType ;
      typedef FilterType::Pointer FilterPointerType ;

      Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
      ( *filter_xptr )->ThresholdOutside( Rcpp::as< InputImageType::PixelType >( r_lower ) , 
					Rcpp::as< InputImageType::PixelType >( r_upper ) 
					) ;
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
