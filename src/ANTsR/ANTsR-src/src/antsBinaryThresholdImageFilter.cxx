
#include<vector>
#include<string>
#include<Rcpp.h>

#include "itkImage.h"
#include "itkBinaryThresholdImageFilter.h"

RcppExport SEXP antsBinaryThresholdImageFilter_New( SEXP r_inputimage_pixeltype , 
						SEXP r_inputimage_dimension , 
						SEXP r_outputimage_pixeltype , 
						SEXP r_outputimage_dimension 
						)
try
{
  if( r_inputimage_pixeltype == NULL || r_inputimage_dimension == NULL || r_outputimage_pixeltype == NULL || r_outputimage_dimension == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  std::string inputimage_pixeltype = Rcpp::as< std::string >( r_inputimage_pixeltype ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( r_inputimage_dimension ) ;

  std::string outputimage_pixeltype = Rcpp::as< std::string >( r_outputimage_pixeltype ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( r_outputimage_dimension ) ;

  // read the image using the above info
  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;
	  FilterPointerType* filter_ptr_ptr = new FilterPointerType( FilterType::New() ) ;
	  Rcpp::XPtr< FilterPointerType > xptr( filter_ptr_ptr , true ) ;
	  Rcpp::S4 filter_r( std::string( "antsBinaryThresholdImageFilter" ) ) ;
	  filter_r.slot( "filter" ) = std::string( "antsBinaryThresholdImageFilter" ) ;
	  filter_r.slot( "pointer" ) = xptr ;
	  filter_r.slot( "inputimage_pixeltype" ) = std::string( "double" ) ;
	  filter_r.slot( "inputimage_dimension" ) = 4 ;
	  filter_r.slot( "outputimage_pixeltype" ) = std::string( "double" ) ;
	  filter_r.slot( "outputimage_dimension" ) = 4 ;
	  return filter_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension or PixelType for output-image" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;
	  FilterPointerType* filter_ptr_ptr = new FilterPointerType( FilterType::New() ) ;
	  Rcpp::XPtr< FilterPointerType > xptr( filter_ptr_ptr , true ) ;
	  Rcpp::S4 filter_r( std::string( "antsBinaryThresholdImageFilter" ) ) ;
	  filter_r.slot( "pointer" ) = xptr ;
	  filter_r.slot( "inputimage_pixeltype" ) = std::string( "float" ) ;
	  filter_r.slot( "inputimage_dimension" ) = 4 ;
	  filter_r.slot( "outputimage_pixeltype" ) = std::string( "float" ) ;
	  filter_r.slot( "outputimage_dimension" ) = 4 ;
	  return filter_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension or PixelType for output-image" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
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

RcppExport SEXP antsBinaryThresholdImageFilter_SetInput( SEXP r_filter , SEXP r_image )
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
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;
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

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  Rcpp::XPtr< InputImagePointerType > image_xptr( static_cast< SEXP >( image.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetInput( ( *image_xptr ) ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  Rcpp::XPtr< InputImagePointerType > image_xptr( static_cast< SEXP >( image.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetInput( ( *image_xptr ) ) ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_GetOutput( SEXP r_filter )
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
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;
  outputimage.slot( "pixeltype" ) = outputimage_pixeltype ;
  outputimage.slot( "dimension" ) = outputimage_dimension ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;
	  typedef OutputImageType::Pointer OutputImagePointerType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  OutputImagePointerType* outputimage_ptr_ptr = new OutputImagePointerType( ( *filter_xptr )->GetOutput() ) ;
	  Rcpp::XPtr< OutputImagePointerType > outputimage_xptr( outputimage_ptr_ptr , true ) ;
	  outputimage.slot( "pointer" ) = outputimage_xptr ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;
	  typedef OutputImageType::Pointer OutputImagePointerType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  OutputImagePointerType* outputimage_ptr_ptr = new OutputImagePointerType( ( *filter_xptr )->GetOutput() ) ;
	  Rcpp::XPtr< OutputImagePointerType > outputimage_xptr( outputimage_ptr_ptr , true ) ;
	  outputimage.slot( "pointer" ) = outputimage_xptr ;
	}
    }

  return outputimage ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_Update( SEXP r_filter )
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
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;
	  typedef OutputImageType::Pointer OutputImagePointerType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->Update() ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;
	  typedef OutputImageType::Pointer OutputImagePointerType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->Update() ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_SetOutsideValue( SEXP r_filter , SEXP r_outsidevalue )
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
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetOutsideValue( Rcpp::as< typename OutputImageType::PixelType >( r_outsidevalue ) ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetOutsideValue( Rcpp::as< typename OutputImageType::PixelType >( r_outsidevalue ) ) ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_SetInsideValue( SEXP r_filter , SEXP r_insidevalue )
try
{
  if( r_filter == NULL || r_insidevalue == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or inside-value" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetInsideValue( Rcpp::as< typename OutputImageType::PixelType >( r_insidevalue ) ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetInsideValue( Rcpp::as< typename OutputImageType::PixelType >( r_insidevalue ) ) ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_SetLowerThreshold( SEXP r_filter , SEXP r_lowerthreshold )
try
{
  if( r_filter == NULL || r_lowerthreshold == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or lowerthreshold-value" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetLowerThreshold( Rcpp::as< typename InputImageType::PixelType >( r_lowerthreshold ) ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetLowerThreshold( Rcpp::as< typename InputImageType::PixelType >( r_lowerthreshold ) ) ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }

RcppExport SEXP antsBinaryThresholdImageFilter_SetUpperThreshold( SEXP r_filter , SEXP r_upperthreshold )
try
{
  if( r_filter == NULL || r_upperthreshold == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: filter or upperthreshold-value" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 filter( r_filter ) ;
  std::string inputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "inputimage_pixeltype" ) ) ;
  unsigned int inputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "inputimage_dimension" ) ) ;
  std::string outputimage_pixeltype = Rcpp::as< std::string >( filter.slot( "outputimage_pixeltype" ) ) ;
  unsigned int outputimage_dimension = Rcpp::as< unsigned int >( filter.slot( "outputimage_dimension" ) ) ;

  if( inputimage_pixeltype == "double" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef double InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "double" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef double OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetUpperThreshold( Rcpp::as< typename InputImageType::PixelType >( r_upperthreshold ) ) ;
	}
    }
  else if( inputimage_pixeltype == "float" && inputimage_dimension == 4 )
    {
      const int InputImageDimension = 4 ;
      typedef float InputImagePixelType ;
      typedef itk::Image< InputImagePixelType , InputImageDimension > InputImageType ;
      typedef InputImageType::Pointer InputImagePointerType ;

      if( outputimage_pixeltype == "float" && outputimage_dimension == 4 )
	{
	  const int OutputImageDimension = 4 ;
	  typedef float OutputImagePixelType ;
	  typedef itk::Image< OutputImagePixelType , OutputImageDimension > OutputImageType ;

	  typedef itk::BinaryThresholdImageFilter< InputImageType , OutputImageType >  FilterType ;
	  typedef FilterType::Pointer FilterPointerType ;

	  Rcpp::XPtr< FilterPointerType > filter_xptr( static_cast< SEXP >( filter.slot( "pointer" ) ) ) ;
	  ( *filter_xptr )->SetUpperThreshold( Rcpp::as< typename InputImageType::PixelType >( r_upperthreshold ) ) ;
	}
    }

  return Rcpp::wrap( 0 ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
