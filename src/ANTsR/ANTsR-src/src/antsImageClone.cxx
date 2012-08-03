
#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>

#include "itkImage.h"
#include "itkImageFileWriter.h"

namespace ants
{

  template< class InImageType , class OutImageType >
  typename OutImageType::Pointer antsImageClone( typename InImageType::Pointer in_image )
  {
    typename OutImageType::Pointer out_image = OutImageType::New() ;
    out_image->SetRegions( in_image->GetLargestPossibleRegion() ) ;
    out_image->SetSpacing( in_image->GetSpacing() ) ;
    out_image->SetOrigin( in_image->GetOrigin() ) ;
    out_image->Allocate() ;

    itk::ImageRegionConstIterator< InImageType > in_iterator( in_image , in_image->GetLargestPossibleRegion() ) ;
    itk::ImageRegionIterator< OutImageType > out_iterator( out_image , out_image->GetLargestPossibleRegion() ) ;
    for( in_iterator.GoToBegin() , out_iterator.GoToBegin() ; !in_iterator.IsAtEnd() ; ++in_iterator , ++out_iterator )
      {
	out_iterator.Set( static_cast< typename OutImageType::PixelType >( in_iterator.Get() ) ) ;
      }
    return out_image ;
  }

}

RcppExport SEXP antsImageClone( SEXP r_in_image , SEXP r_out_pixeltype )
{
  if( r_in_image == NULL || r_out_pixeltype == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: input-image or out-pixeltype" << std::endl ;
      Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 in_image( r_in_image ) ;
  std::string in_pixeltype = Rcpp::as< std::string >( in_image.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >( in_image.slot( "dimension" ) ) ;

  std::string out_pixeltype = Rcpp::as< std::string >( r_out_pixeltype ) ;
  Rcpp::S4 out_image( std::string( "antsImage" ) ) ;
  out_image.slot( "pixeltype" ) = out_pixeltype ;
  out_image.slot( "dimension" ) = dimension ;

  if( in_pixeltype == std::string( "double" ) && dimension == 4 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 4 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 3 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 3 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 2 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 2 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 4 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 4 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 3 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 3 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 2 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 2 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 4 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 4 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 3 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 3 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 2 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 2 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 4 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 4 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 3 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 3 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "double" ) && dimension == 2 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 2 ;
      typedef double InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 4 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 4 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 3 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 3 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 2 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 2 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 4 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 4 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 3 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 3 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 2 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 2 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 4 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 4 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 3 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 3 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 2 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 2 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 4 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 4 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 3 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 3 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "float" ) && dimension == 2 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 2 ;
      typedef float InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 4 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 3 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 2 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 4 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 3 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 2 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 4 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 3 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 2 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 4 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 3 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned int" ) && dimension == 2 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 4 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 3 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 2 && out_pixeltype == std::string( "double" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef double OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 4 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 3 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 2 && out_pixeltype == std::string( "float" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef float OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 4 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 3 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 2 && out_pixeltype == std::string( "unsigned int" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned int OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 4 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 4 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 3 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 3 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else if( in_pixeltype == std::string( "unsigned char" ) && dimension == 2 && out_pixeltype == std::string( "unsigned char" ) )
    {
      const int ImageDimension = 2 ;
      typedef unsigned char InPixelType ;
      typedef itk::Image< InPixelType , ImageDimension > InImageType ;
      typedef InImageType::Pointer InImagePointerType ;

      typedef unsigned char OutPixelType ;
      typedef itk::Image< OutPixelType , ImageDimension > OutImageType ;
      typedef OutImageType::Pointer OutImagePointerType ;

      Rcpp::XPtr< InImagePointerType > in_image_xptr( static_cast< SEXP >( in_image.slot( "pointer" ) ) ) ;
      OutImagePointerType* out_image_ptr_ptr = new OutImagePointerType( ants::antsImageClone< InImageType , OutImageType >( *in_image_xptr ) ) ;
      Rcpp::XPtr< OutImagePointerType > out_image_xptr( out_image_ptr_ptr , true ) ;
      out_image.slot( "pointer" ) = out_image_xptr ;
    }
  else
    {
      Rcpp::Rcout << "Usupported PixelType or Dimension" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
  
  return out_image ;
}
