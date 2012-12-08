
#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>

#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"

namespace ants
{

  template< class ImageType >
  int antsImageWrite( typename ImageType::Pointer image , // image to write 
		      std::string filename // target file
		      )
  {
    typedef itk::ImageFileWriter< ImageType > ImageWriterType ;
    typename ImageWriterType::Pointer image_writer = ImageWriterType::New() ;
    image_writer->SetFileName( filename.c_str() ) ;
    image_writer->SetInput( image );
    try
      {
      image_writer->Write();
      }    
    catch( itk::ExceptionObject & e )
      {
      Rcpp::Rcout << "Exception caught during reference file writing " << std::endl;
      Rcpp::Rcout << e << std::endl;
      return 1;
      }

    return 0 ;
  }

} // namespace ants

RcppExport SEXP antsImageWrite( SEXP r_img , SEXP r_filename )
{
  // check and set the filename
  if( r_img == NULL || r_filename == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
  std::string filename = Rcpp::as< std::string >( r_filename ) ;
  Rcpp::S4 r_image( r_img ) ;
  std::string pixeltype = Rcpp::as< std::string >( r_image.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >( r_image.slot( "dimension" ) ) ;

  // write the image
  if( pixeltype == "double" && dimension == 4 )
    {
      const int ImageDimension = 4 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "double" && dimension == 3 )
    {
      const int ImageDimension = 3 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '3'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "double" && dimension == 2 )
    {
      const int ImageDimension = 2 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '2'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "float" && dimension == 4 )
    {
      const int ImageDimension = 4 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'float' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "float" && dimension == 3 )
    {
      const int ImageDimension = 3 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'float' | Dimension: '3'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "float" && dimension == 2 )
    {
      const int ImageDimension = 2 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'float' | Dimension: '2'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned int" && dimension == 4 )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned int' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned int" && dimension == 3 )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned int' | Dimension: '3'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned int" && dimension == 2 )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned int' | Dimension: '2'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned char" && dimension == 4 )
    {
      const int ImageDimension = 4 ;
      typedef unsigned char PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned char' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned char" && dimension == 3 )
    {
      const int ImageDimension = 3 ;
      typedef unsigned char PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned char' | Dimension: '3'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else if( pixeltype == "unsigned char" && dimension == 2 )
    {
      const int ImageDimension = 2 ;
      typedef unsigned char PixelType ;
      typedef itk::Image< PixelType , ImageDimension > ImageType ;
      typedef ImageType::Pointer ImagePointerType ;
      Rcpp::XPtr< ImagePointerType > xptr( static_cast< SEXP >( r_image.slot( "pointer" ) ) ) ;
      ants::antsImageWrite< ImageType >( *xptr , filename ) ;
      Rcpp::Rcout << "Done writing image. PixelType: 'unsigned char' | Dimension: '2'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
    }
  else
    {
      Rcpp::Rcout << "Usupported PixelType or Dimension" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
