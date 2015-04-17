
#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkVectorImage.h"

namespace ants
{

  template< class ImageType >
  int antsImageWrite( SEXP r_image , // image to write
                      std::string filename )
  {
    Rcpp::Rcout << "antsImageWrite<ImageType>" << std::endl;
    typedef typename ImageType::Pointer ImagePointerType;
    ImagePointerType image = Rcpp::as<ImagePointerType>( r_image );

    typedef itk::ImageFileWriter< ImageType > ImageWriterType ;
    typename ImageWriterType::Pointer image_writer = ImageWriterType::New() ;
    image_writer->SetFileName( filename.c_str() ) ;
    image_writer->SetInput( image );
    image_writer->Update();
    return 0;
  }

} // namespace ants

RcppExport SEXP antsImageWrite( SEXP r_img , SEXP r_filename )
{
try
{
  // check and set the filename
  if( r_img == NULL || r_filename == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  bool verbose = false;
  std::string filename = Rcpp::as< std::string >( r_filename );
  Rcpp::S4 r_image( r_img ) ;
  std::string pixeltype = Rcpp::as< std::string >( r_image.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< unsigned int >( r_image.slot( "dimension" ));
  unsigned int components = Rcpp::as< unsigned int >( r_image.slot( "components"));

  if ( (dimension < 2) || (dimension > 4) )
    {
    Rcpp::stop( "Unsupported image dimension");
    }
  if ( (pixeltype != "double") &&
       (pixeltype != "float") &&
       (pixeltype != "unsigned int") &&
       (pixeltype != "unsigned char") )
    {
    Rcpp::stop( "Unsupported pixeltype");
    }

  Rcpp::Rcout << "Valid image" << std::endl;

  // write the image
  if ( pixeltype == "double" )
    {
    typedef double PixelType;

    if( dimension == 4 )
      {
      const int ImageDimension = 4;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 );
      }
    else if( dimension == 3 )
      {
      const int ImageDimension = 3 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl;
      return Rcpp::wrap( 0 ) ;
      }
    else if( dimension == 2 )
      {
      const int ImageDimension = 2 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl;
      return Rcpp::wrap( 0 );
      }
    }
  else if ( pixeltype == "float" )
    {
    typedef float PixelType;

    if( dimension == 4 )
      {
      const int ImageDimension = 4;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 );
      }
    else if( dimension == 3 )
      {
      const int ImageDimension = 3 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    else if( dimension == 2 )
      {
      const int ImageDimension = 2 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    }
  else if ( pixeltype == "unsigned int" )
    {
    typedef unsigned int PixelType;

    if( dimension == 4 )
      {
      const int ImageDimension = 4;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 );
      }
    else if( dimension == 3 )
      {
      const int ImageDimension = 3 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    else if( dimension == 2 )
      {
      const int ImageDimension = 2 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    }
  else if ( pixeltype == "unsigned char" )
    {
    typedef unsigned char PixelType;

    if( dimension == 4 )
      {
      const int ImageDimension = 4;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 );
      }
    else if( dimension == 3 )
      {
      const int ImageDimension = 3 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    else if( dimension == 2 )
      {
      const int ImageDimension = 2 ;
      typedef itk::Image< PixelType , ImageDimension >      ImageType;
      typedef itk::VectorImage< PixelType, ImageDimension > VectorImageType;

      (components == 1) ?
        ants::antsImageWrite< ImageType >( r_img, filename ) :
        ants::antsImageWrite< VectorImageType >( r_img, filename);

      if ( verbose ) Rcpp::Rcout << "Done writing image. PixelType: 'double' | Dimension: '4'." << std::endl ;
      return Rcpp::wrap( 0 ) ;
      }
    }
}
catch( const itk::ExceptionObject& err )
  {
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
