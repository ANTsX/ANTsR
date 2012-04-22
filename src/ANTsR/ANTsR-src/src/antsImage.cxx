
#include<vector>
#include<string>
#include<Rcpp.h>
#include "itkImage.h"

RcppExport SEXP antsImage( SEXP r_pixeltype , SEXP r_dimension )
try
{
  if( r_pixeltype == NULL || r_dimension == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  unsigned int dimension = Rcpp::as< int >( r_dimension ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '4'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '3'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '2'" << std::endl ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '4'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '3'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '2'" << std::endl ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '4'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '3'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '2'" << std::endl ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }
