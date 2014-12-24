
#include<algorithm>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkContinuousIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkPermuteAxesImageFilter.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '4'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '3'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'double' | Dimension: '2'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '4'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '3'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'float' | Dimension: '2'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '4'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '3'" << std::endl ;
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
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned int' | Dimension: '2'" << std::endl ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '4'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '3'" << std::endl ;
	  return image_r ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  // Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '2'" << std::endl ;
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

template< typename ImageType >
Rcpp::IntegerVector antsImage_isna( typename ImageType::Pointer image )
{
  if ( ! image.IsNotNull() )
    {
    return Rcpp::wrap( 1 );
    }
  return Rcpp::wrap( 0 );
}

RcppExport SEXP antsImage_isna( SEXP r_antsimage )
try
{
  if( r_antsimage == NULL )
    {
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_isna< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
        //Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else
    {
    //Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }



template< typename ImageType >
Rcpp::IntegerVector antsImage_dim( typename ImageType::Pointer image )
{
  if ( ! image.IsNotNull() )
    {
    Rcpp::Rcout << "Image not yet allocated" << std::endl;
    return Rcpp::wrap( 1 );
    }

  Rcpp::IntegerVector dim_r( ImageType::ImageDimension ) ;
  for( unsigned int i = 0 ; i < ImageType::ImageDimension ; ++i )
    {
      dim_r[i] = image->GetLargestPossibleRegion().GetSize(i) ;
    }
  return dim_r ;
}

RcppExport SEXP antsImage_dim( SEXP r_antsimage )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_dim< ImageType >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }


template< class PixelType , unsigned int Dimension >
SEXP antsImage_asVector( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_mask , SEXP r_antsregion )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      typename ImageType::RegionType region ;
      Rcpp::S4 antsregion( r_antsregion ) ;
      Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) ) ;
      Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) ) ;
      if( indexvector.size() == 0 && sizevector.size() == 0 )
	{
	  region = image->GetLargestPossibleRegion() ;
	}
      else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	{
	  Rcpp::Rcout << "antsRegion provided has dimensions incompatible with the image" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
      else
	{
	  typename ImageType::IndexType index ;
	  typename ImageType::SizeType size ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	    }
	  region.SetSize( size ) ;
	  region.SetIndex( index ) ;
	  if( !image->GetLargestPossibleRegion().IsInside( region ) )
	    {
	      Rcpp::Rcout << "'region' is not inside the image" << std::endl ;
	      return Rcpp::wrap( NA_REAL ) ;
	    }
	}
      itk::ImageRegionConstIterator< ImageType > image_iter( image , region ) ;

      Rcpp::LogicalVector mask( r_mask ) ;
      if( mask.size() == 0 )
	{
	  Rcpp::NumericVector vector_r( region.GetNumberOfPixels() ) ;
	  unsigned int vector_r_ind = 0 ;
	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[vector_r_ind++] = image_iter.Get() ;
	    }

	  // set dimensions of the R vector;
	  // dim[0] = num-of-rows, dim[1] = num-of-cols, dim[2] = num-of-slices, dim[3] = num-of-time
	  Rcpp::IntegerVector dims( Dimension ) ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      dims[i] = region.GetSize(i) ;
	    }
	  vector_r.attr( "dim" ) = dims ;
	  return vector_r ;
	}
      else
	{
	  int numberofpixelspertime = region.GetSize(0) ;
	  for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
	    {
	      numberofpixelspertime *= region.GetSize(i) ;
	    }
	  if( mask.size() != numberofpixelspertime && mask.size() != (int)region.GetNumberOfPixels() )
	    {
	      Rcpp::Rcout << "Length of mask vector does not match image-region dimensions" << std::endl ;
	      return Rcpp::wrap( NA_REAL ) ;
	    }

	  Rcpp::LogicalVector::iterator mask_iter = mask.begin() ;
	  // set the length of the R vector to be number of logical TRUEs in the mask provided
	  Rcpp::NumericVector vector_r( std::count( mask.begin() , mask.end() , TRUE ) *
					( mask.size() == numberofpixelspertime ? region.GetSize(Dimension-1) : 1 )
					) ;
	  unsigned int vector_r_ind = 0 ;
	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
	    {
	      // in case mask only covers one time index, reuse the mask for every time index
	      if( mask_iter == mask.end() )
		mask_iter = mask.begin() ;
	      if( *mask_iter == TRUE )
		vector_r[vector_r_ind++] = image_iter.Get() ;
	    }
	  return vector_r ;
	}
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_asVector( SEXP r_antsimage , SEXP r_mask , SEXP r_antsregion )
try
{
  if( r_antsimage == NULL || r_mask == NULL || r_antsregion == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_asVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }

template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetNeighborhoodMatrix( typename itk::Image< PixelType , Dimension >::Pointer image ,
                                      typename itk::Image< PixelType , Dimension >::Pointer mask,
                                      SEXP r_radius, SEXP r_physical, SEXP r_boundary, SEXP r_spatial )
{

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef typename ImageType::RegionType   RegionType;
  typedef typename ImageType::IndexType    IndexType;
  typedef typename ImageType::PointType    PointType;

  Rcpp::NumericVector radius( r_radius ) ;
  int physical = Rcpp::as<int>( r_physical );
  int boundary = Rcpp::as<int>( r_boundary );
  int spatial = Rcpp::as<int>( r_spatial );

  typename itk::NeighborhoodIterator<ImageType>::SizeType nSize;

  unsigned long maxSize = 1;
  for ( unsigned int i=0; i<Dimension; i++ )
    {
    maxSize *= ( 1 + 2*radius[i] );
    nSize[i] = radius[i];
    }

  std::vector<double> pixelList;
  pixelList.reserve(maxSize);

  itk::ImageRegionIteratorWithIndex<ImageType> it( mask, mask->GetLargestPossibleRegion() ) ;
  itk::NeighborhoodIterator<ImageType> nit( nSize, image, image->GetLargestPossibleRegion() ) ;

  unsigned long nVoxels = 0;
  while( !it.IsAtEnd() )
    {
    if ( it.Value() > 0 )
      {
      ++nVoxels;
      }
    ++it;
    }

  //Rcpp::Rcout << "Allocating matrix of size: " << maxSize << " x " << nVoxels << std::endl;
  Rcpp::NumericMatrix matrix(maxSize, nVoxels);
  Rcpp::NumericMatrix indices(nVoxels, Dimension);
  //Rcpp::Rcout << "Filling the matrix" << std::endl;

  // Get relative offsets of neighborhood locations
  Rcpp::NumericMatrix offsets(nit.Size(), Dimension);
  for ( unsigned int i=0; i < nit.Size(); i++ )
    {
    for ( unsigned int j=0; j<Dimension; j++)
      {
      offsets(i,j) = nit.GetOffset(i)[j];
      if ( physical )
        {
        offsets(i,j) = offsets(i,j) * image->GetSpacing()[j];
        }
      }
    }


  unsigned int col = 0;
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if ( it.Value() > 0 )
      {
      PointType pt;

      if ( physical )
        {
        image->TransformIndexToPhysicalPoint(it.GetIndex(), pt);
        }

      for ( unsigned int i=0; i < Dimension; i++)
        {
        if ( physical )
          {
          indices(col,i) = pt[i];
          }
        else
          {
          indices(col,i) = it.GetIndex()[i];
          }
        }

      double mean = 0;
      double count = 0;
      for ( unsigned int row=0; row < nit.Size(); row++ )
        {
        IndexType idx = it.GetIndex() + nit.GetOffset(row);

        // check boundary conditions
        if ( mask->GetRequestedRegion().IsInside(idx) )
          {
          if ( mask->GetPixel(idx) > 0 ) // fully within boundaries
            {
            matrix(row,col) = nit.GetPixel(row);
            mean += nit.GetPixel(row);
            ++count;
            }
          else
            {
            if ( boundary == 1 )
              {
              matrix(row,col)  = nit.GetPixel(row);
              }
            else
              {
              matrix(row,col) = NA_REAL;
              }
            }
          }
        else
          {
          matrix(row,col) = NA_REAL;
          }
        }

      if ( boundary == 2 )
        {
        mean /= count;
        for ( unsigned int row=0; row < nit.Size(); row++ )
          {
          if ( matrix(row,col) != matrix(row,col) )
            {
            matrix(row,col) = mean;
            }
          }
        }

      ++col;
      }
    ++it;
    ++nit;
    }

  if ( spatial )
    {
    return Rcpp::List::create( Rcpp::Named("values") = matrix,
                               Rcpp::Named("indices") = indices,
                               Rcpp::Named("offsets") = offsets );
    }
  else
    {
    return ( matrix );
    }
}

RcppExport SEXP antsImage_GetNeighborhoodMatrix( SEXP r_antsimage, SEXP r_maskimage, SEXP r_radius,
                                                 SEXP r_physical, SEXP r_boundary, SEXP r_spatial )
try
{
  if ( r_antsimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  if ( r_maskimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  Rcpp::S4 maskimage( r_maskimage ) ;

  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  Rcpp::NumericVector radius( r_radius ) ;

  if ( radius.size() != dimension )
    {
    Rcpp::Rcout << "Radius must have same dimension as image" << std::endl ;
    return Rcpp::wrap( NA_REAL );
    }

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }

  else
    {
    Rcpp::Rcout << "Unsupported pixel type: " << pixeltype << std::endl;
    return Rcpp::wrap( NA_REAL );
    }
}
catch( const std::exception& exc )
{
  Rcpp::Rcout<< exc.what() << std::endl ;
  return Rcpp::wrap( 1 ) ;
}


template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetNeighborhood( typename itk::Image< PixelType , Dimension >::Pointer image ,
                                SEXP r_index, SEXP r_radius, SEXP physical )
{

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef typename ImageType::RegionType   RegionType;
  typedef typename ImageType::IndexType    IndexType;

  Rcpp::NumericVector radius( r_radius ) ;
  Rcpp::NumericVector index( r_index ) ;
  int physicalFlag = Rcpp::as<int>( physical );

  // Rcpp::Rcout << "Physical Flag = " << physicalFlag << std::endl;

  unsigned long maxSize = 1;
  for ( unsigned int i=0; i<Dimension; i++ )
    {
    maxSize *= ( 1 + 2*radius[i] );
    }

  std::vector<double> pixelList;
  pixelList.reserve(maxSize);

  std::vector<IndexType> indexList;
  indexList.reserve(maxSize);

  RegionType region;
  for (unsigned int i=0; i<Dimension; i++)
    {
    int offset = index[i] - radius[i] - 1;  // -1 is for R indexing to ITK indexing conversion

    if ( offset < 0 )
      {
      offset = 0;
      }

    int limit = index[i] + radius[i];
    if ( limit > ( image->GetLargestPossibleRegion().GetSize()[i]-1) )
      {
      limit = image->GetLargestPossibleRegion().GetSize()[i]-1;
      }

    region.SetSize(i, limit-offset );
    region.SetIndex(i, offset );
    }

  itk::ImageRegionIteratorWithIndex<ImageType> it( image, region );
  while( !it.IsAtEnd() )
    {
    //Rcpp::Rcout << it.GetIndex() << " " << it.Value() << std::endl;
    pixelList.push_back( it.Value() );
    indexList.push_back( it.GetIndex() );
    ++it;
    }

  Rcpp::NumericVector pixels( pixelList.begin(), pixelList.end() );
  Rcpp::NumericMatrix indices( pixels.size(), Dimension );
  for ( unsigned int i=0; i<pixels.size(); i++)
    {
    typename ImageType::PointType pt;
    if ( physicalFlag )
      {
      image->TransformIndexToPhysicalPoint( indexList[i], pt );
      }

    for ( unsigned int j=0; j<Dimension; j++)
      {
      if ( !physicalFlag )
        {
        indices(i,j) = indexList[i][j];
        }
      else
        {
        indices(i,j) = pt[j];
        }
      }
    }

  return Rcpp::List::create( Rcpp::Named("values") = pixels,
                             Rcpp::Named("indices") = indices );

}

RcppExport SEXP antsImage_GetNeighborhood( SEXP r_antsimage, SEXP r_index, SEXP r_radius, SEXP physical )
try
{
  if ( r_antsimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  Rcpp::NumericVector radius( r_radius ) ;
  Rcpp::NumericVector index( r_index ) ;

  if ( radius.size() != dimension )
    {
    Rcpp::Rcout << "Radius must have same dimension as image" << std::endl ;
    return Rcpp::wrap( NA_REAL );
    }
  if ( index.size() != dimension )
    {
    Rcpp::Rcout << "Index must have same dimension as image" << std::endl ;
    return Rcpp::wrap( NA_REAL );
    }

  unsigned long maxSize = 1;
  for ( unsigned int i=0; i<dimension; i++ )
    {
    maxSize *= ( 1 + 2*radius[i] );
    }

  std::vector<double> pixelList;

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }

  else
    {
    Rcpp::Rcout << "Unsupported pixel type: " << pixeltype << std::endl;
    return Rcpp::wrap( NA_REAL );
    }
}
catch( const std::exception& exc )
{
  Rcpp::Rcout<< exc.what() << std::endl ;
  return Rcpp::wrap( 1 ) ;
}


template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetPixels( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_coordinates )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
    Rcpp::List list_coordinates( r_coordinates ) ;
    std::vector< std::vector< double > > coordinates ;

    if( list_coordinates.size() != Dimension )
      {
	    Rcpp::Rcout << "indices do not match the image in dimensions" << std::endl ;
	    return Rcpp::wrap( NA_REAL ) ;
	    }
    for( int i = 0 ; i < list_coordinates.size() ; ++i )
	    {
	    coordinates.push_back( Rcpp::as< std::vector< double > >( list_coordinates[i] ) ) ;
	    }

    unsigned int vector_r_size = 1 ;
    for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	    // if no coordinates are provided for a dimension, assume entire extent of the dimension
	    if( coordinates[i].size() == 0 )
	      {
	      coordinates[i].reserve( image->GetLargestPossibleRegion().GetSize(i) ) ;

        for( unsigned int j = 0 ; j < image->GetLargestPossibleRegion().GetSize(i) ; ++j )
		      {
		      coordinates[i][j] = j ;
		      }
	       }
	    vector_r_size *= coordinates[i].size() ;
	    }

      Rcpp::NumericVector vector_r( vector_r_size ) ;
      std::vector< unsigned int > ind( Dimension ) ;
      typename ImageType::IndexType index ;
      for( unsigned int i = 0 ; i < vector_r_size ; ++i )
	{
	  for( unsigned int j = 0 ; j < Dimension ; ++j )
	    {
	      index[j] = coordinates[ j ][ ind[j] ] - 1 ;
	    }
	  if( !image->GetLargestPossibleRegion().IsInside( index ) )
	    {
	      Rcpp::Rcout << "index not inside the image : " << index << std::endl ;
	      return Rcpp::wrap( NA_REAL ) ;
	    }
	  vector_r[i] = image->GetPixel( index ) ;
	  ++ind[0] ;
	  for( unsigned int j = 0 ; j < Dimension - 1 ; ++j )
	    {
	      if( ind[j] == coordinates[j].size() )
		{
		  ++ind[j+1] ;
		  ind[j] = 0 ;
		}
	    }
	}

      // set dimensions of the R vector using number of coordinates given for each dimension;
      // dim[0] = x-dimension, dim[1] = y-dimension, dim[2] = z-dimension, dim[3] = t-dimension
      Rcpp::IntegerVector dims( Dimension ) ;
      for( unsigned int i = 0 ; i < Dimension ; ++i )
	{
	  dims[i] = coordinates[i].size() ;
	}
      vector_r.attr( "dim" ) = dims ;
      return vector_r ;
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_GetPixels( SEXP r_antsimage , SEXP r_coordinates )
try
{
  if( r_antsimage == NULL || r_coordinates == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates ) ;
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


template< class PixelType , unsigned int Dimension >
bool antsImage_SetPixels( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_coordinates , SEXP r_value )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      Rcpp::List list_coordinates( r_coordinates ) ;
      std::vector< std::vector< double > > coordinates ;
      if( list_coordinates.size() != Dimension )
	{
	  Rcpp::Rcout << "indices do not match the image in dimensions" << std::endl ;
	  return 1 ;
	}
      for( int i = 0 ; i < list_coordinates.size() ; ++i )
	{
	  coordinates.push_back( Rcpp::as< std::vector< double > >( list_coordinates[i] ) ) ;
	}

      unsigned int value_size = 1 ;
      for( unsigned int i = 0 ; i < Dimension ; ++i )
	{
	  if( coordinates[i].size() == 0 )
	    {
	      coordinates[i].reserve( image->GetLargestPossibleRegion().GetSize(i) ) ;
	      for( unsigned int j = 0 ; j < image->GetLargestPossibleRegion().GetSize(i) ; ++j )
		{
		  coordinates[i].push_back( j ) ;
		}
	    }
	  value_size *= coordinates[i].size() ;
	}
      Rcpp::NumericVector value( r_value ) ;
      if( value.size() != (int)value_size && value.size() != 1 )
	{
	  Rcpp::Rcout << "rhs vector must be scalar or of same length as indices" << std::endl ;
	  return 1 ;
	}
      std::vector< unsigned int > ind( Dimension ) ;
      typename ImageType::IndexType index ;
      if( value.size() == 1 )
	{
	  for( unsigned int i = 0 ; i < value_size ; ++i )
	    {
	      for( unsigned int j = 0 ; j < Dimension ; ++j )
		{
		  index[j] = coordinates[ j ][ ind[j] ] ;
		}
	      image->SetPixel( index , value[0] ) ;
	      ++ind[0] ;
	      for( unsigned int j = 0 ; j < Dimension - 1 ; ++j )
		{
		  if( ind[j] == coordinates[j].size() )
		    {
		      ++ind[j+1] ;
		      ind[j] = 0 ;
		    }
		}
	    }
	}
      else
	{
	  for( unsigned int i = 0 ; i < value_size ; ++i )
	    {
	      for( unsigned int j = 0 ; j < Dimension ; ++j )
		{
		  index[j] = coordinates[ j ][ ind[j] ] ;
		}
	      image->SetPixel( index , value[i] ) ;
	      ++ind[0] ;
	      for( unsigned int j = 0 ; j < Dimension - 1 ; ++j )
		{
		  if( ind[j] == coordinates[j].size() )
		    {
		      ++ind[j+1] ;
		      ind[j] = 0 ;
		    }
		}
	    }
	}
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return 1 ;
    }
  return 0 ;
}

RcppExport SEXP antsImage_SetPixels( SEXP r_antsimage , SEXP r_coordinates , SEXP r_value )
try
{
  if( r_antsimage == NULL || r_coordinates == NULL || r_value == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
    }
  return r_antsimage ;
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }


template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetSpacing( typename itk::Image< PixelType , Dimension >::Pointer image )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  if( image.IsNotNull() )
    {
    Rcpp::NumericVector vector_r( Dimension ) ;

    for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
      vector_r[i] = image->GetSpacing()[i];
	    }

    // set dimensions of the R vector using number of coordinates given for each dimension;
    // dim[0] = x-dimension, dim[1] = y-dimension, dim[2] = z-dimension, dim[3] = t-dimension
    Rcpp::IntegerVector dims( 1 ) ;
    dims[0] = Dimension ;
    vector_r.attr( "dim" ) = dims ;
    return vector_r;
    }
  else
    {
    // Rcpp::Rcout << "Empty image" << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_GetSpacing( SEXP r_antsimage )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetSpacing< PixelType , ImageDimension >( *antsimage_xptr ) ;
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

template< class PixelType , unsigned int Dimension >
bool antsImage_SetSpacing( typename itk::Image< PixelType , Dimension >::Pointer image, SEXP r_spacing )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  Rcpp::NumericVector spacing( r_spacing ) ;
  typename ImageType::SpacingType itkspacing;

  if( spacing.size() != Dimension )
    {
    Rcpp::Rcout << "indices do not match the image in dimensions" << std::endl;
    return 1 ;
    }
  for( int i = 0 ; i < spacing.size() ; ++i )
    {
    itkspacing[i] = spacing[i];
    }

  if( image.IsNotNull() )
    {
    image->SetSpacing( itkspacing );
    return 0;
    }
  else
    {
    // Rcpp::Rcout << "Empty image" << std::endl ;
    return 1;
    }
}

RcppExport SEXP antsImage_SetSpacing( SEXP r_antsimage, SEXP r_spacing )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetSpacing< PixelType , ImageDimension >( *antsimage_xptr, r_spacing ) ;
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
  return Rcpp::wrap( 0 );
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }

template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetOrigin( typename itk::Image< PixelType , Dimension >::Pointer image )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  if( image.IsNotNull() )
    {

      Rcpp::NumericVector vector_r( Dimension ) ;

      for( unsigned int i = 0 ; i < Dimension ; ++i )
	{
        vector_r[i] = image->GetOrigin()[i];
	}

      // set dimensions of the R vector using number of coordinates given for each dimension;
      // dim[0] = x-dimension, dim[1] = y-dimension, dim[2] = z-dimension, dim[3] = t-dimension
      Rcpp::IntegerVector dims( 1 ) ;
      dims[0] = Dimension ;
      vector_r.attr( "dim" ) = dims ;
      return vector_r;
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_GetOrigin( SEXP r_antsimage )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetOrigin< PixelType , ImageDimension >( *antsimage_xptr ) ;
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

template< class PixelType , unsigned int Dimension >
bool antsImage_SetOrigin( typename itk::Image< PixelType , Dimension >::Pointer image, SEXP r_origin )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  Rcpp::NumericVector origin( r_origin ) ;
  typename ImageType::PointType itkorigin;

  if( origin.size() != Dimension )
    {
    Rcpp::Rcout << "origin does not match the image in dimensions" << std::endl;
    return 1 ;
    }
  for( int i = 0 ; i < origin.size() ; ++i )
    {
    itkorigin[i] = origin[i];
    }

  if( image.IsNotNull() )
    {
    image->SetOrigin( itkorigin );
    return 0;
    }
  else
    {
    // Rcpp::Rcout << "Empty image" << std::endl ;
    return 1;
    }
}

RcppExport SEXP antsImage_SetOrigin( SEXP r_antsimage, SEXP r_origin )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetOrigin< PixelType , ImageDimension >( *antsimage_xptr, r_origin ) ;
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
  return Rcpp::wrap( 0 );
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }

template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetDirection( typename itk::Image< PixelType , Dimension >::Pointer image )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  //typedef vnl_vector< ElementType > VectorType ;
  //typedef std::vector< std::string > HeaderType ;
  //typedef std::pair< HeaderType , MatrixType > antsMatrixType ;

  if( image.IsNotNull() )
    {

    //antsMatrixType* ptr_matrix = new antsMatrixType ;
    //Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
    //Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
    //matrix_r.slot( "elementtype" ) = std::string( "double" ) ;
    //matrix_r.slot( "pointer" ) = xptr ;
    //Rcpp::Rcout << "Empty matrix created. ElementType: 'double'" << std::endl ;
    //return matrix_r ;

    Rcpp::NumericVector vector_r( Dimension*Dimension ) ;

    unsigned int idx = 0;
    for( unsigned int i = 0 ; i < Dimension ; ++i )
      {
      for ( unsigned int j=0; j<Dimension; j++ )
        {
        vector_r[idx++] = image->GetDirection()(i,j);
        }
      }

    // set dimensions of the R vector using number of coordinates given for each dimension;
    // dim[0] = x-dimension, dim[1] = y-dimension, dim[2] = z-dimension, dim[3] = t-dimension
    Rcpp::IntegerVector dims( 2 ) ;
    dims[0] = Dimension;
    dims[1] = Dimension;
    vector_r.attr( "dim" ) = dims ;
    return vector_r;
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_GetDirection( SEXP r_antsimage )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetDirection< PixelType , ImageDimension >( *antsimage_xptr ) ;
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

template< class PixelType , unsigned int Dimension >
SEXP antsImage_SetDirection( typename itk::Image< PixelType , Dimension >::Pointer image, SEXP r_direction )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;

  Rcpp::NumericMatrix direction( r_direction );

  if ( (direction.nrow() != Dimension) || (direction.ncol() != Dimension) )
    {
    Rcpp::Rcout << "direction array must be of size ImageDimension * ImageDimension" << std::endl;
    Rcpp::Rcout << "direction size = " << direction.nrow() << " x " << direction.ncol() << std::endl;
    return Rcpp::wrap(1);
    }

  typename ImageType::DirectionType itkdirection;
  if( image.IsNotNull() )
    {

    unsigned int idx = 0;
    for( unsigned int i = 0 ; i < Dimension ; ++i )
      {
      for ( unsigned int j=0; j<Dimension; j++ )
        {
        itkdirection(i,j) = direction[idx++];
        }
      }
    image->SetDirection( itkdirection );

    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
  return Rcpp::wrap(0);
}

RcppExport SEXP antsImage_SetDirection( SEXP r_antsimage, SEXP r_direction )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_SetDirection< PixelType , ImageDimension >( *antsimage_xptr, r_direction ) ;
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

template< class PixelType , unsigned int Dimension >
SEXP antsImage_TransformIndexToPhysicalPoint( typename itk::Image< PixelType , Dimension >::Pointer image, SEXP r_index )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef typename ImageType::PointType PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  typedef typename itk::ContinuousIndex<CoordRepType, Dimension> IndexType;

  Rcpp::NumericMatrix indices( r_index );
  if ( indices.ncol() != Dimension )
    {
    Rcpp::Rcout << "index matrix must be of size N * ImageDimension" << std::endl;
    return Rcpp::wrap(1);
    }
  if ( ! image.IsNotNull() )
    {
    Rcpp::Rcout << "must pass a valid antsImage";
    return Rcpp::wrap(1);
    }

  unsigned long N = indices.nrow();
  Rcpp::NumericMatrix points( N, Dimension ) ;

  IndexType itkindex;
  PointType itkpoint;

  for( unsigned int j = 0; j < N; j++)
    {

    for( unsigned int i = 0; i < Dimension; i++ )
      {
      itkindex[i] = static_cast<CoordRepType>( indices(j,i) - 1.0 );
      }

    image->TransformContinuousIndexToPhysicalPoint( itkindex, itkpoint );

    for ( int i = 0; i < Dimension; i++ )
      {
      points(j,i) = itkpoint[i];
      }
    }

  return points;
}

RcppExport SEXP antsImage_TransformIndexToPhysicalPoint( SEXP r_antsimage, SEXP r_index )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformIndexToPhysicalPoint< PixelType , ImageDimension >( *antsimage_xptr, r_index ) ;
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

template< class PixelType , unsigned int Dimension >
SEXP antsImage_TransformPhysicalPointToIndex( typename itk::Image< PixelType , Dimension >::Pointer image, SEXP r_point )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef typename ImageType::PointType PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  typedef typename itk::ContinuousIndex<CoordRepType, Dimension> IndexType;

 Rcpp::NumericMatrix points( r_point );
  if ( points.ncol() != Dimension )
    {
    Rcpp::Rcout << "point matrix must be of size N * ImageDimension" << std::endl;
    return Rcpp::wrap(1);
    }
  if ( ! image.IsNotNull() )
    {
    Rcpp::Rcout << "must pass a valid antsImage";
    return Rcpp::wrap(1);
    }

  unsigned long N = points.nrow();
  Rcpp::NumericMatrix indices( N, Dimension ) ;

  IndexType itkindex;
  PointType itkpoint;

  for( unsigned int j = 0; j < N; j++)
    {

    for( unsigned int i = 0; i < Dimension; i++ )
      {
      itkpoint[i] = static_cast<CoordRepType>( points(j,i) );
      }

    image->TransformPhysicalPointToContinuousIndex( itkpoint, itkindex );

    for ( int i = 0; i < Dimension; i++ )
      {
      indices(j,i) = itkindex[i] + 1.0;
      }
    }

  return indices;
}

RcppExport SEXP antsImage_TransformPhysicalPointToIndex( SEXP r_antsimage, SEXP r_point )
try
{
  if( r_antsimage == NULL )
    {
      Rcpp::Rcout << "Unspecified Argument" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
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
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_TransformPhysicalPointToIndex< PixelType , ImageDimension >( *antsimage_xptr, r_point ) ;
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

template< class PixelType , unsigned int Dimension >
bool antsImage_SetRegion( typename itk::Image< PixelType , Dimension >::Pointer& image , SEXP r_mask , SEXP r_antsregion , SEXP r_value )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      typename ImageType::RegionType region ;
      Rcpp::S4 antsregion( r_antsregion ) ;
      Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) ) ;
      Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) ) ;
      if( indexvector.size() == 0 && sizevector.size() == 0 )
	{
	  region = image->GetLargestPossibleRegion() ;
	}
      else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	{
	  Rcpp::Rcout << "antsRegion provided has dimensions incompatible with the image" << std::endl ;
	  return 1 ;
	}
      else
	{
	  typename ImageType::IndexType index ;
	  typename ImageType::SizeType size ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	    }
	  region.SetSize( size ) ;
	  region.SetIndex( index ) ;
	}
      itk::ImageRegionIterator< ImageType > image_iter( image , region ) ;
      Rcpp::LogicalVector mask( r_mask ) ;
      int numberofpixelspertime = region.GetSize(0) ;
      for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
	{
	  numberofpixelspertime *= region.GetSize(i) ;
	}
      if( mask.size() != 0 && mask.size() != numberofpixelspertime && mask.size() != (int)region.GetNumberOfPixels() )
	{
	  Rcpp::Rcout << "Length of 'mask' vector must be either 0, num-of-voxels-per-time-in-region, num-of-voxels-in-region" << std::endl ;
	  return 1 ;
	}
      Rcpp::NumericVector value( r_value ) ;
      if( value.size() != 1 && value.size() != (int)region.GetNumberOfPixels() && value.size() != std::count( mask.begin() , mask.end() , TRUE ) )
	{
	  Rcpp::Rcout << "Length of 'value' vector must be either 1, num-of-voxels-in-region or num-of-TRUEs-in-mask" << std::endl ;
	  return 1 ;
	}

      if( mask.size() == 0 )
	{
	  if( value.size() == 1 )
	    {
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
		{
		  image_iter.Set( value[0] ) ;
		}
	    }
	  else
	    {
	      unsigned int value_ind = 0 ;
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
		{
		  image_iter.Set( value[value_ind++] ) ;
		}
	    }
	}
      else
	{
	  Rcpp::LogicalVector::iterator mask_iter = mask.begin() ;
	  if( value.size() == 1 )
	    {
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		{
		  if( mask_iter == mask.end() )
		    mask_iter = mask.begin() ;
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[0] ) ;
		}
	    }
	  else if( value.size() == (int)region.GetNumberOfPixels() )
	    {
	      image_iter.GoToBegin() ;
	      for( unsigned long value_ind = 0 ; (int)value_ind < value.size() ; ++image_iter , ++mask_iter , ++value_ind )
		{
		  if( mask_iter == mask.end() )
		    mask_iter = mask.begin() ;
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[value_ind] ) ;
		}
	    }
	  else
	    {
	      unsigned long value_ind = 0 ;
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		{
		  if( mask_iter == mask.end() )
		    {
		      mask_iter = mask.begin() ;
		      value_ind = 0 ;
		    }
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[value_ind++] ) ;
		}
	    }
	}
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return 1 ;
    }
  return 0 ;
}

RcppExport SEXP antsImage_SetRegion( SEXP r_antsimage , SEXP r_mask , SEXP r_antsregion , SEXP r_value )
try
{
  if( r_antsimage == NULL || r_mask == NULL || r_antsregion == NULL || r_value == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
    }
  return r_antsimage ;
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }


template< class ImageType >
typename ImageType::Pointer antsImage_asantsImage( Rcpp::NumericVector& vector , Rcpp::NumericVector& spacing , Rcpp::NumericVector& origin )
{
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  Rcpp::IntegerVector vector_dim = vector.attr( "dim" ) ;
  typename ImageType::IndexType image_index ;
  typename ImageType::SizeType image_size ;
  typename ImageType::SpacingType image_spacing ;
  typename ImageType::PointType image_origin ;
  for( unsigned int i = 0 ; i < ImageType::ImageDimension ; ++i )
    {
      image_index[i] = 0 ;
      image_size[i] = vector_dim[i] ;
      image_spacing[i] = spacing[i] ;
      image_origin[i] = origin[i] ;
    }
  typename ImageType::RegionType image_region ;
  image_region.SetIndex( image_index ) ;
  image_region.SetSize( image_size ) ;

  ImagePointerType image = ImageType::New() ;
  image->SetRegions( image_region ) ;
  image->SetSpacing( image_spacing ) ;
  image->SetOrigin( image_origin ) ;
  image->Allocate() ;

  itk::ImageRegionIterator< ImageType > image_iter( image , image_region ) ;
  unsigned int vector_ind = 0 ;
  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
    {
      image_iter.Set( static_cast< typename ImageType::PixelType >( vector[vector_ind++] ) ) ;
    }

  return image ;
}

RcppExport SEXP antsImage_asantsImage( SEXP r_vector , SEXP r_pixeltype , SEXP r_spacing , SEXP r_origin )
try
{
  if( r_vector == NULL || r_pixeltype == NULL || r_spacing == NULL || r_origin == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  Rcpp::NumericVector vector( r_vector ) ;
  Rcpp::IntegerVector vector_dim = vector.attr( "dim" ) ;
  Rcpp::NumericVector spacing( r_spacing ) ;
  Rcpp::NumericVector origin( r_origin ) ;
  if( spacing.size() != vector_dim.size() || origin.size() != vector_dim.size() )
    {
      Rcpp::Rcout << "spacing or origin is incompatible with the vector" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  if( pixeltype == "double" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }


template< class PixelType , unsigned int Dimension >
SEXP antsImage_RelationalOperators( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      // PermuteAxesFilterPointerType permuteaxesfilter = PermuteAxesFilterType::New() ;
      // permuteaxesfilter->SetInput( origimage ) ;
      // PermuteAxesFilterOrderType permuteaxesfilterorder ;
      // permuteaxesfilterorder[0] = 1 ;
      // permuteaxesfilterorder[1] = 0 ;
      // for( unsigned int i = 2 ; i < Dimension ; ++i )
      // 	{
      // 	  permuteaxesfilterorder[i] = i ;
      // 	}
      // permuteaxesfilter->SetOrder( permuteaxesfilterorder ) ;
      // ImagePointerType image = permuteaxesfilter->GetOutput() ;
      // permuteaxesfilter->Update() ;

      typename ImageType::RegionType region ;
      Rcpp::S4 antsregion( r_antsregion ) ;
      Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) ) ;
      Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) ) ;
      if( indexvector.size() == 0 && sizevector.size() == 0 )
	{
	  region = image->GetLargestPossibleRegion() ;
	}
      else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	{
	  Rcpp::Rcout << "antsRegion provided has dimensions incompatible with the image" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
      else
	{
	  typename ImageType::IndexType index ;
	  typename ImageType::SizeType size ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	    }
	  region.SetSize( size ) ;
	  region.SetIndex( index ) ;
	}
      itk::ImageRegionConstIterator< ImageType > image_iter( image , region ) ;
      Rcpp::LogicalVector vector_r( (int)region.GetNumberOfPixels() ) ;
      unsigned int ind = 0 ;
      double value = Rcpp::as< double >( r_value ) ;
      if( Rcpp::as< std::string >( r_operator ) == "==" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() == value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "!=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() != value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "<=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() <= value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == ">=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() >= value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "<" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() < value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == ">" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() > value ) ;
	    }
	}
      else
	{
	  Rcpp::Rcout << "unsupported operator" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
      // Rcpp::IntegerVector dims( Dimension ) ;
      // for( unsigned int i = 0 ; i < Dimension ; ++i )
      // 	{
      // 	  dims[i] = region.GetSize( i )  ;
      // 	}
      // vector_r.attr( "dim" ) = dims ;
      return vector_r ;
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_RelationalOperators( SEXP r_antsimage , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
try
{
  if( r_antsimage == NULL || r_value == NULL || r_antsregion == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }
