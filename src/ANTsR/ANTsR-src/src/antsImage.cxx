
#include<algorithm>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "itkImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

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
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '4'" << std::endl ;
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
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '3'" << std::endl ;
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
	  Rcpp::Rcout << "Empty image created. PixelType: 'unsigned char' | Dimension: '2'" << std::endl ;
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


template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetVector( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_mask , SEXP r_antsregion )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;

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
	  return Rcpp::wrap( 1 ) ;
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
	      return Rcpp::wrap( 1 ) ;
	    }
	}
      itk::ImageRegionConstIterator< ImageType > image_iter( image , region ) ;
      Rcpp::NumericVector vector_r( region.GetNumberOfPixels() ) ;
      Rcpp::LogicalVector mask( r_mask ) ;
      if( mask.size() == 0 )
	{
	  unsigned int ind = 0 ;
	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = image_iter.Get() ;
	    }
	}
      else
	{
	  int numberofpixelsperslice = region.GetSize(0) ;
	  for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
	    {
	      numberofpixelsperslice *= region.GetSize(i) ;
	    }
	  if( mask.size() != numberofpixelsperslice && mask.size() != (int)region.GetNumberOfPixels() )
	    {
	      Rcpp::Rcout << "Length of mask vector does not match image-region dimensions" << std::endl ;
	      return Rcpp::wrap( 1 ) ;
	    }
	  Rcpp::LogicalVector::iterator mask_iter = mask.begin() ;
	  unsigned int ind = 0 ;
	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
	    {
	      if( mask_iter == mask.end() )
		mask_iter = mask.begin() ;
	      if( *mask_iter == 1 )
		vector_r[ind++] = image_iter.Get() ;
	      else
		vector_r[ind++] = NA_REAL ;
	    }
	}
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
      Rcpp::Rcout << "Empty Image" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}

RcppExport SEXP antsImage_GetVector( SEXP r_antsimage , SEXP r_mask , SEXP r_antsregion )
try
{
  if( r_antsimage == NULL || r_mask == NULL || r_antsregion == NULL )
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
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
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
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
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
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
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
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_GetVector< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion ) ;
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
SEXP antsImage_GetPixels( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_coordinates )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;

  if( image.IsNotNull() )
    {
      Rcpp::List list_coordinates( r_coordinates ) ;
      std::vector< std::vector< double > > coordinates ;
      if( list_coordinates.size() != Dimension )
	{
	  Rcpp::Rcout << "indices do not match the image in dimensions" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
      for( int i = 0 ; i < list_coordinates.size() ; ++i )
	{
	  coordinates.push_back( Rcpp::as< std::vector< double > >( list_coordinates[i] ) ) ;
	}

      unsigned int vector_r_size = 1 ;
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
	  vector_r_size *= coordinates[i].size() ;
	}
      Rcpp::NumericVector vector_r( vector_r_size ) ;
      std::vector< unsigned int > ind( Dimension ) ;
      typename ImageType::IndexType index ;
      for( unsigned int i = 0 ; i < vector_r_size ; ++i )
	{
	  for( unsigned int j = 0 ; j < Dimension ; ++j )
	    {
	      index[j] = coordinates[ j ][ ind[j] ] ;
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
      return vector_r ;
    }
  else
    {
      Rcpp::Rcout << "Empty Image" << std::endl ;
      return Rcpp::wrap( 1 ) ;
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
bool antsImage_SetRegion( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_mask , SEXP r_antsregion , SEXP r_value )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;

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
      Rcpp::NumericVector value( r_value ) ;
      if( value.size() == 1 || value.size() == (int)region.GetNumberOfPixels() )
	{
	  Rcpp::LogicalVector mask( r_mask ) ;
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
	      return 0 ;
	    }
	  else
	    {
	      int numberofpixelsperslice = region.GetSize(0) ;
	      for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
		{
		  numberofpixelsperslice *= region.GetSize(i) ;
		}
	      if( mask.size() != numberofpixelsperslice && mask.size() != (int)region.GetNumberOfPixels() )
		{
		  Rcpp::Rcout << "Length of mask vector does not match image-region dimensions" << std::endl ;
		  return 1 ;
		}
	      Rcpp::LogicalVector::iterator mask_iter = mask.begin() ;
	      if( value.size() == 1 )
		{
		  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		    {
		      if( mask_iter == mask.end() )
			mask_iter = mask.begin() ;
		      if( *mask_iter == 1 )
			image_iter.Set( value[0] ) ;
		    }
		}
	      else
		{
		  unsigned int value_ind = 0 ;
		  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		    {
		      if( mask_iter == mask.end() )
			mask_iter = mask.begin() ;
		      if( *mask_iter == 1 )
			image_iter.Set( value[value_ind++] ) ;
		    }
		}
	      return 0 ;
	    }
	}
      else
	{
	  Rcpp::Rcout << "'value' has length incompatible with image-region" << std::endl ;
	  return 1 ;
	}
    }
  else
    {
      Rcpp::Rcout << "Empty Image" << std::endl ;
      return 1 ;
    }
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
      return Rcpp::wrap( 1 ) ;
    }

  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  Rcpp::NumericVector vector( r_vector ) ;
  Rcpp::IntegerVector vector_dim = vector.attr( "dim" ) ;
  Rcpp::NumericVector spacing( r_spacing ) ;
  Rcpp::NumericVector origin( r_origin ) ;
  if( spacing.size() != vector_dim.size() || origin.size() != vector_dim.size() )
    {
      Rcpp::Rcout << "spacing or origin is incompatible with the vector" << std::endl ;
      return Rcpp::wrap( 1 ) ;
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
	  return Rcpp::wrap( 1 ) ;
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
	  return Rcpp::wrap( 1 ) ;
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
	  return Rcpp::wrap( 1 ) ;
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
SEXP antsImage_RelationalOperators( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;

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
	  return Rcpp::wrap( 1 ) ;
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
	  return Rcpp::wrap( 1 ) ;
	}
      Rcpp::IntegerVector dims( Dimension ) ;
      for( unsigned int i = 0 ; i < Dimension ; ++i )
	{
	  dims[i] = region.GetSize( i )  ;
	}
      vector_r.attr( "dim" ) = dims ;
      return vector_r ;
    }
  else
    {
      Rcpp::Rcout << "Empty Image" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}

RcppExport SEXP antsImage_RelationalOperators( SEXP r_antsimage , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
try
{
  if( r_antsimage == NULL || r_value == NULL || r_antsregion == NULL )
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
