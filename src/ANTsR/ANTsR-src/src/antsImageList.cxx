
#include<vector>
#include<string>
#include<Rcpp.h>
#include "itkImage.h"

RcppExport SEXP antsImageList( SEXP r_pixeltype , SEXP r_dimension )
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  vectorofimages_r.slot( "dimension" ) = 4 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'double' | Dimension: '4'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  vectorofimages_r.slot( "dimension" ) = 3 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'double' | Dimension: '3'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  vectorofimages_r.slot( "dimension" ) = 2 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'double' | Dimension: '2'" << std::endl ;
	  return vectorofimages_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  vectorofimages_r.slot( "dimension" ) = 4 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'float' | Dimension: '4'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  vectorofimages_r.slot( "dimension" ) = 3 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'float' | Dimension: '3'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  vectorofimages_r.slot( "dimension" ) = 2 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'float' | Dimension: '2'" << std::endl ;
	  return vectorofimages_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  vectorofimages_r.slot( "dimension" ) = 4 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned int' | Dimension: '4'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  vectorofimages_r.slot( "dimension" ) = 3 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned int' | Dimension: '3'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  vectorofimages_r.slot( "dimension" ) = 2 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned int' | Dimension: '2'" << std::endl ;
	  return vectorofimages_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  vectorofimages_r.slot( "dimension" ) = 4 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned char' | Dimension: '4'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  vectorofimages_r.slot( "dimension" ) = 3 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned char' | Dimension: '3'" << std::endl ;
	  return vectorofimages_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  VectorOfImagePointerType* ptr_ptr_vectorofimages = new VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > xptr( ptr_ptr_vectorofimages , true ) ;
	  Rcpp::S4 vectorofimages_r( std::string( "antsImageList" ) ) ;
	  vectorofimages_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  vectorofimages_r.slot( "dimension" ) = 2 ;
	  vectorofimages_r.slot( "pointer" ) = xptr ;
	  Rcpp::Rcout << "Empty list of images created. PixelType: 'unsigned char' | Dimension: '2'" << std::endl ;
	  return vectorofimages_r ;
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

RcppExport SEXP antsImageList_asList( SEXP r_antsimagelist )
try
{
  if( r_antsimagelist == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimagelist( r_antsimagelist ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimagelist.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimagelist.slot( "dimension" ) );

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	      image_r.slot( "dimension" ) = 4 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	      image_r.slot( "dimension" ) = 3 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	      image_r.slot( "dimension" ) = 2 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	      image_r.slot( "dimension" ) = 4 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	      image_r.slot( "dimension" ) = 3 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	      image_r.slot( "dimension" ) = 2 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	      image_r.slot( "dimension" ) = 4 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	      image_r.slot( "dimension" ) = 3 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	      image_r.slot( "dimension" ) = 2 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
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
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	      image_r.slot( "dimension" ) = 4 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	      image_r.slot( "dimension" ) = 3 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
	}
      if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
	  typedef std::vector< ImagePointerType > VectorOfImagePointerType ;
	  Rcpp::XPtr< VectorOfImagePointerType > antsimagelist_xptr( static_cast< SEXP >( antsimagelist.slot( "pointer" ) ) ) ;
	  Rcpp::List list_r ;
	  for( VectorOfImagePointerType::size_type i = 0 ; i < antsimagelist_xptr->size() ; ++i )
	    {
	      ImagePointerType* ptr_ptr_image = new ImagePointerType( (*antsimagelist_xptr)[i] ) ;
	      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	      image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	      image_r.slot( "dimension" ) = 2 ;
	      image_r.slot( "pointer" ) = xptr ;
	      list_r.push_back( image_r ) ;
	    }
	  return list_r ;
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
