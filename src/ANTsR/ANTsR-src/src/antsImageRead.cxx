
#include<typeinfo>
#include<vector>
#include<string>
#include<Rcpp.h>

#include "itkImageFileReader.h"
#include "itkImage.h"

namespace ants
{

  template< class PixelType , int ImageDimension >
  typename itk::Image< PixelType , ImageDimension >::Pointer antsImageRead( std::string filename // image to read
									    ) 
  {
    typedef itk::Image< PixelType , ImageDimension > ImageType ;
    typedef itk::ImageFileReader< ImageType >    ImageReaderType ;
    typename ImageReaderType::Pointer image_reader = ImageReaderType::New() ;
    image_reader->SetFileName( filename.c_str() ) ;
    image_reader->Update();
    return image_reader->GetOutput() ;
  }

  template< class ImagePointerType >
  void printImageInfo( ImagePointerType image , std::string PixelType , std::ostream& os )
  {
    os << "PixelType: " << PixelType << "\n" ;
    os << "Dimension: " << image->ImageDimension << "\n" ;
    os << "Extent:    [ " ;
    for( int i = 0 ; i < image->ImageDimension - 1 ; ++i )
      {
	os << image->GetLargestPossibleRegion().GetSize()[i] << " , " ;
      }
    os << image->GetLargestPossibleRegion().GetSize()[ image->ImageDimension - 1 ] << " ]\n" ;
    os << "Origin:    [ " ;
    for( int i = 0 ; i < image->ImageDimension - 1 ; ++i )
      {
	os << image->GetOrigin()[0] << " , " ;
      }
    os << image->GetOrigin()[ image->ImageDimension - 1 ] << " ]\n" ;
    os << "Spacing:   [ " ;
    for( int i = 0 ; i < image->ImageDimension - 1 ; ++i )
      {
	os << image->GetSpacing()[0] << " , " ;
      }
    os << image->GetSpacing()[ image->ImageDimension - 1 ] << " ]" << std::endl ;
    return ;    
  }
  
} // namespace ants


RcppExport SEXP antsImageRead( SEXP r_filename , SEXP r_pixeltype , SEXP r_dimension )
try
{
  // check and set the parameters
  if( r_filename == NULL || r_pixeltype == NULL || r_dimension == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
  std::string filename = Rcpp::as< std::string >( r_filename ) ;
  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  unsigned int dimension = Rcpp::as< unsigned int >( r_dimension ) ;

  // read the image using the above info
  if( dimension == 4 && pixeltype == "double" )
    {
      const int ImageDimension = 4 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
      image_r.slot( "dimension" ) = 4 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "double" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 3 && pixeltype == "double" )
    {
      const int ImageDimension = 3 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
      image_r.slot( "dimension" ) = 3 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "double" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 2 && pixeltype == "double" )
    {
      const int ImageDimension = 2 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "double" ) ;
      image_r.slot( "dimension" ) = 2 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "double" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension  == 4 && pixeltype == "float" )
    {
      const int ImageDimension = 4 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
      image_r.slot( "dimension" ) = 4 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "float" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 3 && pixeltype == "float" )
    {
      const int ImageDimension = 3 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
      image_r.slot( "dimension" ) = 3 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "float" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 2 && pixeltype == "float" )
    {
      const int ImageDimension = 2 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "float" ) ;
      image_r.slot( "dimension" ) = 2 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "float" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 4 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
      image_r.slot( "dimension" ) = 4 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "unsigned int" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 3 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
      image_r.slot( "dimension" ) = 3 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "unsigned int" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else if( dimension == 2 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
      image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
      image_r.slot( "dimension" ) = 2 ;
      image_r.slot( "pointer" ) = xptr ;
      ants::printImageInfo( ( *ptr_ptr_image ) , "unsigned int" , Rcpp::Rcout ) ;
      return image_r ;
    }
  else
    {
      Rcpp::Rcout << "Unsupported Dimension or PixelType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
