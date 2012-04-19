
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
      Rcpp::Rcout << "Done reading image. PixelType: 'double' | Dimension: '4'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 3 && pixeltype == "double" )
    {
      const int ImageDimension = 3 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'double' | Dimension: '3'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 2 && pixeltype == "double" )
    {
      const int ImageDimension = 2 ;
      typedef double PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'double' | Dimension: '2'" << std::endl ;
      return xptr ;
    }
  else if( dimension  == 4 && pixeltype == "float" )
    {
      const int ImageDimension = 4 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'float' | Dimension: '4'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 3 && pixeltype == "float" )
    {
      const int ImageDimension = 3 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'float' | Dimension: '3'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 2 && pixeltype == "float" )
    {
      const int ImageDimension = 2 ;
      typedef float PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'float' | Dimension: '2'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 4 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 4 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'unsigned int' | Dimension: '4'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 3 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 3 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'unsigned int' | Dimension: '3'" << std::endl ;
      return xptr ;
    }
  else if( dimension == 2 && pixeltype == "unsigned int" )
    {
      const int ImageDimension = 2 ;
      typedef unsigned int PixelType ;
      typedef itk::Image< PixelType , ImageDimension >::Pointer ImagePointerType ;
      ImagePointerType* ptr_ptr_image = new ImagePointerType( ants::antsImageRead< PixelType , ImageDimension >( filename ) ) ;
      Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
      Rcpp::Rcout << "Done reading image. PixelType: 'unsigned int' | Dimension: '2'" << std::endl ;
      return xptr ;
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
