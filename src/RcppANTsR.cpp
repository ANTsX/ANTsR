
#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "RcppANTsR.h"
#include "vnl/vnl_vector_ref.h"

namespace Rcpp {
  //namespace traits {

/*
template<typename T>
SEXP wrap( const vnl_vector<T> &vector)
{
  Rcpp::NumericVector rcppVector(vector.size());
  for (unsigned int i=0; i<vector.size(); i++)
    {
    rcppVector[i] = vector[i];
    }
  return rcppVector;
}
*/

/*
namespace traits {

template <class ImageType>
SEXP wrap( const itk::SmartPointer<ImageType> &image )
{
  typedef ImageType::Pointer                       ImagePointerType;
  typedef ImageType::PixelType                     PixelType;
  typedef itk::NumericTraits<PixelType>::ValueType ValueType;

  ImagePointerType itkImage = dynamic_cast< image
  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true );
  unsigned int imageDim = ImageType::ImageDimension;
  unsigned int pixelDim = image->GetNumberOfDimensionsPerPixel();

  // Deal with pixeltype
  Rcpp::S4 antsImage( std::string( "antsImage" ) );

  if ( sizeof(ValueType) == sizeof(double) )
    {
    antsImage.slot("pixeltype") = "double";
    }
  else if ( sizeof(ValueType) == sizeof(float) )
    {
    antsImage.slot("pixeltype") = "float";
    }
  else if ( sizeof(ValueType) == sizeof(unsigned int) )
    {
    antsImage.slot("pixeltype") = "unsigned int";
    }
  else if ( sizeof(ValueType) == sizeof(unsigned char) )
    {
    antsImage.slot("pixeltype") = "unsigned char";
    }

  antsImage.slot( "dimension" ) = imageDim;
  antsImage.slot( "components" ) = pixelDim;
  antsImage.slot( "pointer") = xptr;

  return(wrap(antsImage));
}

}

*/

template <>
SEXP wrap( const itk::Image<double,2>::Pointer &image )
{
  typedef itk::Image<double,2>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<double,3>::Pointer &image )
{
  typedef itk::Image<double,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<double,4>::Pointer &image )
{
  typedef itk::Image<double,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<double,2>::Pointer &image )
{
  typedef itk::VectorImage<double,2>  ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<double,3>::Pointer &image )
{
  typedef itk::VectorImage<double,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<double,4>::Pointer &image )
{
  typedef itk::VectorImage<double,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 1;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<float,2>::Pointer &image )
{
  typedef itk::Image<float,2>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<float,3>::Pointer &image )
{
  typedef itk::Image<float,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<float,4>::Pointer &image )
{
  typedef itk::Image<float,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<float,2>::Pointer &image )
{
  typedef itk::VectorImage<float,2>  ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<float,3>::Pointer &image )
{
  typedef itk::VectorImage<float,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<float,4>::Pointer &image )
{
  typedef itk::VectorImage<float,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 1;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned int,2>::Pointer &image )
{
  typedef itk::Image<unsigned int,2>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned int,3>::Pointer &image )
{
  typedef itk::Image<unsigned int,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned int,4>::Pointer &image )
{
  typedef itk::Image<unsigned int,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned int,2>::Pointer &image )
{
  typedef itk::VectorImage<unsigned int,2>  ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned int,3>::Pointer &image )
{
  typedef itk::VectorImage<unsigned int,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned int,4>::Pointer &image )
{
  typedef itk::VectorImage<unsigned int,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 1;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned char,2>::Pointer &image )
{
  typedef itk::Image<unsigned char,2>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned char,3>::Pointer &image )
{
  typedef itk::Image<unsigned char,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::Image<unsigned char,4>::Pointer &image )
{
  typedef itk::Image<unsigned char,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = 1;
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned char,2>::Pointer &image )
{
  typedef itk::VectorImage<unsigned char,2>  ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 2;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned char,3>::Pointer &image )
{
  typedef itk::VectorImage<unsigned char,3>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 3;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <>
SEXP wrap( const itk::VectorImage<unsigned char,4>::Pointer &image )
{
  typedef itk::VectorImage<unsigned char,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 1;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}


template <>
itk::ImageBase<2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::ImageBase<Dim>           ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageBase<3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::ImageBase<Dim>           ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageBase<4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::ImageBase<Dim>           ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<double,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<double,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<double,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<float,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<float,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<float,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned int,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned int,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned int,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned char,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned char,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::Image<unsigned char,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}


template <>
itk::VectorImage<double,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::VectorImage<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<double,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::VectorImage<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<double,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::VectorImage<double,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "double") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<float,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::VectorImage<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<float,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::VectorImage<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<float,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::VectorImage<float,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned int,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::VectorImage<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned int,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::VectorImage<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned int,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::VectorImage<unsigned int,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned char,2>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 2;
  typedef itk::VectorImage<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned char,3>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 3;
  typedef itk::VectorImage<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

template <>
itk::VectorImage<unsigned char,4>::Pointer as( SEXP itkImageR )
{
  const unsigned int Dim = 4;
  typedef itk::VectorImage<unsigned char,Dim>        ImageType;
  typedef ImageType::Pointer   ImagePointerType;
  Rcpp::S4 itkImageObject( itkImageR );

  if (!itkImageObject.is( "antsImage") ||
      (Rcpp::as<std::string>(itkImageObject.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageObject.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageObject.slot("components")) < 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<ImagePointerType> xptr( static_cast<SEXP>( itkImageObject.slot("pointer") ));
  return *xptr;
}

// antsImageIterator
 /*
template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<PIXELTYPE,DIMENSION> > & iterator )
{
  typedef itk::Image<PIXELTYPE,DIMENSION>              ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "PIXELTYPE";
  itkImageIterator.slot( "dimension" ) = DIMENSION;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}
*/

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > & iterator )
{
  typedef itk::Image<double,2>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "double";
  itkImageIterator.slot( "dimension" ) = 2;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > & iterator )
{
  typedef itk::Image<double,3>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "double";
  itkImageIterator.slot( "dimension" ) = 3;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > & iterator )
{
  typedef itk::Image<double,4>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "double";
  itkImageIterator.slot( "dimension" ) = 4;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > & iterator )
{
  typedef itk::Image<float,2>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "float";
  itkImageIterator.slot( "dimension" ) = 2;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > & iterator )
{
  typedef itk::Image<float,3>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "float";
  itkImageIterator.slot( "dimension" ) = 3;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > & iterator )
{
  typedef itk::Image<float,4>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "float";
  itkImageIterator.slot( "dimension" ) = 4;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > & iterator )
{
  typedef itk::Image<unsigned int,2>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsigned int";
  itkImageIterator.slot( "dimension" ) = 2;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > & iterator )
{
  typedef itk::Image<unsigned int,3>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsinged int";
  itkImageIterator.slot( "dimension" ) = 3;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > & iterator )
{
  typedef itk::Image<unsigned int,4>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsigned int";
  itkImageIterator.slot( "dimension" ) = 4;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > & iterator )
{
  typedef itk::Image<unsigned char,2>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsigned char";
  itkImageIterator.slot( "dimension" ) = 2;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > & iterator )
{
  typedef itk::Image<unsigned char,3>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsinged char";
  itkImageIterator.slot( "dimension" ) = 3;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > & iterator )
{
  typedef itk::Image<unsigned char,4>                          ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

  IteratorType* rawPointer = new IteratorType( iterator );
  Rcpp::XPtr< IteratorType > xptr( rawPointer, true ) ;

  Rcpp::S4 itkImageIterator( std::string( "antsImageIterator" ) );
  itkImageIterator.slot( "pixeltype" ) = "unsigned char";
  itkImageIterator.slot( "dimension" ) = 4;
  itkImageIterator.slot( "components" ) = 1;
  itkImageIterator.slot( "pointer" ) = xptr;

  return(wrap(itkImageIterator));
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "float") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned int") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned char,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned char,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned char,Dim>                        ImageType;
  typedef ImageType::Pointer                           ImagePointerType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  typedef IteratorType*                                IteratorPointerType;

  Rcpp::S4 itkImageIterator( itkImageIteratorR );

  if (!itkImageIterator.is( "antsImageIterator") ||
      (Rcpp::as<std::string>(itkImageIterator.slot("pixeltype")) != "unsigned char") ||
      (Rcpp::as<int>(itkImageIterator.slot("dimension")) != Dim) ||
      (Rcpp::as<int>(itkImageIterator.slot("components")) != 1) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }

  XPtr<IteratorType> xptr( static_cast<SEXP>( itkImageIterator.slot("pointer") ));
  return *xptr;
}

template<>
SEXP wrap( const itk::AffineTransform<double,2>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<double,2>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "double";
  antsTransform.slot( "dimension" ) = 2;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template<>
SEXP wrap( const itk::AffineTransform<double,3>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<double,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "double";
  antsTransform.slot( "dimension" ) = 3;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template<>
SEXP wrap( const itk::AffineTransform<double,4>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<double,4>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "double";
  antsTransform.slot( "dimension" ) = 4;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template<>
SEXP wrap( const itk::AffineTransform<float,2>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<float,2>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "float";
  antsTransform.slot( "dimension" ) = 2;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template<>
SEXP wrap( const itk::AffineTransform<float,3>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<float,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "float";
  antsTransform.slot( "dimension" ) = 3;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template<>
SEXP wrap( const itk::AffineTransform<float,4>::Pointer & itkTransform )
{
  typedef itk::AffineTransform<float,4>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsTransform( std::string( "antsTransform" ) );
  antsTransform.slot( "precision" ) = "float";
  antsTransform.slot( "dimension" ) = 4;
  antsTransform.slot( "type" ) = "AffineTransform";
  antsTransform.slot( "pointer") = xptr;

  return( wrap(antsTransform) );
}

template <>
itk::AffineTransform<double,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::AffineTransform<double,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::AffineTransform<double,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::AffineTransform<double,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::AffineTransform<double,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::AffineTransform<double,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::AffineTransform<float,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::AffineTransform<float,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::AffineTransform<float,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::AffineTransform<float,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::AffineTransform<float,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::AffineTransform<float,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsTransform( r_transform );

  if (!antsTransform.is( "antsTransform") ||
      (Rcpp::as<std::string>(antsTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsTransform.slot("dimension")) != Dim) ||
      (Rcpp::as<std::string>(antsTransform.slot("type")) < "AffineTransform") )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsTransform.slot("pointer") ));
  return *xptr;
}


}
