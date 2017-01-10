#ifndef __RCPPANTSR_IMAGE_HPP
#define __RCPPANTSR_IMAGE_HPP

#include "itkMacro.h"
#include "itkImage.h"
#include <Rcpp.h>

namespace Rcpp {

/* Example code for implementing 'wrap' for an itk image class
template <> inline
SEXP wrap( const itk::IMAGETYPE<PIXELTYPE,DIMENSION>::Pointer &image )
{
  typedef itk::IMAGETYPE<PIXELTYPE,DIMENSION> ImageType;
  typedef ImageType::Pointer                  ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "PIXELTYPE";
  itkImage.slot( "dimension" ) = DIMENSION;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}
*/

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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


template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

}

#endif
