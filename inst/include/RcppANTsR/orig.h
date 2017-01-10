#ifndef __RCPPANTSR_H
#define __RCPPANTSR_H

#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/vnl_vector_ref.h"
#include "itkTransform.h"
#include "itkAffineTransform.h"

#include <RcppCommon.h>

// From advice @ http://dirk.eddelbuettel.com/code/rcpp/Rcpp-extending.pdf

inline SEXP exception_to_r_condition( const itk::ExceptionObject & ex )
{
  //std::string ex_class = demangle( typeid(ex).name() ) ;
  std::string ex_msg   = ex.what() ;
  std::string ex_class = "ITK";

  Rcpp::Shield<SEXP> cppstack( rcpp_get_stack_trace() );
  Rcpp::Shield<SEXP> call( get_last_call() );
  Rcpp::Shield<SEXP> classes( get_exception_classes(ex_class) );
  Rcpp::Shield<SEXP> condition( make_condition( ex_msg, call, cppstack, classes) );
  rcpp_set_stack_trace( R_NilValue ) ;
  return condition ;
}

inline void forward_exception_to_r( const itk::ExceptionObject & ex )
{
  SEXP stop_sym  = Rf_install( "stop" ) ;
  Rcpp::Shield<SEXP> condition( exception_to_r_condition(ex) );
  Rcpp::Shield<SEXP> expr( Rf_lang2( stop_sym , condition ) ) ;
  Rf_eval( expr, R_GlobalEnv ) ;
}

namespace Rcpp {

// These are used to get header info only for both itk::Image and itk::VectorImage
template <> inline itk::ImageBase<2>::Pointer as( SEXP itkImageR );
template <> inline itk::ImageBase<3>::Pointer as( SEXP itkImageR );
template <> inline itk::ImageBase<4>::Pointer as( SEXP itkImageR );

// itk::Image to antsImage
#include <RcppANTsR/RcppANTsR_ImageBaseDef.h>
#include <RcppANTsR/RcppANTsR_ImageDef.h>
#include <RcppANTsR/RcppANTsR_VectorImageDef.h>
#include <RcppANTsR/RcppANTsR_ImageIteratorDef.h>
#include <RcppANTsR/RcppANTsR_TransformDef.h>
/*
template <> inline SEXP wrap( const itk::Image<double,2>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<double,3>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<double,4>::Pointer &image );

template <> inline SEXP wrap( const itk::Image<float,2>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<float,3>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<float,4>::Pointer &image );

template <> inline SEXP wrap( const itk::Image<unsigned int,2>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<unsigned int,3>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<unsigned int,4>::Pointer &image );

template <> inline SEXP wrap( const itk::Image<unsigned char,2>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<unsigned char,3>::Pointer &image );
template <> inline SEXP wrap( const itk::Image<unsigned char,4>::Pointer &image );

// antsImage to itk::Image
template <> inline itk::Image<double,2>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<double,3>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<double,4>::Pointer as( SEXP itkImageR );

template <> inline itk::Image<float,2>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<float,3>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<float,4>::Pointer as( SEXP itkImageR );

template <> inline itk::Image<unsigned int,2>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<unsigned int,3>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<unsigned int,4>::Pointer as( SEXP itkImageR );

template <> inline itk::Image<unsigned char,2>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<unsigned char,3>::Pointer as( SEXP itkImageR );
template <> inline itk::Image<unsigned char,4>::Pointer as( SEXP itkImageR );
*/

// itk::VectorImage to antsImage
template <> inline SEXP wrap( const itk::VectorImage<double,2>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<double,3>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<double,4>::Pointer &image );

template <> inline SEXP wrap( const itk::VectorImage<float,2>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<float,3>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<float,4>::Pointer &image );

template <> inline SEXP wrap( const itk::VectorImage<unsigned int,2>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<unsigned int,3>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<unsigned int,4>::Pointer &image );

template <> inline SEXP wrap( const itk::VectorImage<unsigned char,2>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<unsigned char,3>::Pointer &image );
template <> inline SEXP wrap( const itk::VectorImage<unsigned char,4>::Pointer &image );

// antsImage to itk::VectorImage
template <> inline itk::VectorImage<double,2>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<double,3>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<double,4>::Pointer as( SEXP itkImageR );

template <> inline itk::VectorImage<float,2>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<float,3>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<float,4>::Pointer as( SEXP itkImageR );

template <> inline itk::VectorImage<unsigned char,2>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<unsigned char,3>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<unsigned char,4>::Pointer as( SEXP itkImageR );

template <> inline itk::VectorImage<unsigned int,2>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<unsigned int,3>::Pointer as( SEXP itkImageR );
template <> inline itk::VectorImage<unsigned int,4>::Pointer as( SEXP itkImageR );

// itkIterator to antsrImageIterator
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > & iterator );

template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > & iterator );

template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > & iterator );

template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > & iterator );
template <> inline SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > & iterator );

// antsrImageIterator to itkIterator
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > as( SEXP itkImageIteratorR );

template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > as( SEXP itkImageIteratorR );

template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > as( SEXP itkImageIteratorR );

template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > as( SEXP itkImageIteratorR );
template <> inline itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > as( SEXP itkImageIteratorR );

// antsTransform functions
template <> inline SEXP wrap( const itk::Transform<double,2,2>::Pointer &itkTransform );
template <> inline SEXP wrap( const itk::Transform<double,3,3>::Pointer &itkTransform );
template <> inline SEXP wrap( const itk::Transform<double,4,4>::Pointer &itkTransform );

template <> inline SEXP wrap( const itk::Transform<float,2,2>::Pointer &itkTransform );
template <> inline SEXP wrap( const itk::Transform<float,3,3>::Pointer &itkTransform );
template <> inline SEXP wrap( const itk::Transform<float,4,4>::Pointer &itkTransform );

template <> inline itk::Transform<double,2,2>::Pointer as( SEXP antsTransform );
template <> inline itk::Transform<double,3,3>::Pointer as( SEXP antsTransform );
template <> inline itk::Transform<double,4,4>::Pointer as( SEXP antsTransform );

template <> inline itk::Transform<float,2,2>::Pointer  as( SEXP antsTransform );
template <> inline itk::Transform<float,3,3>::Pointer  as( SEXP antsTransform );
template <> inline itk::Transform<float,4,4>::Pointer  as( SEXP antsTransform );

}

// This needs to go after wrap declarations and before implementations
#include <Rcpp.h>

namespace Rcpp {

template <> inline
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

template <> inline
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

template <> inline
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

#include <RcppANTsR/RcppANTsR_ImageBaseImp.h>
#include <RcppANTsR/RcppANTsR_ImageImp.h>
#include <RcppANTsR/RcppANTsR_VectorImageImp.h>
#include <RcppANTsR/RcppANTsR_ImageIteratorImp.h>
#include <RcppANTsR/RcppANTsR_TransformImp.h>
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

/*
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
*/

template <> inline
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

template <> inline
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

template <> inline
SEXP wrap( const itk::VectorImage<double,4>::Pointer &image )
{
  typedef itk::VectorImage<double,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "double";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <> inline
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

template <> inline
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

template <> inline
SEXP wrap( const itk::VectorImage<float,4>::Pointer &image )
{
  typedef itk::VectorImage<float,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "float";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <> inline
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

template <> inline
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

template <> inline
SEXP wrap( const itk::VectorImage<unsigned int,4>::Pointer &image )
{
  typedef itk::VectorImage<unsigned int,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned int";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <> inline
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

template <> inline
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

template <> inline
SEXP wrap( const itk::VectorImage<unsigned char,4>::Pointer &image )
{
  typedef itk::VectorImage<unsigned char,4>        ImageType;
  typedef ImageType::Pointer ImagePointerType;

  ImagePointerType* rawPointer = new ImagePointerType( image );
  Rcpp::XPtr< ImagePointerType > xptr( rawPointer , true ) ;

  Rcpp::S4 itkImage( std::string( "antsImage" ) );
  itkImage.slot( "pixeltype" ) = "unsigned char";
  itkImage.slot( "dimension" ) = 4;
  itkImage.slot( "components" ) = image->GetNumberOfComponentsPerPixel();
  itkImage.slot( "pointer") = xptr;

  return(wrap(itkImage));
}

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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

template <> inline
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
template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::IMAGETYPE<PIXELTYPE,DIMENSION> > & iterator )
{
  typedef itk::IMAGETYPE<PIXELTYPE,DIMENSION>          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > & iterator )
{
  typedef itk::Image<double,2>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > & iterator )
{
  typedef itk::Image<double,3>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > & iterator )
{
  typedef itk::Image<double,4>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > & iterator )
{
  typedef itk::Image<float,2>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > & iterator )
{
  typedef itk::Image<float,3>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > & iterator )
{
  typedef itk::Image<float,4>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > & iterator )
{
  typedef itk::Image<unsigned int,2>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > & iterator )
{
  typedef itk::Image<unsigned int,3>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > & iterator )
{
  typedef itk::Image<unsigned int,4>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > & iterator )
{
  typedef itk::Image<unsigned char,2>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > & iterator )
{
  typedef itk::Image<unsigned char,3>                          ImageType;
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

template <> inline
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > & iterator )
{
  typedef itk::Image<unsigned char,4>                          ImageType;
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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<double,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<float,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned int,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 2;
  typedef itk::Image<unsigned char,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 3;
  typedef itk::Image<unsigned char,Dim>                        ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > as( SEXP itkImageIteratorR )
{
  const unsigned int Dim = 4;
  typedef itk::Image<unsigned char,Dim>                ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;

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

template <> inline
SEXP wrap( const itk::Transform<double,2,2>::Pointer & itkTransform )
{
  typedef itk::Transform<double,2,2>      TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 2;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
SEXP wrap( const itk::Transform<double,3,3>::Pointer & itkTransform )
{
  typedef itk::Transform<double,3,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 3;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
SEXP wrap( const itk::Transform<double,4,4>::Pointer & itkTransform )
{
  typedef itk::Transform<double,4,4>      TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 4;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
SEXP wrap( const itk::Transform<float,2,2>::Pointer & itkTransform )
{
  typedef itk::Transform<float,2,2>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 2;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
SEXP wrap( const itk::Transform<float,3,3>::Pointer & itkTransform )
{
  typedef itk::Transform<float,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 3;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
SEXP wrap( const itk::Transform<float,4,4>::Pointer & itkTransform )
{
  typedef itk::Transform<float,4,4>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 4;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <> inline
itk::Transform<double,2,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim)  )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <> inline
itk::Transform<double,3,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <> inline
itk::Transform<double,4,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <> inline
itk::Transform<float,2,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::Transform<float,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <> inline
itk::Transform<float,3,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::Transform<float,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <> inline
itk::Transform<float,4,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::Transform<float,Dim,Dim>              TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}


}

#endif
