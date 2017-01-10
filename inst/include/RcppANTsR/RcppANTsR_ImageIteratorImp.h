#ifndef __RCPPANTSR_IMAGEITERATOR_HPP
#define __RCPPANTSR_IMAGEITERATOR_HPP

#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <Rcpp.h>

namespace Rcpp {

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

}

#endif
