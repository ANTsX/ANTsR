#ifndef __RCPPANTSR_IMAGEBASE_HPP
#define __RCPPANTSR_IMAGEBASE_HPP

#include "itkMacro.h"
#include "itkImageBase.h"
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

}

#endif
