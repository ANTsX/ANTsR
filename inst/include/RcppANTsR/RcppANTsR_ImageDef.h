#ifndef __RCPPANTSR_IMAGE_H
#define __RCPPANTSR_IMAGE_H

#include "itkMacro.h"
#include "itkImage.h"
#include <RcppCommon.h>

namespace Rcpp {

// itk::Image to antsImage
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

}

#endif
