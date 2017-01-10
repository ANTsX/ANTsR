#ifndef __RCPPANTSR_VECTORIMAGE_H
#define __RCPPANTSR_VECTORIMAGE_H

#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"

#include <RcppCommon.h>

namespace Rcpp {

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

}

#endif
