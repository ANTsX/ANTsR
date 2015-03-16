#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "vnl/vnl_vector_ref.h"

#include <RcppCommon.h>

// From advice @ http://dirk.eddelbuettel.com/code/rcpp/Rcpp-extending.pdf

namespace Rcpp {

//template<class ImageType>
//SEXP wrap( const typename ImageType::Pointer &image );

//template<typename T>
//SEXP wrap( const typename vnl_vector<T> &vector );

template <>
SEXP wrap( const typename itk::Image<double,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<double,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<double,4>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<double,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<double,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<double,4>::Pointer &image );


template <>
SEXP wrap( const typename itk::Image<float,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<float,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<float,4>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<float,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<float,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<float,4>::Pointer &image );


template <>
SEXP wrap( const typename itk::Image<unsigned int,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<unsigned int,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<unsigned int,4>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned int,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned int,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned int,4>::Pointer &image );


template <>
SEXP wrap( const typename itk::Image<unsigned char,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<unsigned char,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::Image<unsigned char,4>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned char,2>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned char,3>::Pointer &image );

template <>
SEXP wrap( const typename itk::VectorImage<unsigned char,4>::Pointer &image );


template <>
typename itk::ImageBase<2>::Pointer as( SEXP itkImageR );

template <>
typename itk::ImageBase<3>::Pointer as( SEXP itkImageR );

template <>
typename itk::ImageBase<4>::Pointer as( SEXP itkImageR );


template <>
typename itk::Image<double,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<double,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<double,4>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<double,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<double,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<double,4>::Pointer as( SEXP itkImageR );


template <>
typename itk::Image<float,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<float,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<float,4>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<float,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<float,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<float,4>::Pointer as( SEXP itkImageR );


template <>
typename itk::Image<unsigned int,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<unsigned int,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<unsigned int,4>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned int,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned int,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned int,4>::Pointer as( SEXP itkImageR );


template <>
typename itk::Image<unsigned char,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<unsigned char,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::Image<unsigned char,4>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned char,2>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned char,3>::Pointer as( SEXP itkImageR );

template <>
typename itk::VectorImage<unsigned char,4>::Pointer as( SEXP itkImageR );


}

// This needs to go after wrap declarations
#include <Rcpp.h>
