#ifndef __RCPPANTSR_TRANSFORM_H
#define __RCPPANTSR_TRANSFORM_H

#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/vnl_vector_ref.h"
#include "itkTransform.h"
#include "itkAffineTransform.h"

#include <RcppCommon.h>

namespace Rcpp {

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


#endif
