#ifndef __RCPPANTSR_IMAGEBASE_H
#define __RCPPANTSR_IMAGEBASE_H

#include "itkMacro.h"
#include "itkImageBase.h"
#include <RcppCommon.h>

namespace Rcpp {

// These are used to get header info only for both itk::Image and itk::VectorImage
template <> inline itk::ImageBase<2>::Pointer as( SEXP itkImageR );
template <> inline itk::ImageBase<3>::Pointer as( SEXP itkImageR );
template <> inline itk::ImageBase<4>::Pointer as( SEXP itkImageR );

}

#endif
