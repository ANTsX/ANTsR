#ifndef __RCPPANTSR_IMAGEITERATOR_H
#define __RCPPANTSR_IMAGEITERATOR_H

#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"

#include <RcppCommon.h>

namespace Rcpp {

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

}


#endif
