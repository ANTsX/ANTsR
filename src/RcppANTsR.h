#include "itkMacro.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/vnl_vector_ref.h"

#include <RcppCommon.h>

// From advice @ http://dirk.eddelbuettel.com/code/rcpp/Rcpp-extending.pdf

inline SEXP exception_to_r_condition( const itk::ExceptionObject & ex)
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

inline void forward_exception_to_r( const itk::ExceptionObject & ex)
{
  SEXP stop_sym  = Rf_install( "stop" ) ;
  Rcpp::Shield<SEXP> condition( exception_to_r_condition(ex) );
  Rcpp::Shield<SEXP> expr( Rf_lang2( stop_sym , condition ) ) ;
  Rf_eval( expr, R_GlobalEnv ) ;
}

namespace Rcpp {

//template<class ImageType>
//SEXP wrap( const typename ImageType::Pointer &image );

//template<typename T>
//SEXP wrap( const typename vnl_vector<T> &vector );

template <>
SEXP wrap( const itk::Image<double,2>::Pointer &image );

template <>
SEXP wrap( const itk::Image<double,3>::Pointer &image );

template <>
SEXP wrap( const itk::Image<double,4>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<double,2>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<double,3>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<double,4>::Pointer &image );


template <>
SEXP wrap( const itk::Image<float,2>::Pointer &image );

template <>
SEXP wrap( const itk::Image<float,3>::Pointer &image );

template <>
SEXP wrap( const itk::Image<float,4>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<float,2>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<float,3>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<float,4>::Pointer &image );


template <>
SEXP wrap( const itk::Image<unsigned int,2>::Pointer &image );

template <>
SEXP wrap( const itk::Image<unsigned int,3>::Pointer &image );

template <>
SEXP wrap( const itk::Image<unsigned int,4>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned int,2>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned int,3>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned int,4>::Pointer &image );


template <>
SEXP wrap( const itk::Image<unsigned char,2>::Pointer &image );

template <>
SEXP wrap( const itk::Image<unsigned char,3>::Pointer &image );

template <>
SEXP wrap( const itk::Image<unsigned char,4>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned char,2>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned char,3>::Pointer &image );

template <>
SEXP wrap( const itk::VectorImage<unsigned char,4>::Pointer &image );


template <>
itk::ImageBase<2>::Pointer as( SEXP itkImageR );

template <>
itk::ImageBase<3>::Pointer as( SEXP itkImageR );

template <>
itk::ImageBase<4>::Pointer as( SEXP itkImageR );


template <>
itk::Image<double,2>::Pointer as( SEXP itkImageR );

template <>
itk::Image<double,3>::Pointer as( SEXP itkImageR );

template <>
itk::Image<double,4>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<double,2>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<double,3>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<double,4>::Pointer as( SEXP itkImageR );


template <>
itk::Image<float,2>::Pointer as( SEXP itkImageR );

template <>
itk::Image<float,3>::Pointer as( SEXP itkImageR );

template <>
itk::Image<float,4>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<float,2>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<float,3>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<float,4>::Pointer as( SEXP itkImageR );


template <>
itk::Image<unsigned int,2>::Pointer as( SEXP itkImageR );

template <>
itk::Image<unsigned int,3>::Pointer as( SEXP itkImageR );

template <>
itk::Image<unsigned int,4>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned int,2>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned int,3>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned int,4>::Pointer as( SEXP itkImageR );


template <>
itk::Image<unsigned char,2>::Pointer as( SEXP itkImageR );

template <>
itk::Image<unsigned char,3>::Pointer as( SEXP itkImageR );

template <>
itk::Image<unsigned char,4>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned char,2>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned char,3>::Pointer as( SEXP itkImageR );

template <>
itk::VectorImage<unsigned char,4>::Pointer as( SEXP itkImageR );

// antsImageIterator conversions

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > & iterator );

template <>
SEXP wrap( const itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > & iterator );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,2> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,3> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<double,4> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,2> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,3> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<float,4> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,2> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,3> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned int,4> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,2> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,3> > as( SEXP itkImageIteratorR );

template <>
itk::ImageRegionIteratorWithIndex< itk::Image<unsigned char,4> > as( SEXP itkImageIteratorR );
}

// This needs to go after wrap declarations
#include <Rcpp.h>
