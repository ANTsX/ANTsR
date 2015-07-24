#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "antsSCCANObject.h"
using namespace Rcpp;

RcppExport SEXP robustMatrixTransform( SEXP r_matrix )
{
try
{
  typedef double RealType;
  NumericMatrix M = as< NumericMatrix >( r_matrix );
  NumericMatrix outMat( M.rows(), M.cols() );
  unsigned long rows = M.rows();
  for( unsigned long j = 0; j < M.cols(); j++ )
    {
    NumericVector Mvec = M(_, j);
    NumericVector rank = M(_, j);
    for( unsigned int i = 0; i < rows; i++ )
      {
      RealType   rankval = 0;
      RealType   xi = Mvec(i);
      for( unsigned int k = 0; k < rows; k++ )
        {
        RealType yi = Mvec(k);
        RealType diff = fabs(xi - yi);
        if( diff > 0 )
          {
          RealType val = (xi - yi) / diff;
          rankval += val;
          }
        }
      rank(i) = rankval / rows;
      }
    outMat(_, j) = rank;
    }
  // this passes rcpp data to vnl matrix ... safely?
  if ( 1 == 0 )
    {
    std::vector<RealType> dat =
      Rcpp::as< std::vector<RealType> >( outMat );
    const double* _data = &dat[0];
    vnl_matrix<RealType> vnlmat( _data , M.cols(), M.rows()  );
    vnlmat = vnlmat.transpose();
    }
  return wrap( outMat );
}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  Rcpp::Rcout << "STD ExceptionObject caught !" << std::endl;
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
