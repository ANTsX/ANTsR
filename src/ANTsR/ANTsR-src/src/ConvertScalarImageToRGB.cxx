
#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"

RcppExport SEXP ConvertScalarImageToRGB( SEXP r_args )
try
{
  return Rcpp::wrap( ants::ConvertScalarImageToRGB( Rcpp::as< std::vector<std::string> >( r_args ) , &Rcpp::Rcout ) ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( EXIT_FAILURE ) ;
   }
