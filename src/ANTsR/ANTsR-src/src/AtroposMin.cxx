
#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"

RcppExport SEXP AtroposMin( SEXP r_args )
try
{
  return Rcpp::wrap( ants::AtroposMin( Rcpp::as< std::vector<std::string> >( r_args ) , &Rcpp::Rcout ) ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( EXIT_FAILURE ) ;
   }
