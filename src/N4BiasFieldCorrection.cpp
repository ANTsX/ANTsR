#include<algorithm>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"
#include "antsr.h"

RcppExport SEXP N4BiasFieldCorrection( SEXP r_args )
try
{
  std::vector< std::string > args = Rcpp::as< std::vector< std::string > >( r_args ) ;
  std::transform( args.begin() , args.end() , args.begin() , process_pointers ) ;
  if( insert_commas( args , args.begin() ) )
    {
      return Rcpp::wrap( 1 ) ;
    }
  return Rcpp::wrap( ants::N4BiasFieldCorrection( args , &Rcpp::Rcout ) ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
