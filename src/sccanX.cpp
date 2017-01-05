#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"
#include "antsr.h"

RcppExport SEXP sccanX( SEXP r_args )
try
{
  std::vector< std::string > args = Rcpp::as< std::vector< std::string > >( r_args ) ;
  std::transform( args.begin() , args.end() , args.begin() , process_pointers ) ;
  if( insert_commas( args , args.begin() ) )
    {
      return Rcpp::wrap( 1 ) ;
    }
  ants::sccan( args , &Rcpp::Rcout );
  return 0;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
