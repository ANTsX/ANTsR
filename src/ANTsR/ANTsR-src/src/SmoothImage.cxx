
#include<algorithm>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"

namespace
{
  // replaces the external pointers of the form '<pointer: 0xAABBCCDD>' to '0xAABBCCDD'
  std::string process_pointers( std::string str )
  {
    std::string::size_type xpos = str.find( "<pointer: " ) ;
    if( xpos != std::string::npos )
      {
	str.erase( xpos , 10 ) ;
	str.erase( str.find( '>' , xpos ) , 1 ) ;
	return process_pointers( str ) ;
      }
    return str ;
  }

  bool contains_onlyopenbrace( std::string& str )
  {
    return ( ( str.find( '[' ) != std::string::npos ) && ( str.find( ']' ) == std::string::npos ) ) ;
  }
  bool contains_onlyclosebrace( std::string& str )
  {
    return ( ( str.find( '[' ) == std::string::npos ) && ( str.find( ']' ) != std::string::npos ) ) ;
  }
  std::string append_comma( std::string str ) 
  {
    str.append( "," ) ;
    return str ;
  }

  // inserts std::string( "," ) between parameters appearing in the pattern '[ param1 param2 parm3 ]' 
  // so the replaced pattern is of the form '[ param1 , param2 , param3 ]'
  bool insert_commas( std::vector< std::string >& args , std::vector< std::string >::iterator start_iter  )
  {
    std::vector< std::string >::iterator pos_openbrace = std::find_if( start_iter , args.end() , contains_onlyopenbrace ) ;
    if( pos_openbrace == args.end() )
      {
	return 0 ;
      }
    std::vector< std::string >::iterator pos_closebrace = std::find_if( pos_openbrace , args.end() , contains_onlyclosebrace ) ;
    if( pos_closebrace == args.end() )
      {
	Rcpp::Rcout << "Unbalanced brackets" << std::endl ;
	return 1 ;
      }
    std::transform( pos_openbrace + 1 , pos_closebrace - 1 , pos_openbrace + 1 , append_comma ) ;
    return insert_commas( args , pos_closebrace ) ;
  }

} // namespace

RcppExport SEXP SmoothImage( SEXP r_args )
try
{
  std::vector< std::string > args = Rcpp::as< std::vector< std::string > >( r_args ) ;
  std::transform( args.begin() , args.end() , args.begin() , process_pointers ) ;
  if( insert_commas( args , args.begin() ) )
    {
      return Rcpp::wrap( 1 ) ;
    }
  return Rcpp::wrap( ants::SmoothImage( args , &Rcpp::Rcout ) ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }
