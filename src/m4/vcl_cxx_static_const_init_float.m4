
AC_DEFUN([VCL_CXX_STATIC_CONST_INIT_FLOAT],
[AC_MSG_CHECKING(whether to use VCL_CAN_STATIC_CONST_INIT_FLOAT=1)
dnl set language to c++
AC_LANG_CPLUSPLUS
dnl check if a custom piece of code can be compiled with 
dnl VCL_CAN_STATIC_CONST_INIT_FLOAT=1
dnl if not, we set VCL_CAN_STATIC_CONST_INIT_FLOAT=0

AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include<algorithm>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "ants.h"
#include "antsr.h"

RcppExport SEXP Atropos( SEXP r_args )
try
{
  std::vector< std::string > args = Rcpp::as< std::vector< std::string > >( r_args ) ;
  std::transform( args.begin() , args.end() , args.begin() , process_pointers ) ;
  if( insert_commas( args , args.begin() ) )
    {
      return Rcpp::wrap( 1 ) ;
    }
  return Rcpp::wrap( ants::Atropos( args , &Rcpp::Rcout ) ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( 1 ) ;
   }



]])],[VCL_STATIC_CONST_INIT_FLOAT=1;AC_MSG_RESULT(yes)],[VCL_STATIC_CONST_INIT_FLOAT=0;AC_MSG_RESULT(no)])
dnl export variable
export VCL_STATIC_CONST_INIT_FLOAT
])

