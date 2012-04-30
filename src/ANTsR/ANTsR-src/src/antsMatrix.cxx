
#include<utility>
#include<vector>
#include<string>
#include<Rcpp.h>
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

RcppExport SEXP antsMatrix( SEXP r_elementtype )
try
{
  if( r_elementtype == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  std::string elementtype = Rcpp::as< std::string >( r_elementtype ) ;

  if( elementtype == "double" )
    {
      typedef double ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "double" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      Rcpp::Rcout << "Empty matrix created. ElementType: 'double'" << std::endl ;
      return matrix_r ;
    }
  if( elementtype == "float" )
    {
      typedef float ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "float" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      Rcpp::Rcout << "Empty matrix created. ElementType: 'float'" << std::endl ;
      return matrix_r ;
    }
  if( elementtype == "unsigned int" )
    {
      typedef unsigned int ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "unsigned int" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      Rcpp::Rcout << "Empty matrix created. ElementType: 'unsigned int'" << std::endl ;
      return matrix_r ;
    }
  if( elementtype == "unsigned char" )
    {
      typedef unsigned char ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "unsigned char" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      Rcpp::Rcout << "Empty matrix created. ElementType: 'unsigned char'" << std::endl ;
      return matrix_r ;
    }
  else
    {
      Rcpp::Rcout << "Unsupported ElementType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }

RcppExport SEXP antsMatrix_asList( SEXP r_antsmatrix )
try
{
  if( r_antsmatrix == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsmatrix( r_antsmatrix ) ;
  std::string elementtype = Rcpp::as< std::string >( antsmatrix.slot( "elementtype" ) ) ;

  if( elementtype == "double" )
    {
      typedef double ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > antsmatrix_xptr( static_cast< SEXP >( antsmatrix.slot( "pointer" ) ) ) ;
      Rcpp::List list_r ;
      for( std::vector< std::string >::size_type i = 0 ; i < antsmatrix_xptr->first.size() ; ++i )
	{
	  list_r.push_back( antsmatrix_xptr->second.get_column(i) ) ;
	}
      list_r.push_back( antsmatrix_xptr->first ) ;
      return list_r ;
    }
  if( elementtype == "float" )
    {
      typedef float ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > antsmatrix_xptr( static_cast< SEXP >( antsmatrix.slot( "pointer" ) ) ) ;
      Rcpp::List list_r ;
      for( std::vector< std::string >::size_type i = 0 ; i < antsmatrix_xptr->first.size() ; ++i )
	{
	  list_r.push_back( antsmatrix_xptr->second.get_column(i) ) ;
	}
      list_r.push_back( antsmatrix_xptr->first ) ;
      return list_r ;
    }
  if( elementtype == "unsigned int" )
    {
      typedef unsigned int ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > antsmatrix_xptr( static_cast< SEXP >( antsmatrix.slot( "pointer" ) ) ) ;
      Rcpp::List list_r ;
      for( std::vector< std::string >::size_type i = 0 ; i < antsmatrix_xptr->first.size() ; ++i )
	{
	  list_r.push_back( antsmatrix_xptr->second.get_column(i) ) ;
	}
      list_r.push_back( antsmatrix_xptr->first ) ;
      return list_r ;
    }
  if( elementtype == "unsigned char" )
    {
      typedef unsigned char ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      Rcpp::XPtr< antsMatrixType > antsmatrix_xptr( static_cast< SEXP >( antsmatrix.slot( "pointer" ) ) ) ;
      Rcpp::List list_r ;
      for( std::vector< std::string >::size_type i = 0 ; i < antsmatrix_xptr->first.size() ; ++i )
	{
	  list_r.push_back( antsmatrix_xptr->second.get_column(i) ) ;
	}
      list_r.push_back( antsmatrix_xptr->first ) ;
      return list_r ;
    }
  else
    {
      Rcpp::Rcout << "Unsupported ElementType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }

RcppExport SEXP antsMatrix_asantsMatrix( SEXP r_list , SEXP r_elementtype )
try
{
  if( r_list == NULL || r_elementtype == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  std::string elementtype = Rcpp::as< std::string >( r_elementtype ) ;

  if( elementtype == "double" )
    {
      typedef double ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::List list( r_list ) ;
      ptr_matrix->first = Rcpp::as< std::vector< std::string > >( list.names() ) ;
      if( ptr_matrix->second.set_size( list.size() , ( Rcpp::as< std::vector< ElementType > >( list[0] ) ).size() ) == 0 )
	{
	  Rcpp::Rcout << "Failed to set number of rows and columns" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
      for( int i = 0 ; i < list.size() ; ++i )
	{
	  std::vector< ElementType > column = Rcpp::as< std::vector< ElementType > >( list[i] ) ;
	  ptr_matrix->second.set_column( i , column.data() ) ;
	}
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "double" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      return matrix_r ;
    }
  else if( elementtype == "float" )
    {
      typedef float ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::List list( r_list ) ;
      ptr_matrix->first = Rcpp::as< std::vector< std::string > >( list.names() ) ;
      if( ptr_matrix->second.set_size( list.size() , ( Rcpp::as< std::vector< ElementType > >( list[0] ) ).size() ) == 0 )
	{
	  Rcpp::Rcout << "Failed to set number of rows and columns" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
      for( int i = 0 ; i < list.size() ; ++i )
	{
	  std::vector< ElementType > column = Rcpp::as< std::vector< ElementType > >( list[i] ) ;
	  ptr_matrix->second.set_column( i , column.data() ) ;
	}
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "float" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      return matrix_r ;
    }
  else if( elementtype == "unsigned int" )
    {
      typedef unsigned int ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::List list( r_list ) ;
      ptr_matrix->first = Rcpp::as< std::vector< std::string > >( list.names() ) ;
      if( ptr_matrix->second.set_size( list.size() , ( Rcpp::as< std::vector< ElementType > >( list[0] ) ).size() ) == 0 )
	{
	  Rcpp::Rcout << "Failed to set number of rows and columns" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
      for( int i = 0 ; i < list.size() ; ++i )
	{
	  std::vector< ElementType > column = Rcpp::as< std::vector< ElementType > >( list[i] ) ;
	  ptr_matrix->second.set_column( i , column.data() ) ;
	}
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "unsigned int" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      return matrix_r ;
    }
  else if( elementtype == "unsigned char" )
    {
      typedef unsigned char ElementType ;
      typedef vnl_matrix< ElementType > MatrixType ;
      typedef vnl_vector< ElementType > VectorType ;
      typedef std::vector< std::string > HeaderType ;
      typedef std::pair< HeaderType , MatrixType > antsMatrixType ;
      antsMatrixType* ptr_matrix = new antsMatrixType ;
      Rcpp::List list( r_list ) ;
      ptr_matrix->first = Rcpp::as< std::vector< std::string > >( list.names() ) ;
      if( ptr_matrix->second.set_size( list.size() , ( Rcpp::as< std::vector< ElementType > >( list[0] ) ).size() ) == 0 )
	{
	  Rcpp::Rcout << "Failed to set number of rows and columns" << std::endl ;
	  return Rcpp::wrap( 1 ) ;
	}
      for( int i = 0 ; i < list.size() ; ++i )
	{
	  std::vector< ElementType > column = Rcpp::as< std::vector< ElementType > >( list[i] ) ;
	  ptr_matrix->second.set_column( i , column.data() ) ;
	}
      Rcpp::XPtr< antsMatrixType > xptr( ptr_matrix , true ) ;
      Rcpp::S4 matrix_r( std::string( "antsMatrix" ) ) ;
      matrix_r.slot( "elementtype" ) = std::string( "unsigned char" ) ;
      matrix_r.slot( "pointer" ) = xptr ;
      return matrix_r ;
    }
  else
    {
      Rcpp::Rcout << "Unsupported ElementType" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }
