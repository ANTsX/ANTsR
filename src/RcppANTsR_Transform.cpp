
#include "itkMacro.h"
#include "RcppANTsR_Transform.h"

namespace Rcpp {

template<>
SEXP wrap( const itk::Transform<double,2,2>::Pointer & itkTransform )
{
  typedef itk::Transform<double,2,2>      TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 2;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template<>
SEXP wrap( const itk::Transform<double,3,3>::Pointer & itkTransform )
{
  typedef itk::Transform<double,3,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 3;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template<>
SEXP wrap( const itk::Transform<double,4,4>::Pointer & itkTransform )
{
  typedef itk::Transform<double,4,4>      TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "double";
  antsrTransform.slot( "dimension" ) = 4;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template<>
SEXP wrap( const itk::Transform<float,2,2>::Pointer & itkTransform )
{
  typedef itk::Transform<float,2,2>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 2;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template<>
SEXP wrap( const itk::Transform<float,3,3>::Pointer & itkTransform )
{
  typedef itk::Transform<float,3>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 3;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template<>
SEXP wrap( const itk::Transform<float,4,4>::Pointer & itkTransform )
{
  typedef itk::Transform<float,4,4>   TransformType;
  typedef TransformType::Pointer          TransformPointerType;

  TransformPointerType* rawPointer = new TransformPointerType( itkTransform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );

  Rcpp::S4 antsrTransform( std::string( "antsrTransform" ) );
  antsrTransform.slot( "precision" ) = "float";
  antsrTransform.slot( "dimension" ) = 4;
  antsrTransform.slot( "type" ) = itkTransform->GetNameOfClass();
  antsrTransform.slot( "pointer") = xptr;

  return( wrap(antsrTransform) );
}

template <>
itk::Transform<double,2,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim)  )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::Transform<double,3,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::Transform<double,4,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::Transform<double,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "double") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::Transform<float,2,2>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 2;
  typedef itk::Transform<float,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::Transform<float,3,3>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 3;
  typedef itk::Transform<float,Dim,Dim>         TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}

template <>
itk::Transform<float,4,4>::Pointer as( SEXP r_transform )
{
  const unsigned int Dim = 4;
  typedef itk::Transform<float,Dim,Dim>              TransformType;
  typedef TransformType::Pointer                  TransformPointerType;
  Rcpp::S4 antsrTransform( r_transform );

  if (!antsrTransform.is( "antsrTransform") ||
      (Rcpp::as<std::string>(antsrTransform.slot("precision")) != "float") ||
      (Rcpp::as<int>(antsrTransform.slot("dimension")) != Dim) )
    {
    Rcpp::stop( "Invalid S4 object type");
    }
  XPtr<TransformPointerType> xptr( static_cast<SEXP>( antsrTransform.slot("pointer") ));
  return *xptr;
}


}
