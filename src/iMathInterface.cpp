#include <RcppANTsR.h>
#include "iMathFunctions.h"

template <class ImageType>
SEXP iMathCanny( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double sigma = Rcpp::as<double>( args[2] );
  double lower = Rcpp::as<double>( args[3] );
  double upper = Rcpp::as<double>( args[4] );

  ImagePointerType output = ants::iMathCanny<ImageType>( input, sigma, lower, upper );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathGetLargestComponent( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned int minSize = Rcpp::as<unsigned int>( args[2] );

  ImagePointerType output = ants::iMathGetLargestComponent<ImageType>( input, minSize );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathNormalize( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );

  ImagePointerType output = ants::iMathNormalize<ImageType>( input );

  return Rcpp::wrap(output);
 }

RcppExport SEXP iMathInterface( SEXP r_args )
{
try
{
  Rcpp::List args(r_args);
  std::string operation = Rcpp::as< std::string >( args[1] );

  unsigned int dim = 0;
  unsigned int components = 0;
  std::string pixeltype = "";

  if ( operation == "Canny" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( args.size() < 5 )
      {
      Rcpp::stop("To few input parameters");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }

    }
  else if ( operation == "GetLargestComponent" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GetLargestComponent only supports scalar images");
      }

    // Optional parameters with defaul values
    if ( args.size() < 3 )
      {
      args.push_back( Rcpp::wrap(50) );  // minSize
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    }
  else if ( operation == "Normalize" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("Normalize only supports scalar images");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    }
  else
    {
    Rcpp::Rcout << "Operation = " << operation << std::endl;
    Rcpp::stop( "Unknown iMath operation");
    }

  return Rcpp::wrap(NA_REAL); // FIXME - for debugging only
}
catch( const itk::ExceptionObject& err )
  {
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }

return Rcpp::wrap(NA_REAL); //not reached
}
