
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "itkAffineTransform.h"
#include "itkTranslationTransform.h"

template< class TransformType >
Rcpp::XPtr<typename TransformType::Pointer> antsTransformGetXPtr()
{
  typedef typename TransformType::Pointer           TransformPointerType;
  TransformPointerType transformPtr = TransformType::New();

  TransformPointerType * rawPointer = new TransformPointerType( transformPtr );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );
  return xptr;
}

template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform( SEXP r_precision, SEXP r_dimension, SEXP r_type )
{

  Rcpp::S4 transform( "antsTransform" );
  std::string type = Rcpp::as<std::string>( r_type );
  transform.slot("type") = type;
  transform.slot("dimension") = Rcpp::as< unsigned int >( r_dimension );
  transform.slot("precision") = Rcpp::as<std::string>( r_precision );

  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    return Rcpp::wrap( transformPointer );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  return transform;
}

RcppExport SEXP antsTransform( SEXP r_precision, SEXP r_dimension, SEXP r_type )
{
try
{
  std::string precision = Rcpp::as< std::string >( r_precision );
  unsigned int dimension = Rcpp::as< int >( r_dimension );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  if ( (precision != "float") && (precision != "double"))
    {
    Rcpp::stop( "Precision must be 'float' or 'double'");
    }

  if( precision == "double" )
    {
    typedef double PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform<PrecisionType,4>( r_precision, r_dimension, r_type  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform<PrecisionType,3>( r_precision, r_dimension, r_type  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform<PrecisionType,2>( r_precision, r_dimension, r_type  );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform<PrecisionType,4>( r_precision, r_dimension, r_type  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform<PrecisionType,3>( r_precision, r_dimension, r_type  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform<PrecisionType,2>( r_precision, r_dimension, r_type  );
	    }
    }

  return( Rcpp::wrap(NA_REAL) );

}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc ) ;
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}

template< class TransformType >
SEXP antsTransform_GetParameters( SEXP r_transform )
{
  typedef typename TransformType::Pointer           TransformPointerType;

  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );
  Rcpp::NumericVector parameters( itkTransform->GetNumberOfParameters() );

  for (unsigned int i=0; i<itkTransform->GetNumberOfParameters(); i++ )
  {
    parameters[i] = itkTransform->GetParameters()[i];
  }

  return parameters;
}

template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_GetParameters( SEXP r_transform )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    return antsTransform_GetParameters<TransformType>( transform );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  return Rcpp::wrap(NA_REAL);
}


RcppExport SEXP antsTransform_GetParameters( SEXP r_transform )
{
try
{
  Rcpp::S4 transform( r_transform );

  std::string precision = Rcpp::as<std::string>( transform.slot("precision") );
  unsigned int dimension = Rcpp::as<int>( transform.slot("dimension") );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  if ( (precision != "float") && (precision != "double"))
    {
    Rcpp::stop( "Precision must be 'float' or 'double'");
    }

  if( precision == "double" )
    {
    typedef double PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_GetParameters<PrecisionType,4>( r_transform  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_GetParameters<PrecisionType,3>( r_transform  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_GetParameters<PrecisionType,2>( r_transform );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_GetParameters<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_GetParameters<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_GetParameters<PrecisionType,2>( r_transform );
	    }
    }

  return( Rcpp::wrap(NA_REAL) );

}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc ) ;
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}

// Set transform parameters

template< class TransformType >
SEXP antsTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
{
  typedef typename TransformType::Pointer          TransformPointerType;

  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );
  Rcpp::NumericVector parameters( r_parameters );

  if ( parameters.size() != itkTransform->GetNumberOfParameters() )
  {
    Rcpp::stop("Incorrect number of parameters passed");
  }

  typename TransformType::ParametersType itkParameters;
  itkParameters.SetSize( itkTransform->GetNumberOfParameters() );
  for (unsigned int i=0; i < itkTransform->GetNumberOfParameters(); i++)
  {
    itkParameters[i] = parameters[i];
  }

  itkTransform->SetParameters( itkParameters );

  return(Rcpp::wrap(true));

}

template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    return antsTransform_SetParameters<TransformType>( r_transform, r_parameters );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  return Rcpp::wrap(NA_REAL);
}


RcppExport SEXP antsTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
{
try
{
  Rcpp::S4 transform( r_transform );

  std::string precision = Rcpp::as<std::string>( transform.slot("precision") );
  unsigned int dimension = Rcpp::as<int>( transform.slot("dimension") );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  if ( (precision != "float") && (precision != "double"))
    {
    Rcpp::stop( "Precision must be 'float' or 'double'");
    }

  if( precision == "double" )
    {
    typedef double PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_SetParameters<PrecisionType,4>( r_transform, r_parameters  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_SetParameters<PrecisionType,3>( r_transform, r_parameters  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_SetParameters<PrecisionType,2>( r_transform, r_parameters );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_SetParameters<PrecisionType,4>( r_transform, r_parameters );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_SetParameters<PrecisionType,3>( r_transform, r_parameters );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_SetParameters<PrecisionType,2>( r_transform, r_parameters );
	    }
    }

  return( Rcpp::wrap(NA_REAL) );

}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc ) ;
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}


// Apply transform to point
template< class TransformType >
SEXP antsTransform_TransformPoint( SEXP r_transform, SEXP r_point )
{
  typedef typename TransformType::Pointer          TransformPointerType;
  typedef typename TransformType::InputPointType   InputPointType;
  typedef typename TransformType::OutputPointType  OutputPointType;

  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );
  Rcpp::NumericVector inPoint( r_point );

  InputPointType inItkPoint;
  for (unsigned int i=0; i<InputPointType::PointDimension; i++)
    {
    inItkPoint[i] = inPoint[i];
    }

  OutputPointType outItkPoint = itkTransform->TransformPoint( inItkPoint );

  Rcpp::NumericVector outPoint( OutputPointType::PointDimension );
  for (unsigned int i=0; i<OutputPointType::PointDimension; i++)
    {
    outPoint[i] = outItkPoint[i];
    }

  return outPoint;
}

template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_TransformPoint( SEXP r_transform, SEXP r_point )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    return antsTransform_TransformPoint<TransformType>( r_transform, r_point );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  return Rcpp::wrap(NA_REAL);
}


RcppExport SEXP antsTransform_TransformPoint( SEXP r_transform, SEXP r_point )
{
try
{
  Rcpp::S4 transform( r_transform );

  std::string precision = Rcpp::as<std::string>( transform.slot("precision") );
  unsigned int dimension = Rcpp::as<int>( transform.slot("dimension") );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  if ( (precision != "float") && (precision != "double"))
    {
    Rcpp::stop( "Precision must be 'float' or 'double'");
    }

  if( precision == "double" )
    {
    typedef double PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_TransformPoint<PrecisionType,4>( r_transform, r_point  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformPoint<PrecisionType,3>( r_transform, r_point  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformPoint<PrecisionType,2>( r_transform, r_point );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_TransformPoint<PrecisionType,4>( r_transform, r_point );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformPoint<PrecisionType,3>( r_transform, r_point );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformPoint<PrecisionType,2>( r_transform, r_point );
	    }
    }

  return( Rcpp::wrap(NA_REAL) );

}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc ) ;
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
