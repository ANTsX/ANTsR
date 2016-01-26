
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "antsUtilities.h"
#include "itkAffineTransform.h"
#include "itkCenteredAffineTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkTranslationTransform.h"
#include "itkResampleImageFilter.h"
#include "itkTransformFileReader.h"
#include "itkCompositeTransform.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkDisplacementFieldTransform.h"
#include "itkConstantBoundaryCondition.h"

#include "itkBSplineInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGaussianInterpolateImageFunction.h"
#include "itkInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkLabelImageGaussianInterpolateImageFunction.h"


/*
template< class TransformType >
Rcpp::XPtr<typename TransformType::Pointer> antsrTransformGetXPtr()
{
  typedef typename TransformType::Pointer           TransformPointerType;
  TransformPointerType transformPtr = TransformType::New();

  TransformPointerType * rawPointer = new TransformPointerType( transformPtr );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );
  return xptr;
}*/

template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform( SEXP r_precision, SEXP r_dimension, SEXP r_type )
{

  std::string type = Rcpp::as<std::string>( r_type );

  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();

    typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformBaseType;
    typedef typename TransformBaseType::Pointer               TransformBasePointerType;
    TransformBasePointerType basePointer
      = dynamic_cast<TransformBaseType *>( transformPointer.GetPointer() );

    return Rcpp::wrap( basePointer );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  return Rcpp::wrap(NA_REAL);
}

RcppExport SEXP antsrTransform( SEXP r_precision, SEXP r_dimension, SEXP r_type )
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
      return antsrTransform<PrecisionType,4>( r_precision, r_dimension, r_type  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform<PrecisionType,3>( r_precision, r_dimension, r_type  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform<PrecisionType,2>( r_precision, r_dimension, r_type  );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform<PrecisionType,4>( r_precision, r_dimension, r_type  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform<PrecisionType,3>( r_precision, r_dimension, r_type  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform<PrecisionType,2>( r_precision, r_dimension, r_type  );
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

template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_MatrixOffset( SEXP r_type, SEXP r_precision, SEXP r_dimension,
    SEXP r_matrix, SEXP r_offset, SEXP r_center, SEXP r_translation,
    SEXP r_parameters, SEXP r_fixedparameters )
  {

  std::string type = Rcpp::as<std::string>( r_type );
  unsigned int dimension = Rcpp::as< unsigned int >( r_dimension );

  typedef itk::MatrixOffsetTransformBase< PrecisionType, Dimension, Dimension> MatrixOffsetBaseType;
  typedef typename MatrixOffsetBaseType::Pointer                               MatrixOffsetBasePointerType;
  typedef itk::Transform<PrecisionType,Dimension,Dimension>                    TransformBaseType;
  typedef typename TransformBaseType::Pointer                                  TransformBasePointerType;

  MatrixOffsetBasePointerType matrixOffset = NULL;

  // Initialize transform by type
  if ( type == "AffineTransform" )
    {
    typedef itk::AffineTransform<PrecisionType,Dimension> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "CenteredAffineTransform" )
    {
    typedef itk::CenteredAffineTransform<PrecisionType,Dimension> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "Euler3DTransform" )
    {
    typedef itk::Euler3DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "Euler2DTransform" )
    {
    typedef itk::Euler2DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "QuaternionRigidTransform" )
    {
    typedef itk::QuaternionRigidTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "Rigid2DTransform" )
    {
    typedef itk::Rigid2DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "CenteredEuler3DTransform" )
    {
    typedef itk::CenteredEuler3DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "CenteredRigid2DTransform" )
    {
    typedef itk::CenteredRigid2DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "Similarity3DTransform" )
    {
    typedef itk::Similarity3DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "Similarity2DTransform" )
    {
    typedef itk::Similarity2DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else if ( type == "CenteredSimilarity2DTransform" )
    {
    typedef itk::CenteredSimilarity2DTransform<PrecisionType> TransformType;
    typename TransformType::Pointer transformPointer = TransformType::New();
    matrixOffset = dynamic_cast<MatrixOffsetBaseType*>( transformPointer.GetPointer() );
    }
  else
    {
    Rcpp::Rcout << "Passed transform type: " << type << std::endl;
    Rcpp::stop( "Transform type not supported" );
    }

  matrixOffset->SetIdentity();

  // Set parameters
  Rcpp::NumericVector testvec( r_matrix );
  if ( testvec[0] == testvec[0])
    {
    Rcpp::NumericMatrix matrix( r_matrix );
    if ( (matrix.nrow() != dimension) || (matrix.ncol() != dimension) )
      {
      Rcpp::stop( "Matrix must of size dimension*dimension");
      }

    typename MatrixOffsetBaseType::MatrixType itkMatrix;
    for ( unsigned int i=0; i<dimension; i++)
      for ( unsigned int j=0; j<dimension; j++)
      {
        itkMatrix(i,j) = matrix(i,j);
      }

    matrixOffset->SetMatrix( itkMatrix );
    }

  Rcpp::NumericVector translation( r_translation );
  if ( translation[0] == translation[0] )
  {
    if ( translation.length() != dimension )
      {
      Rcpp::stop("Translation must be of length: dimension");
      }

    typename MatrixOffsetBaseType::OutputVectorType itkTranslation;
    for ( unsigned int i=0; i<dimension; i++)
      {
      itkTranslation[i] = translation[i];
      }

    matrixOffset->SetTranslation( itkTranslation );
  }

  Rcpp::NumericVector offset( r_offset );
  if ( offset[0] == offset[0] )
  {
    if ( offset.length() != dimension )
      {
      Rcpp::stop("Offset must be of length: dimension");
      }

    typename MatrixOffsetBaseType::OutputVectorType itkOffset;
    for ( unsigned int i=0; i<dimension; i++)
      {
      itkOffset[i] = offset[i];
      }

    matrixOffset->SetOffset( itkOffset );
  }

  Rcpp::NumericVector center( r_center );
  if ( center[0] == center[0] )
  {
    if ( center.length() != dimension )
      {
      Rcpp::stop("Center must be of length: dimension");
      }

    typename MatrixOffsetBaseType::InputPointType itkCenter;
    for ( unsigned int i=0; i<dimension; i++)
      {
      itkCenter[i] = center[i];
      }

    matrixOffset->SetCenter( itkCenter );
  }

  Rcpp::NumericVector parameters( r_parameters );
  if ( parameters[0] == parameters[0] )
  {
    if ( parameters.length() != matrixOffset->GetNumberOfParameters() )
      {
      Rcpp::stop("Parameters has incorrect length");
      }

    typename MatrixOffsetBaseType::ParametersType itkParameters;
    itkParameters.SetSize( matrixOffset->GetNumberOfParameters() );
    for ( unsigned int i=0; i<matrixOffset->GetNumberOfParameters(); i++)
      {
      itkParameters[i] = parameters[i];
      }

    matrixOffset->SetParameters( itkParameters );
  }

  Rcpp::NumericVector fixedparameters( r_fixedparameters );
  if ( fixedparameters[0] == fixedparameters[0] )
  {
    if ( fixedparameters.length() != matrixOffset->GetNumberOfFixedParameters() )
      {
      Rcpp::stop("fixed.parameters has incorrect length");
      }

    typename MatrixOffsetBaseType::FixedParametersType itkFixedParameters;
    itkFixedParameters.SetSize( matrixOffset->GetNumberOfFixedParameters() );
    for ( unsigned int i=0; i<matrixOffset->GetNumberOfFixedParameters(); i++)
      {
      itkFixedParameters[i] = fixedparameters[i];
      }

    matrixOffset->SetFixedParameters( itkFixedParameters );
  }


  TransformBasePointerType itkTransform = dynamic_cast<TransformBaseType*>( matrixOffset.GetPointer() );
  return Rcpp::wrap( itkTransform );
}

RcppExport SEXP antsrTransform_MatrixOffset( SEXP r_type, SEXP r_precision, SEXP r_dimension,
  SEXP r_matrix, SEXP r_offset, SEXP r_center, SEXP r_translation,
  SEXP r_parameters, SEXP r_fixedparameters )
{
try
{

  std::string precision = Rcpp::as< std::string >( r_precision );
  unsigned int dimension = Rcpp::as< int >( r_dimension );

  if ( (dimension < 2) || (dimension > 4) )
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
      return antsrTransform_MatrixOffset<PrecisionType,4>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_MatrixOffset<PrecisionType,3>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_MatrixOffset<PrecisionType,2>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_MatrixOffset<PrecisionType,4>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_MatrixOffset<PrecisionType,3>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_MatrixOffset<PrecisionType,2>( r_type, r_precision, r_dimension, r_matrix,
          r_offset, r_center, r_translation, r_parameters, r_fixedparameters );
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_GetParameters( SEXP r_transform )
{
  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer                   TransformPointerType;

  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );
  Rcpp::NumericVector parameters( itkTransform->GetNumberOfParameters() );

  for (unsigned int i=0; i<itkTransform->GetNumberOfParameters(); i++ )
  {
    parameters[i] = itkTransform->GetParameters()[i];
  }

  return parameters;
}


RcppExport SEXP antsrTransform_GetParameters( SEXP r_transform )
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
      return antsrTransform_GetParameters<PrecisionType,4>( r_transform  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_GetParameters<PrecisionType,3>( r_transform  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_GetParameters<PrecisionType,2>( r_transform );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_GetParameters<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_GetParameters<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_GetParameters<PrecisionType,2>( r_transform );
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
{
  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer                   TransformPointerType;

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

RcppExport SEXP antsrTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
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
      return antsrTransform_SetParameters<PrecisionType,4>( r_transform, r_parameters  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_SetParameters<PrecisionType,3>( r_transform, r_parameters  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_SetParameters<PrecisionType,2>( r_transform, r_parameters );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_SetParameters<PrecisionType,4>( r_transform, r_parameters );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_SetParameters<PrecisionType,3>( r_transform, r_parameters );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_SetParameters<PrecisionType,2>( r_transform, r_parameters );
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
template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_TransformPoint( SEXP r_transform, SEXP r_point )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
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


RcppExport SEXP antsrTransform_TransformPoint( SEXP r_transform, SEXP r_point )
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
      return antsrTransform_TransformPoint<PrecisionType,4>( r_transform, r_point  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformPoint<PrecisionType,3>( r_transform, r_point  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformPoint<PrecisionType,2>( r_transform, r_point );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_TransformPoint<PrecisionType,4>( r_transform, r_point );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformPoint<PrecisionType,3>( r_transform, r_point );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformPoint<PrecisionType,2>( r_transform, r_point );
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

// Apply transform to vector
template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_TransformVector( SEXP r_transform, SEXP r_vector )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer          TransformPointerType;
  typedef typename TransformType::InputVectorType   InputVectorType;
  typedef typename TransformType::OutputVectorType  OutputVectorType;


  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );
  Rcpp::NumericVector inVector( r_vector );

  InputVectorType inItkVector;
  for (unsigned int i=0; i<InputVectorType::Dimension; i++)
    {
    inItkVector[i] = inVector[i];
    }

  OutputVectorType outItkVector = itkTransform->TransformVector( inItkVector );

  Rcpp::NumericVector outVector( OutputVectorType::Dimension );
  for (unsigned int i=0; i<OutputVectorType::Dimension; i++)
    {
    outVector[i] = outItkVector[i];
    }

  return outVector;
}


RcppExport SEXP antsrTransform_TransformVector( SEXP r_transform, SEXP r_vector )
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
      return antsrTransform_TransformVector<PrecisionType,4>( r_transform, r_vector  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformVector<PrecisionType,3>( r_transform, r_vector  );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformVector<PrecisionType,2>( r_transform, r_vector );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_TransformVector<PrecisionType,4>( r_transform, r_vector );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformVector<PrecisionType,3>( r_transform, r_vector );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformVector<PrecisionType,2>( r_transform, r_vector );
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

// Apply transform to image
template< class TransformType, class PixelType >
SEXP antsrTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_interpolation )
{
  typedef typename TransformType::Pointer          TransformPointerType;

  const unsigned int Dimension = TransformType::InputSpaceDimension;

  Rcpp::S4 antsrTransform( r_transform );
  std::string type = Rcpp::as<std::string>( antsrTransform.slot("type") );

  TransformPointerType transform = Rcpp::as<TransformPointerType>( r_transform );

  typedef typename TransformType::ParametersValueType                   PrecisionType;

  typedef itk::Image<PixelType,TransformType::InputSpaceDimension> ImageType;
  typedef typename ImageType::Pointer                              ImagePointerType;

  // Use base for reference image so we can ignore it's pixeltype
  typedef itk::ImageBase<TransformType::InputSpaceDimension> ImageBaseType;
  typedef typename ImageBaseType::Pointer                    ImageBasePointerType;

  ImagePointerType inputImage = Rcpp::as<ImagePointerType>( r_image );
  ImageBasePointerType refImage = Rcpp::as<ImageBasePointerType>( r_ref );

  typedef itk::ResampleImageFilter<ImageType,ImageType,PrecisionType,PrecisionType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();

  std::string interp = Rcpp::as<std::string>( r_interpolation );

  typedef itk::InterpolateImageFunction<ImageType, PrecisionType> InterpolatorType;
  typename InterpolatorType::Pointer interpolator = ITK_NULLPTR;

  // adapted from make_interpolator_snip.tmpl
  if( interp == "linear" )
    {
    typedef itk::LinearInterpolateImageFunction<ImageType, PrecisionType> LinearInterpolatorType;
    typename LinearInterpolatorType::Pointer linearInterpolator = LinearInterpolatorType::New();
    interpolator = linearInterpolator;
    }
  else if( interp == "nearestneighbor" )
    {
    typedef itk::NearestNeighborInterpolateImageFunction<ImageType, PrecisionType> NearestNeighborInterpolatorType;
    typename NearestNeighborInterpolatorType::Pointer nearestNeighborInterpolator = NearestNeighborInterpolatorType::New();
    interpolator = nearestNeighborInterpolator;
    }
  else if( interp == "bspline" )
    {
    typedef itk::BSplineInterpolateImageFunction<ImageType, PrecisionType> BSplineInterpolatorType;
    typename BSplineInterpolatorType::Pointer bSplineInterpolator = BSplineInterpolatorType::New();
    interpolator = bSplineInterpolator;
    }
  else if( interp == "gaussian" )
    {
    typedef itk::GaussianInterpolateImageFunction<ImageType, PrecisionType> GaussianInterpolatorType;
    typename GaussianInterpolatorType::Pointer gaussianInterpolator = GaussianInterpolatorType::New();
    double sigma[Dimension];
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      sigma[d] = inputImage->GetSpacing()[d];
      }
    double alpha = 1.0;
    gaussianInterpolator->SetParameters( sigma, alpha );
    interpolator = gaussianInterpolator;
    }
  else if( interp ==  "CosineWindowedSinc" )
    {
    typedef itk::WindowedSincInterpolateImageFunction
                 <ImageType, 3, itk::Function::CosineWindowFunction<3, PrecisionType, PrecisionType>, itk::ConstantBoundaryCondition< ImageType >, PrecisionType> CosineInterpolatorType;
    typename CosineInterpolatorType::Pointer cosineInterpolator = CosineInterpolatorType::New();
    interpolator = cosineInterpolator;
    }
  else if( interp == "hammingwindowedsinc" )
    {
    typedef itk::WindowedSincInterpolateImageFunction
                 <ImageType, 3, itk::Function::HammingWindowFunction<3, PrecisionType, PrecisionType >, itk::ConstantBoundaryCondition< ImageType >, PrecisionType> HammingInterpolatorType;
    typename HammingInterpolatorType::Pointer hammingInterpolator = HammingInterpolatorType::New();
    interpolator = hammingInterpolator;
    }
  else if( interp == "lanczoswindowedsinc" )
    {
    typedef itk::WindowedSincInterpolateImageFunction
                 <ImageType, 3, itk::Function::LanczosWindowFunction<3, PrecisionType, PrecisionType>, itk::ConstantBoundaryCondition< ImageType >, PrecisionType > LanczosInterpolatorType;
    typename LanczosInterpolatorType::Pointer lanczosInterpolator = LanczosInterpolatorType::New();
    interpolator = lanczosInterpolator;
    }
  else if( interp == "blackmanwindowedsinc" )
    {
    typedef itk::WindowedSincInterpolateImageFunction
                 <ImageType, 3, itk::Function::BlackmanWindowFunction<3, PrecisionType, PrecisionType>, itk::ConstantBoundaryCondition< ImageType >, PrecisionType > BlackmanInterpolatorType;
    typename BlackmanInterpolatorType::Pointer blackmanInterpolator = BlackmanInterpolatorType::New();
    interpolator = blackmanInterpolator;
    }
  else if( interp == "welchwindowedsinc" )
    {
    typedef itk::WindowedSincInterpolateImageFunction
                 <ImageType, 3, itk::Function::WelchWindowFunction<3, PrecisionType, PrecisionType>, itk::ConstantBoundaryCondition< ImageType >, PrecisionType > WelchInterpolatorType;
    typename WelchInterpolatorType::Pointer welchInterpolator = WelchInterpolatorType::New();
    interpolator = welchInterpolator;
    }
  else if( interp == "multilabel" )
    {
    const unsigned int NVectorComponents = 1;
    typedef ants::VectorPixelCompare<PrecisionType, NVectorComponents> CompareType;
    typedef typename itk::LabelImageGaussianInterpolateImageFunction<ImageType, PrecisionType,
	    CompareType> MultiLabelInterpolatorType;
    typename MultiLabelInterpolatorType::Pointer multiLabelInterpolator = MultiLabelInterpolatorType::New();
    double sigma[Dimension];
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      sigma[d] = inputImage->GetSpacing()[d];
      }
    double alpha = 4.0;
    multiLabelInterpolator->SetParameters( sigma, alpha );
    interpolator = multiLabelInterpolator;
    }

   //sanity check thtat this function MUST return a valid interpolator
   if ( interpolator.IsNull() )
    {
    Rcpp::stop("Error:  Unrecognized interpolation option. ");
    }


  filter->SetInput( inputImage );
  filter->SetSize( refImage->GetLargestPossibleRegion().GetSize() );
  filter->SetOutputSpacing( refImage->GetSpacing() );
  filter->SetOutputOrigin( refImage->GetOrigin() );
  filter->SetOutputDirection( refImage->GetDirection() );
  filter->SetInterpolator( interpolator );

  filter->SetTransform( transform );
  filter->Update();

  return Rcpp::wrap<ImagePointerType>( filter->GetOutput() );
}

template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_interpolation )
{
  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  //return antsrTransform_TransformImage<TransformType>( r_transform, r_image, r_ref );

  Rcpp::S4 image( r_image );
  std::string pixeltype = Rcpp::as<std::string>(image.slot("pixeltype"));

  if ( pixeltype == "double" )
  {
    return antsrTransform_TransformImage<TransformType, double>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "float" )
  {
    return antsrTransform_TransformImage<TransformType, float>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "unsigned int" )
  {
    return antsrTransform_TransformImage<TransformType, unsigned int>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "unsigned char" )
  {
    return antsrTransform_TransformImage<TransformType, unsigned char>( r_transform, r_image, r_ref, r_interpolation );
  }
  else
  {
    Rcpp::stop("Unsupported pixeltype in antsImage");
  }

  return Rcpp::wrap(NA_REAL);

}


RcppExport SEXP antsrTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_iterpolation )
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
      return antsrTransform_TransformImage<PrecisionType,4>( r_transform, r_image, r_ref, r_iterpolation  );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformImage<PrecisionType,3>( r_transform, r_image, r_ref, r_iterpolation );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformImage<PrecisionType,2>( r_transform, r_image, r_ref, r_iterpolation );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_TransformImage<PrecisionType,4>( r_transform, r_image, r_ref, r_iterpolation );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_TransformImage<PrecisionType,3>( r_transform, r_image, r_ref, r_iterpolation );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_TransformImage<PrecisionType,2>( r_transform, r_image, r_ref, r_iterpolation );
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_Read( SEXP r_filename, SEXP r_precision )
{

  std::string filename = Rcpp::as<std::string>( r_filename );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformBaseType;
  typedef typename TransformBaseType::Pointer               TransformBasePointerType;
  typedef typename itk::CompositeTransform<PrecisionType, Dimension> CompositeTransformType;

  typedef itk::TransformFileReaderTemplate<PrecisionType> TransformReaderType;
  typedef typename TransformReaderType::TransformListType TransformListType;

  typename TransformReaderType::Pointer reader = TransformReaderType::New();
  reader->SetFileName( filename );
  reader->Update();

  const typename TransformReaderType::TransformListType * transformList = reader->GetTransformList();
  //Rcpp::Rcout << "Number of transforms = " << transformList->size() << std::endl;

  Rcpp::S4 antsrTransform( "antsrTransform" );
  antsrTransform.slot("dimension") = Dimension;
  antsrTransform.slot("precision") = Rcpp::as<std::string>( r_precision );

  TransformBasePointerType transform;

  if ( transformList->size() > 1 )
  {
    typename CompositeTransformType::Pointer comp_transform = CompositeTransformType::New();
    typedef typename TransformListType::const_iterator TransformIteratorType;
    for (TransformIteratorType i = transformList->begin(); i != transformList->end(); ++i)
    {
      comp_transform->AddTransform( dynamic_cast<TransformBaseType *>( i->GetPointer()) );
    }

    transform = dynamic_cast<TransformBaseType *>(comp_transform.GetPointer());
  }
  else
  {
    transform = dynamic_cast<TransformBaseType *>( transformList->front().GetPointer() );
  }

  std::string type = transform->GetNameOfClass();
  antsrTransform.slot("type") = type;

  TransformBasePointerType * rawPointer = new TransformBasePointerType( transform );
  Rcpp::XPtr<TransformBasePointerType> xptr( rawPointer, true );
  antsrTransform.slot("pointer") = xptr;

  return antsrTransform;
}

RcppExport SEXP antsrTransform_Read( SEXP r_filename, SEXP r_dimension, SEXP r_precision )
{
try
{
  unsigned int dimension = Rcpp::as<int>( r_dimension );
  std::string precision = Rcpp::as<std::string>( r_precision );

  if ( precision == "float")
  {
    typedef float PrecisionType;
    if ( dimension == 4 )
    {
      return antsrTransform_Read<PrecisionType,4>( r_filename, r_precision );
    }
    else if ( dimension == 3)
    {
      return antsrTransform_Read<PrecisionType,3>( r_filename, r_precision );
    }
    else if ( dimension == 2 )
    {
      return antsrTransform_Read<PrecisionType,2>( r_filename, r_precision );
    }
    else
    {
      Rcpp::stop( "Unsupported dimension" );
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_Compose( SEXP r_list, SEXP r_precision )
{
  Rcpp::S4 antsrTransform( "antsrTransform" );
  antsrTransform.slot("dimension") = Dimension;
  antsrTransform.slot("precision") = Rcpp::as<std::string>( r_precision );

  Rcpp::List transforms( r_list );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformBaseType;
  typedef typename TransformBaseType::Pointer               TransformBasePointerType;
  typedef typename itk::CompositeTransform<PrecisionType, Dimension> CompositeTransformType;

  typename CompositeTransformType::Pointer comp_transform = CompositeTransformType::New();

  for ( unsigned int i=0; i<transforms.size(); i++ )
    {
    TransformBasePointerType t = Rcpp::as<TransformBasePointerType>( transforms[i] );
    comp_transform->AddTransform( t );
    Rcpp::S4 tran(transforms[i]);

    //Rcpp::Rcout << "Adding transform: " << Rcpp::as<std::string>(tran.slot("type")) << std::endl;
    }

  TransformBasePointerType transform = dynamic_cast<TransformBaseType *>(comp_transform.GetPointer());

  std::string type = comp_transform->GetNameOfClass();
  antsrTransform.slot("type") = type;

  TransformBasePointerType * rawPointer = new TransformBasePointerType( transform );
  Rcpp::XPtr<TransformBasePointerType> xptr( rawPointer, true );
  antsrTransform.slot("pointer") = xptr;

  return antsrTransform;
}

RcppExport SEXP antsrTransform_Compose( SEXP r_list, SEXP r_dimension, SEXP r_precision )
{
try
{
  unsigned int dimension = Rcpp::as<int>( r_dimension );
  std::string precision = Rcpp::as<std::string>( r_precision );

  if ( precision == "float")
  {
    typedef float PrecisionType;
    if ( dimension == 4 )
    {
      return antsrTransform_Compose<PrecisionType,4>( r_list, r_precision );
    }
    else if ( dimension == 3)
    {
      return antsrTransform_Compose<PrecisionType,3>( r_list, r_precision );
    }
    else if ( dimension == 2 )
    {
      return antsrTransform_Compose<PrecisionType,2>( r_list, r_precision );
    }
    else
    {
      Rcpp::stop( "Unsupported dimension" );
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_FromDisplacementField( SEXP r_field, std::string precision )
{

  typedef itk::Transform<PrecisionType,Dimension,Dimension>                  TransformType;
  typedef typename TransformType::Pointer                                    TransformPointerType;
  typedef typename itk::DisplacementFieldTransform<PrecisionType, Dimension> DisplacementFieldTransformType;
  typedef typename DisplacementFieldTransformType::DisplacementFieldType     DisplacementFieldType;
  typedef typename DisplacementFieldType::PixelType                          VectorType;

  // Displacement field is an itk::Image with vector pixels, while in ANTsR we use the
  // itk::VectorImage class for multichannel data. So we must copy the field
  // and pass it to the transform
  typedef itk::VectorImage<PrecisionType, Dimension> AntsrFieldType;
  typedef typename AntsrFieldType::Pointer           AntsrFieldPointerType;

  AntsrFieldPointerType antsrField = Rcpp::as<AntsrFieldPointerType>( r_field );
  typename DisplacementFieldType::Pointer itkField = DisplacementFieldType::New();
  itkField->SetRegions( antsrField->GetLargestPossibleRegion() );
  itkField->SetSpacing( antsrField->GetSpacing() );
  itkField->SetOrigin( antsrField->GetOrigin() );
  itkField->SetDirection( antsrField->GetDirection() );
  itkField->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<DisplacementFieldType> IteratorType;
  IteratorType it( itkField, itkField->GetLargestPossibleRegion() );
  while ( !it.IsAtEnd() )
  {
    typename AntsrFieldType::PixelType vec = antsrField->GetPixel( it.GetIndex() );
    VectorType dvec;
    for ( unsigned int i=0; i<Dimension; i++)
      {
      dvec[i] = vec[i];
      }
    itkField->SetPixel(it.GetIndex(), dvec);
    ++it;
  }

  typename DisplacementFieldTransformType::Pointer displacementTransform =
    DisplacementFieldTransformType::New();
  displacementTransform->SetDisplacementField( itkField );

  TransformPointerType transform = dynamic_cast<TransformType *>( displacementTransform.GetPointer() );

  Rcpp::S4 antsrTransform( "antsrTransform" );
  antsrTransform.slot("dimension") = Dimension;
  antsrTransform.slot("precision") = precision;
  std::string type = displacementTransform->GetNameOfClass();
  antsrTransform.slot("type") = type;
  TransformPointerType * rawPointer = new TransformPointerType( transform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );
  antsrTransform.slot("pointer") = xptr;

  return antsrTransform;
}

RcppExport SEXP antsrTransform_FromDisplacementField( SEXP r_field )
{
try
{
  Rcpp::S4 antsImage( r_field );
  unsigned int dimension = Rcpp::as<int>( antsImage.slot("dimension"));
  unsigned int components = Rcpp::as<int>( antsImage.slot("components"));
  std::string precision = Rcpp::as<std::string>( antsImage.slot("pixeltype"));

  if ( (precision != "float") && (precision != "double") )
  {
    Rcpp::stop("Field must have pixeltype of either float or double");
  }

  if ( components != dimension )
  {
    Rcpp::stop("Field must have number of pixel compenents equal to image dimension");
  }

  if ( precision == "float")
  {
    typedef float PrecisionType;
    if ( dimension == 4 )
    {
      return antsrTransform_FromDisplacementField<PrecisionType,4>( r_field, precision );
    }
    else if ( dimension == 3)
    {
      return antsrTransform_FromDisplacementField<PrecisionType,3>( r_field, precision );
    }
    else if ( dimension == 2 )
    {
      return antsrTransform_FromDisplacementField<PrecisionType,2>( r_field, precision );
    }
    else
    {
      Rcpp::stop( "Unsupported dimension" );
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
template< class PrecisionType, unsigned int Dimension >
SEXP antsrTransform_Inverse( SEXP r_transform )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer          TransformPointerType;
  TransformPointerType itkTransform = Rcpp::as<TransformPointerType>( r_transform );

  if ( !itkTransform->IsLinear() )
  {
    Rcpp::stop("Only linear transforms may be inverted with this method");
  }

  TransformPointerType inverse = itkTransform->GetInverseTransform();
  return Rcpp::wrap(inverse);

}


RcppExport SEXP antsrTransform_Inverse( SEXP r_transform )
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
      return antsrTransform_Inverse<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_Inverse<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_Inverse<PrecisionType,2>( r_transform );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsrTransform_Inverse<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsrTransform_Inverse<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsrTransform_Inverse<PrecisionType,2>( r_transform );
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
