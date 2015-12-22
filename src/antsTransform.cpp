
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "antsUtilities.h"
#include "itkAffineTransform.h"
#include "itkTranslationTransform.h"
#include "itkResampleImageFilter.h"
#include "itkTransformFileReader.h"
#include "itkCompositeTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkConstantBoundaryCondition.h"

#include "itkBSplineInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGaussianInterpolateImageFunction.h"
#include "itkInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkLabelImageGaussianInterpolateImageFunction.h"



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


template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_GetParameters( SEXP r_transform )
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


template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_SetParameters( SEXP r_transform, SEXP r_parameters )
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
template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_TransformPoint( SEXP r_transform, SEXP r_point )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer          TransformPointerType;
  typedef typename TransformType::InputPointType   InputPointType;
  typedef typename TransformType::OutputPointType  OutputPointType;

  typedef itk::CompositeTransform<PrecisionType, TransformType::InputSpaceDimension > CompositeTransformType;
  typedef typename CompositeTransformType::Pointer          CompositeTransformPointerType;

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

// Apply transform to vector
template< class PrecisionType, unsigned int Dimension >
SEXP antsTransform_TransformVector( SEXP r_transform, SEXP r_vector )
{

  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  typedef typename TransformType::Pointer          TransformPointerType;
  typedef typename TransformType::InputVectorType   InputVectorType;
  typedef typename TransformType::OutputVectorType  OutputVectorType;

  typedef itk::CompositeTransform<PrecisionType, TransformType::InputSpaceDimension > CompositeTransformType;
  typedef typename CompositeTransformType::Pointer          CompositeTransformPointerType;

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


RcppExport SEXP antsTransform_TransformVector( SEXP r_transform, SEXP r_vector )
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
      return antsTransform_TransformVector<PrecisionType,4>( r_transform, r_vector  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformVector<PrecisionType,3>( r_transform, r_vector  );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformVector<PrecisionType,2>( r_transform, r_vector );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_TransformVector<PrecisionType,4>( r_transform, r_vector );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformVector<PrecisionType,3>( r_transform, r_vector );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformVector<PrecisionType,2>( r_transform, r_vector );
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
SEXP antsTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_interpolation )
{
  typedef typename TransformType::Pointer          TransformPointerType;
  typedef typename TransformType::InputPointType   InputPointType;
  typedef typename TransformType::OutputPointType  OutputPointType;

  const unsigned int Dimension = TransformType::InputSpaceDimension;

  Rcpp::S4 antsTransform( r_transform );
  std::string type = Rcpp::as<std::string>( antsTransform.slot("type") );

  TransformPointerType transform = Rcpp::as<TransformPointerType>( r_transform );

  typedef typename TransformType::ParametersValueType                   PrecisionType;

  typedef itk::CompositeTransform<PrecisionType, TransformType::InputSpaceDimension > CompositeTransformType;
  typedef typename CompositeTransformType::Pointer          CompositeTransformPointerType;

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
SEXP antsTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_interpolation )
{
  Rcpp::S4 transform( r_transform );
  std::string type = Rcpp::as<std::string>( transform.slot("type") );

  typedef itk::Transform<PrecisionType,Dimension,Dimension> TransformType;
  //return antsTransform_TransformImage<TransformType>( r_transform, r_image, r_ref );

  Rcpp::S4 image( r_image );
  std::string pixeltype = Rcpp::as<std::string>(image.slot("pixeltype"));

  if ( pixeltype == "double" )
  {
    return antsTransform_TransformImage<TransformType, double>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "float" )
  {
    return antsTransform_TransformImage<TransformType, float>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "unsigned int" )
  {
    return antsTransform_TransformImage<TransformType, unsigned int>( r_transform, r_image, r_ref, r_interpolation );
  }
  else if ( pixeltype == "unsigned char" )
  {
    return antsTransform_TransformImage<TransformType, unsigned char>( r_transform, r_image, r_ref, r_interpolation );
  }
  else
  {
    Rcpp::stop("Unsupported pixeltype in antsImage");
  }

  return Rcpp::wrap(NA_REAL);

}


RcppExport SEXP antsTransform_TransformImage( SEXP r_transform, SEXP r_image, SEXP r_ref, SEXP r_iterpolation )
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
      return antsTransform_TransformImage<PrecisionType,4>( r_transform, r_image, r_ref, r_iterpolation  );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformImage<PrecisionType,3>( r_transform, r_image, r_ref, r_iterpolation );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformImage<PrecisionType,2>( r_transform, r_image, r_ref, r_iterpolation );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_TransformImage<PrecisionType,4>( r_transform, r_image, r_ref, r_iterpolation );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_TransformImage<PrecisionType,3>( r_transform, r_image, r_ref, r_iterpolation );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_TransformImage<PrecisionType,2>( r_transform, r_image, r_ref, r_iterpolation );
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
SEXP antsTransform_Read( SEXP r_filename, SEXP r_precision )
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

  Rcpp::S4 antsTransform( "antsTransform" );
  antsTransform.slot("dimension") = Dimension;
  antsTransform.slot("precision") = Rcpp::as<std::string>( r_precision );

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
  antsTransform.slot("type") = type;

  TransformBasePointerType * rawPointer = new TransformBasePointerType( transform );
  Rcpp::XPtr<TransformBasePointerType> xptr( rawPointer, true );
  antsTransform.slot("pointer") = xptr;

  return antsTransform;
}

RcppExport SEXP antsTransform_Read( SEXP r_filename, SEXP r_dimension, SEXP r_precision )
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
      return antsTransform_Read<PrecisionType,4>( r_filename, r_precision );
    }
    else if ( dimension == 3)
    {
      return antsTransform_Read<PrecisionType,3>( r_filename, r_precision );
    }
    else if ( dimension == 2 )
    {
      return antsTransform_Read<PrecisionType,2>( r_filename, r_precision );
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
SEXP antsTransform_Compose( SEXP r_list, SEXP r_precision )
{
  Rcpp::S4 antsTransform( "antsTransform" );
  antsTransform.slot("dimension") = Dimension;
  antsTransform.slot("precision") = Rcpp::as<std::string>( r_precision );

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
  antsTransform.slot("type") = type;

  TransformBasePointerType * rawPointer = new TransformBasePointerType( transform );
  Rcpp::XPtr<TransformBasePointerType> xptr( rawPointer, true );
  antsTransform.slot("pointer") = xptr;

  return antsTransform;
}

RcppExport SEXP antsTransform_Compose( SEXP r_list, SEXP r_dimension, SEXP r_precision )
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
      return antsTransform_Compose<PrecisionType,4>( r_list, r_precision );
    }
    else if ( dimension == 3)
    {
      return antsTransform_Compose<PrecisionType,3>( r_list, r_precision );
    }
    else if ( dimension == 2 )
    {
      return antsTransform_Compose<PrecisionType,2>( r_list, r_precision );
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
SEXP antsTransform_FromDisplacementField( SEXP r_field, std::string precision )
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

  Rcpp::S4 antsTransform( "antsTransform" );
  antsTransform.slot("dimension") = Dimension;
  antsTransform.slot("precision") = precision;
  std::string type = displacementTransform->GetNameOfClass();
  antsTransform.slot("type") = type;
  TransformPointerType * rawPointer = new TransformPointerType( transform );
  Rcpp::XPtr<TransformPointerType> xptr( rawPointer, true );
  antsTransform.slot("pointer") = xptr;

  return antsTransform;
}

RcppExport SEXP antsTransform_FromDisplacementField( SEXP r_field )
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
      return antsTransform_FromDisplacementField<PrecisionType,4>( r_field, precision );
    }
    else if ( dimension == 3)
    {
      return antsTransform_FromDisplacementField<PrecisionType,3>( r_field, precision );
    }
    else if ( dimension == 2 )
    {
      return antsTransform_FromDisplacementField<PrecisionType,2>( r_field, precision );
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
SEXP antsTransform_Inverse( SEXP r_transform )
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


RcppExport SEXP antsTransform_Inverse( SEXP r_transform )
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
      return antsTransform_Inverse<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_Inverse<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_Inverse<PrecisionType,2>( r_transform );
	    }
	  }
  else if( precision == "float" )
    {
    typedef float PrecisionType;
    if( dimension == 4 )
	    {
      return antsTransform_Inverse<PrecisionType,4>( r_transform );
      }
    else if( dimension == 3 )
	    {
      return antsTransform_Inverse<PrecisionType,3>( r_transform );
	    }
    else if( dimension == 2 )
	    {
      return antsTransform_Inverse<PrecisionType,2>( r_transform );
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
