#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkAffineTransform.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageMomentsCalculator.h"
#include "itkTransformFileWriter.h"

template< class ImageType >
SEXP reflectionMatrix( SEXP r_image, unsigned int axis, std::string filename )
{
  typedef typename ImageType::Pointer       ImagePointerType;
  typedef itk::AffineTransform<double, ImageType::ImageDimension> AffineTransformType;

  ImagePointerType image = Rcpp::as<ImagePointerType>(r_image);

  typedef typename itk::ImageMomentsCalculator<ImageType> ImageCalculatorType;
  typename ImageCalculatorType::Pointer calculator = ImageCalculatorType::New();
  calculator->SetImage( image );

  typename ImageCalculatorType::VectorType fixed_center;
  fixed_center.Fill(0);

  calculator->Compute();
  fixed_center = calculator->GetCenterOfGravity();

  typename AffineTransformType::Pointer aff = AffineTransformType::New();
  aff->SetIdentity();

  typename AffineTransformType::ParametersType myoff = aff->GetFixedParameters();
  for( unsigned int i = 0; i < ImageType::ImageDimension; i++ )
    {
    myoff[i] = fixed_center[i];
    }
  typename AffineTransformType::MatrixType mymat = aff->GetMatrix();
  if( axis < ImageType::ImageDimension )
    {
    mymat[axis][axis] = ( -1.0 );
    }

  aff->SetFixedParameters( myoff );
  aff->SetMatrix( mymat );

  typedef itk::TransformFileWriter TransformWriterType;
  typename TransformWriterType::Pointer transformWriter = TransformWriterType::New();
  transformWriter->SetInput( aff );
  transformWriter->SetFileName( filename.c_str() );
  transformWriter->Update();

  return Rcpp::wrap( 1 );
}


RcppExport SEXP reflectionMatrix( SEXP r_image, SEXP r_axis, SEXP r_filename )
{
try
{

  unsigned int axis = Rcpp::as<unsigned int>( r_axis );
  std::string filename = Rcpp::as<std::string>( r_filename );

  Rcpp::S4 image( r_image );
  int dimension = Rcpp::as< int >( image.slot( "dimension" ) );
  std::string pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return reflectionMatrix<ImageType>( r_image, axis, filename );
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else
    {
    Rcpp::stop( "Unsupported pixeltype");
    }
}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  Rcpp::Rcout << "STD ExceptionObject caught !" << std::endl;
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
