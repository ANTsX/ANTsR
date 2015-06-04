#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"

template< class ImageType >
SEXP makeImage( Rcpp::NumericVector size, Rcpp::NumericVector spacing,
                Rcpp::NumericVector origin, Rcpp::NumericMatrix direction,
                unsigned int components )
{
  typedef typename ImageType::Pointer       ImagePointerType;

  typename ImageType::RegionType region;

  for ( unsigned int i=0; i<size.size(); i++)
    {
    region.SetSize(i, size[i]);
    }

  typename ImageType::SpacingType   itk_spacing;
  typename ImageType::PointType     itk_origin;
  typename ImageType::DirectionType itk_direction;

  for ( unsigned int i=0; i<ImageType::ImageDimension; i++)
    {
    itk_spacing[i] = spacing[i];
    itk_origin[i] = origin[i];
    for ( unsigned int j=0; j<ImageType::ImageDimension; j++)
      {
      itk_direction(i,j) = direction(i,j);
      }
    }

  ImagePointerType image = ImageType::New();
  image->SetRegions( region );
  image->SetNumberOfComponentsPerPixel( components );
  image->SetSpacing( itk_spacing );
  image->SetOrigin( itk_origin );
  image->SetDirection( itk_direction );
  image->Allocate();
  image->FillBuffer(0);

  return Rcpp::wrap( image );
}


RcppExport SEXP makeImage( SEXP r_pixeltype, SEXP r_size, SEXP r_spacing, SEXP r_origin, SEXP r_direction, SEXP r_components )
{
try
{

  Rcpp::NumericVector size(r_size);
  Rcpp::NumericVector spacing(r_spacing);
  Rcpp::NumericVector origin(r_origin);
  Rcpp::NumericMatrix direction(r_direction);
  unsigned int components = Rcpp::as<unsigned int>( r_components );
  std::string pixeltype = Rcpp::as<std::string>( r_pixeltype );

  unsigned int dimension = size.size();

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
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
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
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
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
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
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return makeImage<ImageType>( size, spacing, origin, direction, components );
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
