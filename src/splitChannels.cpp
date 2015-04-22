#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"

template< class VectorImageType, class ImageType >
SEXP splitChannels( SEXP r_antsimage )
{
  typedef typename ImageType::Pointer       ImagePointerType;
  typedef typename VectorImageType::Pointer VectorImagePointerType;

  VectorImagePointerType input = Rcpp::as<VectorImagePointerType>( r_antsimage);
  unsigned int nComponents = input->GetNumberOfComponentsPerPixel();

  // Create output images
  std::vector<ImagePointerType> images;
  for ( unsigned int i=0; i<nComponents; i++)
    {
    ImagePointerType image = ImageType::New();
    image->SetRegions( input->GetLargestPossibleRegion() );
    image->SetSpacing( input->GetSpacing() );
    image->SetOrigin( input->GetOrigin() );
    image->SetDirection( input->GetDirection() );
    image->Allocate();
    images.push_back( image );
    }

  // Fill image data
  itk::ImageRegionIteratorWithIndex<VectorImageType> it( input,
    input->GetLargestPossibleRegion() );

  while (!it.IsAtEnd() )
    {
    typename VectorImageType::PixelType pix = input->GetPixel(it.GetIndex());

    for (unsigned int i=0; i<nComponents; i++)
      {
      images[i]->SetPixel(it.GetIndex(), pix[i]);
      }
    ++it;
    }

  Rcpp::List outputList( nComponents );
  for (unsigned int i=0; i<nComponents; i++)
    {
    Rcpp::S4 img( Rcpp::wrap(images[i]) );
    outputList[i] = img;
    }

  return( Rcpp::wrap(outputList) );

}


RcppExport SEXP splitChannels( SEXP r_antsimage )
{
try
{
  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
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
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
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
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
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
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return splitChannels<VectorImageType,ImageType>( r_antsimage );
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
