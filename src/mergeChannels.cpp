#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"

template< class ImageType, class VectorImageType >
SEXP mergeChannels( Rcpp::List imageList )
{
  typedef typename ImageType::Pointer       ImagePointerType;
  typedef typename VectorImageType::Pointer VectorImagePointerType;

  unsigned int nImages = imageList.size();

  std::vector<ImagePointerType> images;
  for ( unsigned int i=0; i<nImages; i++)
    {
    images.push_back( Rcpp::as<ImagePointerType>( imageList[i] ) );
    }

  VectorImagePointerType vectorImage = VectorImageType::New();
  vectorImage->SetRegions( images[0]->GetLargestPossibleRegion() );
  vectorImage->SetSpacing( images[0]->GetSpacing() );
  vectorImage->SetOrigin( images[0]->GetOrigin() );
  vectorImage->SetDirection( images[0]->GetDirection() );
  vectorImage->SetNumberOfComponentsPerPixel( nImages );
  vectorImage->Allocate();

  // Fill image data
  itk::ImageRegionIteratorWithIndex<VectorImageType> it( vectorImage,
    vectorImage->GetLargestPossibleRegion() );

  while (!it.IsAtEnd() )
    {
    typename VectorImageType::PixelType pix;
    pix.SetSize( nImages );
    for (unsigned int i=0; i<nImages; i++)
      {
      pix[i] = images[i]->GetPixel(it.GetIndex());
      }
    vectorImage->SetPixel(it.GetIndex(), pix);
    ++it;
    }

  return Rcpp::wrap( vectorImage );
}


RcppExport SEXP mergeChannels( SEXP r_imageList )
{
try
{
  Rcpp::List imageList( r_imageList );
  unsigned int nImages = imageList.size();

  SEXP r_antsimage = imageList[0];
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
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
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
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
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
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
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
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      typedef itk::VectorImage<PixelType,dim> VectorImageType;
      return mergeChannels<ImageType,VectorImageType>( imageList );
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
