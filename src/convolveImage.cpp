#include <algorithm>
#include <stdio.h>
#include "itkCastImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkConvolutionImageFilter.h"
#include <string>
#include <vector>
#include <Rcpp.h>


template< class ImageType >
typename ImageType::Pointer convolveImageHelper(
  typename ImageType::Pointer image,
  typename ImageType::Pointer kernel )
{
  enum { Dimension = ImageType::ImageDimension };
  typedef typename ImageType::Pointer ImagePointerType;
  if( image.IsNotNull() & kernel.IsNotNull() )
    {
    typedef itk::ConvolutionImageFilter<ImageType> FilterType;
    typename FilterType::Pointer convolutionFilter = FilterType::New();
    convolutionFilter->SetInput( image );
    convolutionFilter->SetKernelImage( kernel );
    convolutionFilter->Update();
    return convolutionFilter->GetOutput();
    }
  return NULL;
}

// [[myRcpp::export]]
RcppExport SEXP itkConvolveImage( SEXP r_in_image1 ,
  SEXP r_in_image2  )
{
  if( r_in_image1 == NULL || r_in_image2 == NULL  )
    {
    Rcpp::Rcout << " Invalid Arguments: convolveImage.cpp " << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  Rcpp::S4 in_image1( r_in_image1 ) ;
  Rcpp::S4 in_image2( r_in_image2 ) ;
  std::string in_pixeltype = Rcpp::as< std::string >(
    in_image1.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >(
    in_image1.slot( "dimension" ) ) ;
  std::string in_pixeltype2 = Rcpp::as< std::string >(
    in_image2.slot( "pixeltype" ) ) ;
  unsigned int dimension2 = Rcpp::as< unsigned int >(
    in_image2.slot( "dimension" ) ) ;
  if (  ( dimension != dimension2 ) ||
        ( in_pixeltype.compare(in_pixeltype2) != 0 ) )
  {
    Rcpp::Rcout << " Images must have equivalent dimensionality & pixel type" << std::endl ;
    Rcpp::wrap( 1 );
  }

  // make new out image, result of convolution
  Rcpp::S4 out_image( std::string( "antsImage" ) ) ;
  out_image.slot( "pixeltype" ) = in_pixeltype ;
  out_image.slot( "dimension" ) = dimension ;

  if ( dimension == 2 )
    {
    typedef itk::Image< float , 2 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr2(
      static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;

    ImagePointerType* out_image_ptr_ptr =
      new ImagePointerType(
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
        );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
  else if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr2(
      static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;

    ImagePointerType* out_image_ptr_ptr =
      new ImagePointerType(
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
        );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;

    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr2(
      static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;

    ImagePointerType* out_image_ptr_ptr =
      new ImagePointerType(
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
        );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return out_image;
}
