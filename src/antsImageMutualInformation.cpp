
#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>

#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"


template< unsigned int Dimension >
double antsImageMIHelper(
  typename itk::Image< float , Dimension >::Pointer image1,
  typename itk::Image< float , Dimension >::Pointer image2 )
{
  typedef itk::Image< float , Dimension > ImageType;
  if( image1.IsNotNull() & image2.IsNotNull() )
  {
    double mi = 1;
    typedef itk::MattesMutualInformationImageToImageMetricv4
      <ImageType, ImageType, ImageType> MetricType;
    unsigned int bins = 32;
    typename MetricType::Pointer metric = MetricType::New();
    metric->SetFixedImage( image1 );
    metric->SetMovingImage( image2 );
    metric->SetNumberOfHistogramBins( bins );
    metric->Initialize();
    mi = metric->GetValue();
    return mi;
  }
  else
  {
    return 1;
  }
}


RcppExport SEXP antsImageMutualInformation( SEXP r_in_image1 ,
  SEXP   r_in_image2  )
{
  if( r_in_image1 == NULL || r_in_image2 == NULL )
    {
      Rcpp::Rcout << "Invalid Arguments: pass 2 images in " << std::endl ;
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
    Rcpp::Rcout << "Images must have equivalent dimensionality & pixel type" << std::endl ;
    Rcpp::wrap( 1 );
  }

  double mivalue = 1;
  if ( dimension == 2 )
    {
    typedef itk::Image< float , 2 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr2(
      static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    mivalue = antsImageMIHelper<2>(*antsimage_xptr1,*antsimage_xptr2);
    }
  else if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType3;
    typedef ImageType3::Pointer ImagePointerType3;
    Rcpp::XPtr< ImagePointerType3 > antsimage_xptr1_3(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType3 > antsimage_xptr2_3(
    static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    mivalue = antsImageMIHelper<3>(*antsimage_xptr1_3,*antsimage_xptr2_3);
    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType4;
    typedef ImageType4::Pointer ImagePointerType4;
    Rcpp::XPtr< ImagePointerType4 > antsimage_xptr1_4(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType4 > antsimage_xptr2_4(
    static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    mivalue = antsImageMIHelper<4>(*antsimage_xptr1_4,*antsimage_xptr2_4);
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return Rcpp::wrap( mivalue );
}
