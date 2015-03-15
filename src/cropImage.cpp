#include <algorithm>
#include <stdio.h>
#include <Rcpp.h>
#include "itkCastImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkPasteImageFilter.h"
#include <string>
#include <vector>


template< class ImageType >
typename ImageType::Pointer cropImageHelper(
  typename ImageType::Pointer image,
  typename ImageType::Pointer labimage,
  unsigned int whichLabel  )
{
  enum { Dimension = ImageType::ImageDimension };
  typename ImageType::RegionType region;
  typedef typename ImageType::Pointer ImagePointerType;
  if( image.IsNotNull() & labimage.IsNotNull() )
    {
    typedef itk::Image<unsigned short, Dimension>      ShortImageType;
    typedef itk::CastImageFilter<ImageType, ShortImageType> CasterType;
    typename CasterType::Pointer caster = CasterType::New();
    caster->SetInput( labimage );
    caster->Update();
    typedef itk::LabelStatisticsImageFilter<ShortImageType, ShortImageType>
      StatsFilterType;
    typename StatsFilterType::Pointer stats = StatsFilterType::New();
    stats->SetLabelInput( caster->GetOutput() );
    stats->SetInput( caster->GetOutput() );
    stats->Update();
    region = stats->GetRegion( whichLabel );
    typedef itk::ExtractImageFilter<ImageType, ImageType> CropperType;
    typename CropperType::Pointer cropper = CropperType::New();
    cropper->SetInput( image );
    cropper->SetExtractionRegion( region );
    cropper->SetDirectionCollapseToSubmatrix();
    cropper->Update();
    cropper->GetOutput()->SetSpacing( image->GetSpacing() );
    return cropper->GetOutput();
    }
  return NULL;
}


template< class ImageType >
typename ImageType::Pointer cropIndHelper(
  typename ImageType::Pointer image,
  SEXP r_loind, SEXP r_upind )
{
  enum { Dimension = ImageType::ImageDimension };
  typedef typename ImageType::Pointer ImagePointerType;
  Rcpp::NumericVector lindv( r_loind ) ;
  Rcpp::NumericVector uindv( r_upind ) ;
  if( lindv.size() != Dimension || uindv.size() != Dimension )
    {
    Rcpp::Rcout << "indices do not match the image in dimensions" << std::endl;
    return NULL;
    }
  typename ImageType::RegionType region;
  typename ImageType::RegionType::SizeType size;
  typename ImageType::IndexType loind;
  typename ImageType::IndexType upind;
  typename ImageType::IndexType index;
  for( int i = 0 ; i < Dimension; ++i )
    {
    loind[i] = lindv[i] - 1;
    upind[i] = uindv[i] - 1; // R uses a different indexing, by 1 instead of 0
    if ( upind[i] > loind[i] )
      {
      size[i] = upind[i] - loind[i] + 1;
      index[i] = loind[i];
      }
    else
      {
      size[i] = loind[i] - upind[i] + 1;
      index[i] = upind[i];
      }
    }
  if( image.IsNotNull() )
    {
    region.SetSize( size );
    region.SetIndex( index );
    typedef itk::ExtractImageFilter<ImageType, ImageType> CropperType;
    typename CropperType::Pointer cropper = CropperType::New();
    cropper->SetInput( image );
    cropper->SetExtractionRegion( region );
    cropper->SetDirectionCollapseToSubmatrix();
    cropper->Update();
    cropper->GetOutput()->SetSpacing( image->GetSpacing() );
    return cropper->GetOutput();
    }
  return NULL;
}

template< class ImageType >
typename ImageType::Pointer decropImageHelper(
  typename ImageType::Pointer cimage,
  typename ImageType::Pointer fimage )
{
  enum { Dimension = ImageType::ImageDimension };
  typename ImageType::RegionType region;
  typedef typename ImageType::Pointer ImagePointerType;
  if( cimage.IsNotNull() & fimage.IsNotNull() )
    {
    typedef itk::PasteImageFilter <ImageType, ImageType >
      PasteImageFilterType;
    // The SetDestinationIndex() method prescribes where in the first
    // input to start pasting data from the second input.
    // The SetSourceRegion method prescribes the section of the second
    // image to paste into the first.
    typename ImageType::IndexType destinationIndex =
      cimage->GetLargestPossibleRegion().GetIndex();
    typename PasteImageFilterType::Pointer pasteFilter
      = PasteImageFilterType::New ();
    pasteFilter->SetSourceImage(cimage);
    pasteFilter->SetDestinationImage(fimage);
    pasteFilter->SetSourceRegion(cimage->GetLargestPossibleRegion());
    pasteFilter->SetDestinationIndex(destinationIndex);
    pasteFilter->Update();
    return pasteFilter->GetOutput();
    }
  return NULL;
}

RcppExport SEXP cropImage( SEXP r_in_image1 ,
  SEXP r_in_image2,  SEXP r_label, SEXP r_decrop,
  SEXP r_loind, SEXP r_upind  )
{
  if( r_in_image1 == NULL  )
    {
    Rcpp::Rcout << " Invalid Arguments: pass at least 1 image in " << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  unsigned int label = 1;
  if ( r_label != NULL  )
    label=Rcpp::as< unsigned int >( r_label );
  unsigned int decrop = 0;
  if ( r_decrop != NULL  )
    decrop=Rcpp::as< unsigned int >( r_decrop );
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

  // make new out image, result of cropping
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

    ImagePointerType* out_image_ptr_ptr = NULL;

    if ( decrop == 0 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2, label )
          );
    else if ( decrop == 1 )
      out_image_ptr_ptr =
        new ImagePointerType(
          decropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2 )
          );
    else if ( decrop == 2 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropIndHelper<ImageType>(
            *antsimage_xptr1, r_loind, r_upind  )
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

    ImagePointerType* out_image_ptr_ptr = NULL;

    if ( decrop == 0 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2, label )
          );
    else if ( decrop == 1 )
      out_image_ptr_ptr =
        new ImagePointerType(
          decropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2 )
          );
    else if ( decrop == 2 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropIndHelper<ImageType>(
            *antsimage_xptr1, r_loind, r_upind  )
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

    ImagePointerType* out_image_ptr_ptr = NULL;

    if ( decrop == 0 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2, label )
          );
    else if ( decrop == 1 )
      out_image_ptr_ptr =
        new ImagePointerType(
          decropImageHelper<ImageType>(
            *antsimage_xptr1,*antsimage_xptr2 )
          );
    else if ( decrop == 2 )
      out_image_ptr_ptr =
        new ImagePointerType(
          cropIndHelper<ImageType>(
            *antsimage_xptr1, r_loind, r_upind  )
          );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return out_image;
}




template< class ImageType, class ImageDM1Type >
typename ImageDM1Type::Pointer extractSliceHelper(
  typename ImageType::Pointer image,
  SEXP r_slice, SEXP r_direction )
{
enum { ImageDimension = ImageType::ImageDimension };
typedef itk::Image< float, ImageDimension - 1> SliceType;
typename ImageType::RegionType region;
typename ImageType::RegionType::SizeType size =
  image->GetLargestPossibleRegion().GetSize();
size[ Rcpp::as< unsigned int >( r_direction )  ] = 0;
typename ImageType::IndexType index;
index.Fill( 0 );
index[ Rcpp::as< unsigned int >( r_direction ) ] =
  Rcpp::as< unsigned int >( r_slice );
region.SetIndex( index );
region.SetSize( size );

typedef itk::ExtractImageFilter<ImageType, SliceType> ExtracterType;
typename ExtracterType::Pointer extracter = ExtracterType::New();
extracter->SetInput( image );
extracter->SetExtractionRegion( region );
extracter->SetDirectionCollapseToIdentity();
extracter->Update();
return extracter->GetOutput();
}


RcppExport SEXP extractSlice( SEXP r_in_image1,
  SEXP r_slice, SEXP r_direction  )
{
  if( r_in_image1 == NULL  )
    {
    Rcpp::Rcout << " Invalid Arguments: pass at least 1 image in " << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  Rcpp::S4 in_image1( r_in_image1 ) ;
  std::string in_pixeltype = Rcpp::as< std::string >(
    in_image1.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >(
    in_image1.slot( "dimension" ) ) ;
  // make new out image, result of cropping
  Rcpp::S4 out_image( std::string( "antsImage" ) ) ;
  out_image.slot( "pixeltype" ) = in_pixeltype ;
  out_image.slot( "dimension" ) = dimension-1 ;

  if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType;
    typedef itk::Image< float , 2 > ImageDM1Type;
    typedef ImageType::Pointer ImagePointerType;
    typedef ImageDM1Type::Pointer ImageDM1PointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
        static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    ImageDM1PointerType* out_image_ptr_ptr = NULL;
    out_image_ptr_ptr =
          new ImageDM1PointerType(
            extractSliceHelper<ImageType, ImageDM1Type>(
              *antsimage_xptr1, r_slice, r_direction )
            );
    Rcpp::XPtr< ImageDM1PointerType >
        out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType;
    typedef itk::Image< float , 3 > ImageDM1Type;
    typedef ImageType::Pointer ImagePointerType;
    typedef ImageDM1Type::Pointer ImageDM1PointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
        static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    ImageDM1PointerType* out_image_ptr_ptr = NULL;
    out_image_ptr_ptr =
            new ImageDM1PointerType(
              extractSliceHelper<ImageType, ImageDM1Type>(
                *antsimage_xptr1, r_slice, r_direction )
              );
    Rcpp::XPtr< ImageDM1PointerType >
          out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return out_image;
}
