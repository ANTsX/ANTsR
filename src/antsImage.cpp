
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkAddImageFilter.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkMultiplyImageFilter.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageBase.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkContinuousIndex.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/algo/vnl_determinant.h"

template< class ImageType >
SEXP antsImage( std::string pixeltype, unsigned int components )
{
  unsigned int nDim = ImageType::ImageDimension;
  Rcpp::S4 image_r(std::string( "antsImage"));
  image_r.slot("pixeltype") = pixeltype;
  image_r.slot("dimension") = nDim;
  image_r.slot("components") = components;

  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType* ptr_ptr_image = new ImagePointerType;
  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true );
  image_r.slot("pointer") = xptr;

  return image_r;
}

RcppExport SEXP antsImage( SEXP r_pixeltype , SEXP r_dimension, SEXP r_components )
{
try
{
  if( r_pixeltype == NULL || r_dimension == NULL || r_components == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  unsigned int dimension = Rcpp::as< int >( r_dimension ) ;
  unsigned int components = Rcpp::as< int >( r_components );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  if( pixeltype == "double" )
    {
    typedef double ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
	    typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
	    typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
	    typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
	    typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage<ImageType>( pixeltype, components ) :
        antsImage<VectorImageType>( pixeltype, components );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

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

template< typename ImageType >
typename ImageType::Pointer antsImage_Arith(
  typename ImageType::Pointer image1,
  typename ImageType::Pointer image2,
  std::string arithOp  )
{
  if ( ! image1.IsNotNull() || ! image2.IsNotNull()  )
    {
    return NULL;
    }
  if ( arithOp.compare("+") == 0 )
    {
    typedef itk::AddImageFilter<ImageType, ImageType, ImageType> ArithType;
    typename ArithType::Pointer arither = ArithType::New();
    arither->SetInput1( image1 );
    arither->SetInput2( image2 );
    arither->Update();
    return arither->GetOutput();
    }
  else if ( arithOp.compare("*") == 0 )
    {
    typedef itk::MultiplyImageFilter<ImageType, ImageType, ImageType> ArithType;
    typename ArithType::Pointer arither = ArithType::New();
    arither->SetInput1( image1 );
    arither->SetInput2( image2 );
    arither->Update();
    return arither->GetOutput();
    }
  return NULL;
}


RcppExport SEXP antsImageArith( SEXP r_in_image1 ,
  SEXP r_in_image2,  SEXP r_op  )
try
{
  if( r_in_image1 == NULL || r_in_image2 == NULL  )
    {
    Rcpp::Rcout << " Invalid Arguments: pass 2 images in " << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  std::string in_op = Rcpp::as< std::string >( r_op ) ;
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

  // make new out image, result of arith
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
        antsImage_Arith<ImageType>(
          *antsimage_xptr1, *antsimage_xptr2, in_op )
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
        antsImage_Arith<ImageType>(
          *antsimage_xptr1, *antsimage_xptr2, in_op )
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
        antsImage_Arith<ImageType>(
          *antsimage_xptr1, *antsimage_xptr2, in_op )
        );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return out_image;
}
catch( const std::exception& exc )
    {
      Rcpp::Rcout<< exc.what() << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }


template< typename ImageType >
Rcpp::IntegerVector antsImage_isna( SEXP r_antsimage )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );

  if ( ! image.IsNotNull() )
    {
    return Rcpp::wrap( 1 );
    }
  return Rcpp::wrap( 0 );
}

RcppExport SEXP antsImage_isna( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if ( dimension == 2 )
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_isna<ImageType>( r_antsimage );
    }
  else if ( dimension == 3 )
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_isna<ImageType>( r_antsimage );
    }
  else if ( dimension == 4 )
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_isna<ImageType>( r_antsimage );
    }
  else
    {
    Rcpp::wrap(1);
    }
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

return Rcpp::wrap(NA_REAL); // not reached
}




template< typename ImageType >
Rcpp::IntegerVector antsImage_dim( typename ImageType::Pointer image )
{
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  Rcpp::IntegerVector dim_r( ImageType::ImageDimension ) ;
  for( unsigned int i = 0 ; i < ImageType::ImageDimension ; ++i )
    {
    dim_r[i] = image->GetLargestPossibleRegion().GetSize(i) ;
    }

  return dim_r ;
}

RcppExport SEXP antsImage_dim( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if ( dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    typedef ImageType::Pointer     ImagePointerType;
    return antsImage_dim<ImageType>( Rcpp::as<ImagePointerType>( r_antsimage ) );
    }
  else if ( dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    typedef ImageType::Pointer     ImagePointerType;
    return antsImage_dim<ImageType>( Rcpp::as<ImagePointerType>( r_antsimage ) );
    }
  else if ( dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    typedef ImageType::Pointer     ImagePointerType;
    return antsImage_dim<ImageType>( Rcpp::as<ImagePointerType>( r_antsimage ) );
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }
}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}


template< class ImageType >
SEXP antsImage_asVector( SEXP r_image, SEXP r_mask, SEXP r_antsregion )
{
  typedef typename  ImageType::PixelType       PixelType;
  typedef typename ImageType::Pointer         ImagePointerType;
  const unsigned int Dimension = ImageType::ImageDimension;

  typedef itk::DefaultConvertPixelTraits<PixelType> PixelConvertType;

  ImagePointerType image = Rcpp::as<ImagePointerType>(r_image);
  const unsigned int Components = image->GetNumberOfComponentsPerPixel();

  if( image.IsNotNull() )
    {
    typename ImageType::RegionType region;
    Rcpp::S4 antsregion( r_antsregion );
    Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) );
    Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) );

    if( indexvector.size() == 0 && sizevector.size() == 0 )
	    {
	    region = image->GetLargestPossibleRegion();
	    }
    else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	    {
	    Rcpp::stop("antsRegion provided has dimensions incompatible with the image");
	    }
    else
	    {
	    typename ImageType::IndexType index ;
	    typename ImageType::SizeType size ;
	    for( unsigned int i = 0 ; i < Dimension ; ++i )
	      {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	      }
	    region.SetSize( size ) ;
	    region.SetIndex( index ) ;
	    if( !image->GetLargestPossibleRegion().IsInside( region ) )
	      {
	      Rcpp::stop("'region' is not inside the image");
	      }
	    }

    itk::ImageRegionConstIterator< ImageType > image_iter( image , region );

    Rcpp::LogicalVector mask( r_mask );
    if( mask.size() == 0 )
	    {
      unsigned long nValues = region.GetNumberOfPixels() * Components;
	    Rcpp::NumericVector vector_r( nValues );

	    unsigned int vector_r_ind = 0;
	    for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	      {
        typename ImageType::PixelType pix = image_iter.Get();
        for ( unsigned int n=0; n<Components; n++)
          {
          vector_r[vector_r_ind++] = PixelConvertType::GetNthComponent(n, pix);
          }
	      }

	    // set dimensions of the R vector;
	    // dim[0] = num-of-rows, dim[1] = num-of-cols, dim[2] = num-of-slices, dim[3] = num-of-time
      unsigned int arrayDim = Dimension;
      if (Components > 1)
        {
        ++arrayDim;
        }
	    Rcpp::IntegerVector dims( arrayDim );

      if (Components > 1)
        {
        dims[0] = Components;
        for( unsigned int i = 0; i < Dimension; ++i )
          {
        	dims[i+1] = region.GetSize(i);
        	}
        }
      else
        {
        for( unsigned int i = 0; i < Dimension; ++i )
          {
          dims[i] = region.GetSize(i);
          }
        }

	    vector_r.attr( "dim" ) = dims;
	    return vector_r;
	    }
    else
	    {
	    int numberofpixelspertime = region.GetSize(0);
	    for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
	      {
	      numberofpixelspertime *= region.GetSize(i);
	      }

	    if( mask.size() != numberofpixelspertime && mask.size() != (int)region.GetNumberOfPixels() )
	      {
	      Rcpp::stop("Length of mask vector does not match image-region dimensions");
	      }

	    Rcpp::LogicalVector::iterator mask_iter = mask.begin();

	    // set the length of the R vector to be number of logical TRUEs in the mask provided
	    Rcpp::NumericVector vector_r( Components * std::count( mask.begin() , mask.end() , TRUE ) *
					( mask.size() == numberofpixelspertime ? region.GetSize(Dimension-1) : 1 ) );

	    unsigned int vector_r_ind = 0 ;
	    for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
	      {
	      // in case mask only covers one time index, reuse the mask for every time index
	      if( mask_iter == mask.end() )
          {
		      mask_iter = mask.begin();
          }
	      if( *mask_iter == TRUE )
          {
          typename ImageType::PixelType pix = image_iter.Get();
          for ( unsigned int n=0; n<Components; n++)
            {
            vector_r[vector_r_ind++] = PixelConvertType::GetNthComponent(n, pix);
            }
          }
	      }
      if (Components > 1)
        {
        Rcpp::IntegerVector dims(2);
        dims[0] = Components;
        dims[1] = vector_r.size() / Components;
        vector_r.attr( "dim" ) = dims;
        }
	    return vector_r;
	    }
    }
  else
    {
    Rcpp::stop("Invalid input image");
    }
  return Rcpp::wrap(NA_REAL); // not reached
}

RcppExport SEXP antsImage_asVector( SEXP r_antsimage , SEXP r_mask , SEXP r_antsregion )
{
try
{
  if( r_antsimage == NULL || r_mask == NULL || r_antsregion == NULL )
    {
    Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
    }

  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );
  unsigned int components = Rcpp::as< int >( antsimage.slot( "components") );

  if( pixeltype == "double" )
    {
    typedef double ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
 	    }
    else if( dimension == 3 )
	    {
      typedef itk::Image<ValueType,3>       ImageType;
      typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else if( dimension == 2 )
	    {
      typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
	    }
    }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
 	    }
    else if( dimension == 3 )
	    {
      typedef itk::Image<ValueType,3>       ImageType;
      typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else if( dimension == 2 )
	    {
      typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
 	    }
    else if( dimension == 3 )
	    {
      typedef itk::Image<ValueType,3>       ImageType;
      typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else if( dimension == 2 )
	    {
      typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      typedef itk::VectorImage<ValueType,4> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
 	    }
    else if( dimension == 3 )
	    {
      typedef itk::Image<ValueType,3>       ImageType;
      typedef itk::VectorImage<ValueType,3> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else if( dimension == 2 )
	    {
      typedef itk::Image<ValueType,2>       ImageType;
      typedef itk::VectorImage<ValueType,2> VectorImageType;

      return (components==1) ?
        antsImage_asVector<ImageType>( r_antsimage, r_mask, r_antsregion ) :
        antsImage_asVector<VectorImageType>( r_antsimage, r_mask, r_antsregion );
      }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }
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

template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetNeighborhoodMatrix( typename itk::Image< PixelType , Dimension >::Pointer image ,
                                      typename itk::Image< PixelType , Dimension >::Pointer mask,
                                      SEXP r_radius, SEXP r_physical, SEXP r_boundary, SEXP r_spatial, SEXP r_gradient )
{

  typedef double                           RealType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef typename ImageType::RegionType   RegionType;
  typedef typename ImageType::IndexType    IndexType;
  typedef typename ImageType::PointType    PointType;
  typedef itk::CentralDifferenceImageFunction< ImageType,
                                           RealType >
                                           GradientCalculatorType;
  typedef itk::CovariantVector<RealType, Dimension> CovariantVectorType;

  Rcpp::NumericVector radius( r_radius ) ;
  int physical = Rcpp::as<int>( r_physical );
  int boundary = Rcpp::as<int>( r_boundary );
  int spatial = Rcpp::as<int>( r_spatial );
  int getgradient = Rcpp::as<int>( r_gradient );

  typename itk::NeighborhoodIterator<ImageType>::SizeType nSize;

  unsigned long maxSize = 1;
  for ( unsigned int i=0; i<Dimension; i++ )
    {
    maxSize *= ( 1 + 2*radius[i] );
    nSize[i] = radius[i];
    }

  std::vector<double> pixelList;
  pixelList.reserve(maxSize);

  itk::ImageRegionIteratorWithIndex<ImageType> it( mask, mask->GetLargestPossibleRegion() ) ;
  itk::NeighborhoodIterator<ImageType> nit( nSize, image, image->GetLargestPossibleRegion() ) ;

  unsigned long nVoxels = 0;
  while( !it.IsAtEnd() )
    {
    if ( it.Value() > 0 )
      {
      ++nVoxels;
      }
    ++it;
    }

  Rcpp::NumericMatrix matrix(maxSize, nVoxels);
  if ( ( ! spatial )  && ( ! getgradient ) )
    {
      unsigned int col = 0;
      it.GoToBegin();
      while( !it.IsAtEnd() )
        {
        if ( it.Value() > 1.e-6 ) // use epsilon instead of zero
          {
          double mean = 0;
          double count = 0;
          for ( unsigned int row=0; row < nit.Size(); row++ )
            {
            IndexType idx = it.GetIndex() + nit.GetOffset(row);

            // check boundary conditions
            if ( mask->GetRequestedRegion().IsInside(idx) )
              {
              if ( mask->GetPixel(idx) > 0 ) // fully within boundaries
                {
                matrix(row,col) = nit.GetPixel(row);
                mean += nit.GetPixel(row);
                ++count;
                }
              else
                {
                if ( boundary == 1 )
                  {
                  matrix(row,col)  = nit.GetPixel(row);
                  }
                else
                  {
                  matrix(row,col) = NA_REAL;
                  }
                }
              }
            else
              {
              matrix(row,col) = NA_REAL;
              }
            }

          if ( boundary == 2 )
            {
            mean /= count;
            for ( unsigned int row=0; row < nit.Size(); row++ )
              {
              if ( matrix(row,col) != matrix(row,col) )
                {
                matrix(row,col) = mean;
                }
              }
            }

          ++col;
          }
        ++it;
        ++nit;
        }
    return ( matrix );
    }

  if ( ( ! spatial )  && ( getgradient ) )
  {
    typename GradientCalculatorType::Pointer
      imageGradientCalculator = GradientCalculatorType::New();

    imageGradientCalculator->SetInputImage( image );
    // this will hold spatial locations of pixels or voxels
    Rcpp::NumericMatrix gradients( Dimension, nVoxels );
    unsigned int col = 0;
    it.GoToBegin();
    while( !it.IsAtEnd() )
      {
      if ( it.Value() > 1.e-6 ) // use epsilon instead of zero
        {
        double mean = 0;
        double count = 0;
        for ( unsigned int row=0; row < nit.Size(); row++ )
          {
          IndexType idx = it.GetIndex() + nit.GetOffset(row);

          // check boundary conditions
          if ( mask->GetRequestedRegion().IsInside(idx) )
            {
            if ( mask->GetPixel(idx) > 0 ) // fully within boundaries
              {
              matrix(row,col) = nit.GetPixel(row);
              mean += nit.GetPixel(row);
              ++count;
              if ( row == 0 )
                {
                CovariantVectorType gradient =
                  imageGradientCalculator->EvaluateAtIndex( it.GetIndex() );
                for ( unsigned int dd = 0; dd < Dimension; dd++ )
                  gradients( dd , col ) = gradient[ dd ];
                }
              }
            else
              {
              if ( boundary == 1 )
                {
                matrix(row,col)  = nit.GetPixel(row);
                }
              else
                {
                matrix(row,col) = NA_REAL;
                }
              }
            }
          else
            {
            matrix(row,col) = NA_REAL;
            }
          }

        if ( boundary == 2 )
          {
          mean /= count;
          for ( unsigned int row=0; row < nit.Size(); row++ )
            {
            if ( matrix(row,col) != matrix(row,col) )
              {
              matrix(row,col) = mean;
              }
            }
          }

        ++col;
        }
      ++it;
      ++nit;
      }
  return Rcpp::List::create( Rcpp::Named("values") = matrix,
                             Rcpp::Named("gradients") = gradients );
  }
// if spatial and gradient, then just use spatial - no gradient ...

  // this will hold spatial locations of pixels or voxels
  Rcpp::NumericMatrix indices(nVoxels, Dimension);
  // Get relative offsets of neighborhood locations
  Rcpp::NumericMatrix offsets(nit.Size(), Dimension);
  for ( unsigned int i=0; i < nit.Size(); i++ )
    {
    for ( unsigned int j=0; j<Dimension; j++)
      {
      offsets(i,j) = nit.GetOffset(i)[j];
      if ( physical )
        {
        offsets(i,j) = offsets(i,j) * image->GetSpacing()[j];
        }
      }
    }


  unsigned int col = 0;
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if ( it.Value() > 0 )
      {
      PointType pt;

      if ( physical )
        {
        image->TransformIndexToPhysicalPoint(it.GetIndex(), pt);
        }

      for ( unsigned int i=0; i < Dimension; i++)
        {
        if ( physical )
          {
          indices(col,i) = pt[i];
          }
        else
          {
          indices(col,i) = it.GetIndex()[i];
          }
        }

      double mean = 0;
      double count = 0;
      for ( unsigned int row=0; row < nit.Size(); row++ )
        {
        IndexType idx = it.GetIndex() + nit.GetOffset(row);

        // check boundary conditions
        if ( mask->GetRequestedRegion().IsInside(idx) )
          {
          if ( mask->GetPixel(idx) > 0 ) // fully within boundaries
            {
            matrix(row,col) = nit.GetPixel(row);
            mean += nit.GetPixel(row);
            ++count;
            }
          else
            {
            if ( boundary == 1 )
              {
              matrix(row,col)  = nit.GetPixel(row);
              }
            else
              {
              matrix(row,col) = NA_REAL;
              }
            }
          }
        else
          {
          matrix(row,col) = NA_REAL;
          }
        }

      if ( boundary == 2 )
        {
        mean /= count;
        for ( unsigned int row=0; row < nit.Size(); row++ )
          {
          if ( matrix(row,col) != matrix(row,col) )
            {
            matrix(row,col) = mean;
            }
          }
        }

      ++col;
      }
    ++it;
    ++nit;
    }
  return Rcpp::List::create( Rcpp::Named("values") = matrix,
                               Rcpp::Named("indices") = indices,
                               Rcpp::Named("offsets") = offsets );
}

RcppExport SEXP antsImage_GetNeighborhoodMatrix( SEXP r_antsimage, SEXP r_maskimage, SEXP r_radius,
                                                 SEXP r_physical, SEXP r_boundary, SEXP r_spatial, SEXP r_gradient )
try
{
  if ( r_antsimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  if ( r_maskimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  Rcpp::S4 maskimage( r_maskimage ) ;

  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  Rcpp::NumericVector radius( r_radius ) ;

  if ( radius.size() != dimension )
    {
    Rcpp::Rcout << "Radius must have same dimension as image" << std::endl ;
    return Rcpp::wrap( NA_REAL );
    }

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 4>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 3>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      Rcpp::XPtr< ImagePointerType > itkMask( static_cast< SEXP >( maskimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhoodMatrix<PixelType, 2>( *itkImage, *itkMask, r_radius, r_physical, r_boundary, r_spatial, r_gradient );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }

  else
    {
    Rcpp::Rcout << "Unsupported pixel type: " << pixeltype << std::endl;
    return Rcpp::wrap( NA_REAL );
    }
}
catch( const std::exception& exc )
{
  Rcpp::Rcout<< exc.what() << std::endl ;
  return Rcpp::wrap( 1 ) ;
}


template< class PixelType , unsigned int Dimension >
SEXP antsImage_GetNeighborhood( typename itk::Image< PixelType , Dimension >::Pointer image ,
                                SEXP r_index, SEXP r_kernel, SEXP r_radius, SEXP physical )
{

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef typename ImageType::RegionType   RegionType;
  typedef typename ImageType::IndexType    IndexType;

  Rcpp::NumericVector kernel( r_kernel );
  Rcpp::NumericVector radius( r_radius );
  Rcpp::NumericVector index( r_index );
  int physicalFlag = Rcpp::as<int>( physical );

  unsigned long maxSize = 0;
  std::vector<int> offsets;
  for ( unsigned int i=0; i<kernel.size(); i++ )
    {
    if ( kernel[i] > 1.e-6 ) // use epsilon instead of zero
      {
      offsets.push_back(i);
      ++maxSize;
      }
    }

  Rcpp::NumericVector pixels(maxSize);
  std::vector<IndexType> indexList;
  indexList.reserve(maxSize);

  RegionType region;
  typename itk::NeighborhoodIterator<ImageType>::SizeType nRadius;

  for ( unsigned int i=0; i<Dimension; i++ )
    {
    nRadius[i] = radius[i];
    region.SetSize(i, 1);
    region.SetIndex(i, index[i]-1); // R-to-ITK index conversion
    }

  RegionType imageSize = image->GetLargestPossibleRegion();
  itk::NeighborhoodIterator<ImageType> nit( nRadius, image, region );

  unsigned int idx = 0;
  for (unsigned int i = 0; i < offsets.size(); i++ )
    {
    //Rcpp::Rcout << nit.GetIndex(i) << ":" << offsets[i] << "=" << kernel[offsets[i]] << std::endl;
    if ( kernel[offsets[i]] > 1e-6 )
      {
      if ( imageSize.IsInside( nit.GetIndex(offsets[i]) ) )
        {
        pixels[idx] = nit.GetPixel(offsets[i]);
        }
      else
        {
        pixels[idx] = NA_REAL;
        }
      indexList.push_back( nit.GetIndex(offsets[i]) );
      ++idx;
      }
    }

  Rcpp::NumericMatrix indices( pixels.size(), Dimension );
  for ( unsigned int i=0; i<pixels.size(); i++)
    {
    typename ImageType::PointType pt;
    if ( physicalFlag )
      {
      image->TransformIndexToPhysicalPoint( indexList[i], pt );
      }

    for ( unsigned int j=0; j<Dimension; j++)
      {
      if ( !physicalFlag )
        {
        indices(i,j) = indexList[i][j] + 1; // ITK-to-R index conversion
        }
      else
        {
        indices(i,j) = pt[j];
        }
      }
    }

  return Rcpp::List::create( Rcpp::Named("values") = pixels,
                             Rcpp::Named("indices") = indices );

}

RcppExport SEXP antsImage_GetNeighborhood( SEXP r_antsimage, SEXP r_index,
                                           SEXP r_kernel, SEXP r_radius,
                                           SEXP physical )
{
try
{
  if ( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  Rcpp::NumericVector kernel( r_kernel );
  Rcpp::NumericVector index( r_index );
  Rcpp::NumericVector radius( r_radius );

  if ( radius.size() != dimension )
    {
    Rcpp::stop("Radius must have same dimension as image");
    }
  if ( index.size() != dimension )
    {
    Rcpp::stop("Index must have same dimension as image");
    }

  unsigned long maxSize = 1;
  for ( unsigned int i=0; i<dimension; i++ )
    {
    maxSize *= ( 1 + 2*radius[i] );
    }

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if( dimension == 4 )
      {
      typedef itk::Image<PixelType,4>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 4>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 3 )
      {
      typedef itk::Image<PixelType,3>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 3>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else if( dimension == 2 )
      {
      typedef itk::Image<PixelType,2>::Pointer ImagePointerType;
      Rcpp::XPtr< ImagePointerType > itkImage( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
      return antsImage_GetNeighborhood<PixelType, 2>( *itkImage, r_index, r_kernel, r_radius, physical );
      }
    else
      {
      Rcpp::Rcout << "Unsupported image dimnesion: " << dimension << std::endl;
      return Rcpp::wrap( NA_REAL );
      }
    }

  else
    {
    Rcpp::Rcout << "Unsupported pixel type: " << pixeltype << std::endl;
    return Rcpp::wrap( NA_REAL );
    }
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


/*
 * antsImage_GetPixels
 * r_antsimage - pointer to antsImage
 * r_indices - list of indices to examine. one list item for each dimension
 */
template<class ImageType>
SEXP antsImage_GetPixels( SEXP r_antsimage, SEXP r_indices )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>(r_antsimage);
  unsigned int nComponents = image->GetNumberOfComponentsPerPixel();

  typedef typename ImageType::PixelType             PixelType;
  typedef itk::DefaultConvertPixelTraits<PixelType> PixelConvertType;

  const unsigned int Dimension = ImageType::ImageDimension;

  if( image.IsNotNull() )
    {
    Rcpp::List list_indices( r_indices );
    std::vector< std::vector< double > > indices;

    if( list_indices.size() != Dimension )
      {
	    Rcpp::stop("indices do not match the image in dimensions");
	    }

    for( int i = 0 ; i < list_indices.size() ; ++i )
	    {
	    indices.push_back( Rcpp::as< std::vector< double > >( list_indices[i] ) ) ;
	    }

    unsigned int nValues = 1;
    for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	    // if no coordinates are provided for a dimension, assume entire extent of the dimension
	    if( indices[i].size() == 0 )
	      {
	      indices[i].reserve( image->GetLargestPossibleRegion().GetSize(i) );

        for( unsigned int j = 0; j < image->GetLargestPossibleRegion().GetSize(i); ++j )
		      {
		      indices[i][j] = j;
		      }
	      }
	    nValues *= indices[i].size();
	    }

    //Rcpp::NumericVector vector_r( vector_r_size );
    Rcpp::NumericMatrix values( nComponents, nValues );

    std::vector< unsigned int > ind( Dimension );

    for( unsigned int i = 0; i < nValues; ++i )
	    {
      typename ImageType::IndexType index;
	    for( unsigned int j = 0; j < Dimension; ++j )
	      {
	      index[j] = indices[ j ][ ind[j] ] - 1;
	      }
	    if( !image->GetLargestPossibleRegion().IsInside( index ) )
	      {
	      Rcpp::stop("index not inside the image");
	      }

      PixelType pix = image->GetPixel(index);

      if ( nComponents == 1 )
        {
        //unsigned int channel = rcpp_indices(v, rcpp_indices.ncol()-1);
        values(0,i) = PixelConvertType::GetNthComponent( 0, pix );
        }
      else
        {
        for ( unsigned int n=0; n<nComponents; n++)
          {
          values(n,i) = PixelConvertType::GetNthComponent( n, pix );
          }
        }

      ++ind[0];
      for( unsigned int j = 0; j < Dimension - 1; ++j )
        {
        if( ind[j] == indices[j].size() )
          {
          ++ind[j+1];
          ind[j] = 0;
          }
        }

	    }

    // set dimensions of the R vector using number of coordinates given for each dimension;
    // dim[0] = x-dimension, dim[1] = y-dimension, dim[2] = z-dimension, dim[3] = t-dimension
    if ( nComponents == 1)
      {
      Rcpp::IntegerVector dims( Dimension );
      for( unsigned int i = 0 ; i < Dimension ; ++i )
	      {
	      dims[i] = indices[i].size();
	      }
      values.attr( "dim" ) = dims;
      }
    else
      {
      Rcpp::IntegerVector dims( Dimension+1 );
      dims[0] = nComponents;
      for( unsigned int i = 1 ; i <= Dimension ; ++i )
    	  {
    	  dims[i] = indices[i-1].size();
    	  }
      values.attr( "dim" ) = dims;
      }
      return values;
    }
  else
    {
    Rcpp::stop("Empty image");
    }

  return Rcpp::wrap(NA_REAL); // this is never reached
}

RcppExport SEXP antsImage_GetPixels( SEXP r_antsimage , SEXP r_indices )
{
try
{
  if( r_antsimage == NULL || r_indices == NULL )
    {
    Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );
  unsigned int components = Rcpp::as< int >( antsimage.slot( "components" ) );

  if( pixeltype == "double" )
    {
	  typedef double PixelType ;
    if( dimension == 4 )
  	  {
  	  typedef itk::Image< PixelType,4>      ImageType;
      typedef itk::VectorImage<PixelType,4> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else if( dimension == 3 )
	    {
  	  typedef itk::Image<PixelType,3>       ImageType;
      typedef itk::VectorImage<PixelType,3> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
      }
    else if( dimension == 2 )
	    {
  	  typedef itk::Image<PixelType,2>       ImageType;
      typedef itk::VectorImage<PixelType,2> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
    	}
    }
  else if( pixeltype == "float" )
    {
	  typedef float PixelType ;
    if( dimension == 4 )
  	  {
  	  typedef itk::Image< PixelType,4>      ImageType;
      typedef itk::VectorImage<PixelType,4> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else if( dimension == 3 )
	    {
  	  typedef itk::Image<PixelType,3>       ImageType;
      typedef itk::VectorImage<PixelType,3> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
      }
    else if( dimension == 2 )
	    {
  	  typedef itk::Image<PixelType,2>       ImageType;
      typedef itk::VectorImage<PixelType,2> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
    	}
    }
  else if( pixeltype == "unsigned int" )
    {
	  typedef unsigned int PixelType ;
    if( dimension == 4 )
  	  {
  	  typedef itk::Image< PixelType,4>      ImageType;
      typedef itk::VectorImage<PixelType,4> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else if( dimension == 3 )
	    {
  	  typedef itk::Image<PixelType,3>       ImageType;
      typedef itk::VectorImage<PixelType,3> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
      }
    else if( dimension == 2 )
	    {
  	  typedef itk::Image<PixelType,2>       ImageType;
      typedef itk::VectorImage<PixelType,2> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
    	}
    }
  else if( pixeltype == "unsigned char" )
    {
	  typedef unsigned char PixelType ;
    if( dimension == 4 )
  	  {
  	  typedef itk::Image< PixelType,4>      ImageType;
      typedef itk::VectorImage<PixelType,4> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else if( dimension == 3 )
	    {
  	  typedef itk::Image<PixelType,3>       ImageType;
      typedef itk::VectorImage<PixelType,3> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
      }
    else if( dimension == 2 )
	    {
  	  typedef itk::Image<PixelType,2>       ImageType;
      typedef itk::VectorImage<PixelType,2> VectorImageType;
      return (components==1) ?
        antsImage_GetPixels<ImageType>( r_antsimage, r_indices ) :
        antsImage_GetPixels<VectorImageType>( r_antsimage, r_indices );
	    }
    else
	    {
	    Rcpp::stop("Unsupported Dimension");
    	}
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }
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




template< class PixelType , unsigned int Dimension >
int antsImage_SetPixels( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_coordinates , SEXP r_value )
{
  typedef itk::Image< PixelType , Dimension > ImageType;
  typedef typename ImageType::Pointer ImagePointerType;
  std::string errorMessage = "";

  if( image.IsNotNull() )
    {
    Rcpp::List list_coordinates( r_coordinates );
    std::vector< std::vector< double > > coordinates;

    if( list_coordinates.size() != Dimension )
	    {
	    errorMessage = "indices do not match the image in dimensions";
	    return 1;
 	    }

    for( int i = 0; i < list_coordinates.size(); ++i )
	    {
	    coordinates.push_back( Rcpp::as< std::vector< double > >( list_coordinates[i] ) );
	    }

    unsigned int value_size = 1 ;
    for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	    if( coordinates[i].size() == 0 )
	      {
	      coordinates[i].reserve( image->GetLargestPossibleRegion().GetSize(i) ) ;
	      for( unsigned int j = 0 ; j < image->GetLargestPossibleRegion().GetSize(i) ; ++j )
		      {
		      coordinates[i].push_back( j ) ;
		      }
	       }
	     value_size *= coordinates[i].size() ;
	    }
    Rcpp::NumericVector value( r_value ) ;
    if( value.size() != (int)value_size && value.size() != 1 )
	    {
	    errorMessage = "rhs vector must be scalar or of same length as indices";
	    return 2;
	    }

    std::vector< unsigned int > ind( Dimension ) ;
    typename ImageType::IndexType index ;
    if( value.size() == 1 )
	    {
	    for( unsigned int i = 0 ; i < value_size ; ++i )
	      {
	      for( unsigned int j = 0 ; j < Dimension ; ++j )
		      {
		      index[j] = coordinates[ j ][ ind[j] ]-1 ;
		      }
        //Rcpp::Rcout << index << std::endl;
        if ( image->GetLargestPossibleRegion().IsInside(index) )
          {
          image->SetPixel( index , value[0] );
          }
        else
          {
          errorMessage = "Index is outside of image space";
          return 3;
          }
	      ++ind[0];

	      for( unsigned int j = 0 ; j < Dimension-1 ; ++j )
		      {
		      if( ind[j] == coordinates[j].size() )
		        {
		        ++ind[j+1] ;
		        ind[j] = 0 ;
		        }
		      }
	      }
	    }
    else
	    {
	    for( unsigned int i = 0 ; i < value_size ; ++i )
	      {
	      for( unsigned int j = 0 ; j < Dimension ; ++j )
		      {
		      index[j] = coordinates[ j ][ ind[j] ]-1 ;
		      }
	      image->SetPixel( index , value[i] ) ;
	      ++ind[0] ;
	      for( unsigned int j = 0 ; j < Dimension - 1 ; ++j )
		      {
		      if( ind[j] == coordinates[j].size() )
		        {
		        ++ind[j+1] ;
		        ind[j] = 0 ;
		        }
		      }
	      }
	    }
    }
  else
    {
    Rcpp::Rcout << "Unallocated image" << std::endl ;
    return 4;
    }
  return 0;
}

RcppExport SEXP antsImage_SetPixels( SEXP r_antsimage , SEXP r_coordinates , SEXP r_value )
try
{
  int returnFlag = 0;

  if( r_antsimage == NULL || r_coordinates == NULL || r_value == NULL )
    {
    //Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
    //return Rcpp::wrap( 1 ) ;
    returnFlag = 7;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;
  std::string errorMessage = "Unable to set pixel values: ";

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
    returnFlag = 5;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  //Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
    returnFlag = 5;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  //Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
    returnFlag = 5;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  returnFlag = antsImage_SetPixels< PixelType , ImageDimension >( *antsimage_xptr , r_coordinates , r_value ) ;
	}
      else
	{
	  //Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
    returnFlag = 5;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      returnFlag = 6;
    }

  switch ( returnFlag )
    {
    case 1:
      errorMessage += "Indices do not match the image in dimensions";
      break;
    case 2:
      errorMessage += "RHS vector must be scalar or of same length as indices";
      break;
    case 3:
      errorMessage += "Index is outside of image space";
      break;
    case 4:
      errorMessage += "Input image has not been allocated";
      break;
    case 5:
      errorMessage += "Unsupported image dimension";
      break;
    case 6:
      errorMessage += "Unsupported pixeltype";
      break;
    case 7:
      errorMessage += "Unspecified arguments";
      break;
    default:
      errorMessage += "Unknown error";
    }

  return Rcpp::List::create( Rcpp::Named("image") = r_antsimage,
                             Rcpp::Named("flag") = returnFlag,
                             Rcpp::Named("error") = errorMessage );
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }


template< class ImageType >
SEXP antsImage_GetSpacing( SEXP  r_antsimage )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>(r_antsimage);
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }
  return Rcpp::wrap( image->GetSpacing().GetVnlVector() );
}

RcppExport SEXP antsImage_GetSpacing( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::Rcout << "Unspecified Argument" << std::endl ;
    return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_GetSpacing<ImageType>( r_antsimage );
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_GetSpacing<ImageType>( r_antsimage );
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_GetSpacing<ImageType>( r_antsimage );
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }
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


template< class ImageType >
SEXP antsImage_SetSpacing( SEXP r_antsimage, SEXP r_spacing )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>(r_antsimage);

  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  unsigned int nDim = ImageType::ImageDimension;
  typename ImageType::SpacingType itkSpacing;

  Rcpp::NumericVector rcpp_spacing(r_spacing);
  for (unsigned int i=0; i<nDim; i++)
  {
    itkSpacing[i] = rcpp_spacing[i];
  }

  image->SetSpacing(itkSpacing);
  return Rcpp::wrap(NULL);
}

RcppExport SEXP antsImage_SetSpacing( SEXP r_antsimage, SEXP r_spacing )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  Rcpp::NumericVector rcpp_spacing(r_spacing);

  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>   ImageType;
    return antsImage_SetSpacing<ImageType>( r_antsimage, r_spacing );
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>   ImageType;
    return antsImage_SetSpacing<ImageType>( r_antsimage, r_spacing );
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>   ImageType;
    return antsImage_SetSpacing<ImageType>( r_antsimage, r_spacing );
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }

  return Rcpp::wrap(NULL);

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


template< class ImageType >
SEXP antsImage_GetOrigin( SEXP r_antsimage )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>(r_antsimage);
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }
  return Rcpp::wrap( image->GetOrigin().GetVnlVector() );
}

RcppExport SEXP antsImage_GetOrigin( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>  ImageType;
    return antsImage_GetOrigin<ImageType>( r_antsimage );
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>  ImageType;
    return antsImage_GetOrigin<ImageType>( r_antsimage );
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>  ImageType;
    return antsImage_GetOrigin<ImageType>( r_antsimage );
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }
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


template< class ImageType >
SEXP antsImage_SetOrigin( SEXP r_antsimage, SEXP r_origin )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  unsigned int nDim = ImageType::ImageDimension;
  typename ImageType::PointType itkOrigin;

  Rcpp::NumericVector rcpp_origin(r_origin);
  for (unsigned int i=0; i<nDim; i++)
    {
    itkOrigin[i] = rcpp_origin[i];
    }

  image->SetOrigin(itkOrigin);
  return Rcpp::wrap(NULL);
}

RcppExport SEXP antsImage_SetOrigin( SEXP r_antsimage, SEXP r_origin )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_SetOrigin<ImageType>( r_antsimage, r_origin ) ;
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_SetOrigin<ImageType>( r_antsimage, r_origin ) ;
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_SetOrigin<ImageType>( r_antsimage, r_origin ) ;
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }
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



template< class ImageType >
SEXP antsImage_GetDirection( SEXP r_antsimage )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  unsigned int nDim = ImageType::ImageDimension;
  Rcpp::NumericMatrix dir( nDim, nDim );

  for (unsigned int i=0; i<nDim; i++)
  {
    for (unsigned int j=0; j<nDim; j++)
    {
      dir(i,j) = image->GetDirection()(i,j);
    }
  }

  return dir;
}

RcppExport SEXP antsImage_GetDirection( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  Rcpp::NumericMatrix direction(dimension, dimension);

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_GetDirection<ImageType>(r_antsimage);
   }
  else if (dimension == 3)
   {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_GetDirection<ImageType>(r_antsimage);
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_GetDirection<ImageType>(r_antsimage);
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }

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


template< class ImageType >
SEXP antsImage_SetDirection( SEXP r_antsimage, SEXP r_direction )
{
  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  unsigned int nDim = ImageType::ImageDimension;
  Rcpp::NumericMatrix direction( r_direction );

  if ( (direction.nrow() != nDim) || (direction.ncol() != nDim) )
    {
    Rcpp::stop("direction array must be of size ImageDimension * ImageDimension");
    }

  if ( !image.IsNotNull() )
    {
    Rcpp::stop("Invalid image");
    }

  typename ImageType::DirectionType itkDirection;
  for( unsigned int i=0; i < nDim; ++i )
    {
    for ( unsigned int j=0; j<nDim; j++ )
      {
        itkDirection(i,j) = direction(i,j);
      }
    }

  if ( vnl_determinant(itkDirection.GetVnlMatrix()) == 0.0 )
    {
    Rcpp::stop("Image direction matrix has determinant == 0");
    }

  image->SetDirection( itkDirection );
  return Rcpp::wrap(NULL);

}

RcppExport SEXP antsImage_SetDirection( SEXP r_antsimage, SEXP r_direction )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_SetDirection<ImageType>( r_antsimage, r_direction ) ;
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_SetDirection<ImageType>( r_antsimage, r_direction ) ;
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_SetDirection<ImageType>( r_antsimage, r_direction ) ;
  }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }

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


template< class ImageType >
SEXP antsImage_TransformIndexToPhysicalPoint( SEXP r_antsimage, SEXP r_index )
{
  typedef typename ImageType::Pointer                            ImagePointerType ;
  typedef typename ImageType::PointType                          PointType;
  typedef typename PointType::CoordRepType                       CoordRepType;

  typedef typename itk::ContinuousIndex<CoordRepType, ImageType::ImageDimension> IndexType;

  const unsigned int nDim = ImageType::ImageDimension;

  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  Rcpp::NumericMatrix indices( r_index );
  if ( indices.ncol() != nDim )
    {
    Rcpp::stop("index matrix must be of size N * ImageDimension");
    }
  if ( !image.IsNotNull() )
    {
    Rcpp::stop("must pass a valid antsImage");
    }

  unsigned long N = indices.nrow();
  Rcpp::NumericMatrix points( N, nDim ) ;

  IndexType itkindex;
  PointType itkpoint;

  for( unsigned int j = 0; j < N; j++)
    {

    for( unsigned int i = 0; i < nDim; i++ )
      {
      itkindex[i] = static_cast<CoordRepType>( indices(j,i) - 1.0 );
      }

    image->TransformContinuousIndexToPhysicalPoint( itkindex, itkpoint );

    for ( int i = 0; i < nDim; i++ )
      {
      points(j,i) = itkpoint[i];
      }
    }

  return points;
}

RcppExport SEXP antsImage_TransformIndexToPhysicalPoint( SEXP r_antsimage, SEXP r_index )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_TransformIndexToPhysicalPoint<ImageType>( r_antsimage, r_index ) ;
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_TransformIndexToPhysicalPoint<ImageType>( r_antsimage, r_index ) ;
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_TransformIndexToPhysicalPoint<ImageType>( r_antsimage, r_index ) ;
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }

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

template< class ImageType >
SEXP antsImage_TransformPhysicalPointToIndex( SEXP r_antsimage, SEXP r_point )
{
  typedef typename ImageType::Pointer      ImagePointerType ;
  typedef typename ImageType::PointType    PointType;
  typedef typename PointType::CoordRepType CoordRepType;

  typedef typename itk::ContinuousIndex<CoordRepType, ImageType::ImageDimension> IndexType;
  const unsigned int nDim = ImageType::ImageDimension;

  ImagePointerType image = Rcpp::as<ImagePointerType>(r_antsimage);
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("Image not yet allocated");
    }

  Rcpp::NumericMatrix points( r_point );
  if ( points.ncol() != nDim)
    {
    Rcpp::stop("point matrix must be of size N * ImageDimension");
    }
  if ( ! image.IsNotNull() )
    {
    Rcpp::stop("must pass a valid antsImage");
    }

  unsigned long N = points.nrow();
  Rcpp::NumericMatrix indices( N, nDim) ;

  IndexType itkindex;
  PointType itkpoint;

  for( unsigned int j = 0; j < N; j++)
    {

    for( unsigned int i = 0; i < nDim; i++ )
      {
      itkpoint[i] = static_cast<CoordRepType>( points(j,i) );
      }

    image->TransformPhysicalPointToContinuousIndex( itkpoint, itkindex );

    for ( int i = 0; i < nDim; i++ )
      {
      indices(j,i) = itkindex[i] + 1.0;
      }
    }

  return indices;
}

RcppExport SEXP antsImage_TransformPhysicalPointToIndex( SEXP r_antsimage, SEXP r_point )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Argument");
    }

  Rcpp::S4 antsimage( r_antsimage );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );

  if (dimension == 2)
    {
    typedef itk::ImageBase<2>      ImageType;
    return antsImage_TransformPhysicalPointToIndex<ImageType>( r_antsimage, r_point ) ;
    }
  else if (dimension == 3)
    {
    typedef itk::ImageBase<3>      ImageType;
    return antsImage_TransformPhysicalPointToIndex<ImageType>( r_antsimage, r_point ) ;
    }
  else if (dimension == 4)
    {
    typedef itk::ImageBase<4>      ImageType;
    return antsImage_TransformPhysicalPointToIndex<ImageType>( r_antsimage, r_point ) ;
    }
  else
    {
    Rcpp::stop( "Invalid image dimension");
    }

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

template< class PixelType , unsigned int Dimension >
bool antsImage_SetRegion( typename itk::Image< PixelType , Dimension >::Pointer& image , SEXP r_mask , SEXP r_antsregion , SEXP r_value )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      typename ImageType::RegionType region ;
      Rcpp::S4 antsregion( r_antsregion ) ;
      Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) ) ;
      Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) ) ;
      if( indexvector.size() == 0 && sizevector.size() == 0 )
	{
	  region = image->GetLargestPossibleRegion() ;
	}
      else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	{
	  Rcpp::Rcout << "antsRegion provided has dimensions incompatible with the image" << std::endl ;
	  return 1 ;
	}
      else
	{
	  typename ImageType::IndexType index ;
	  typename ImageType::SizeType size ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	    }
	  region.SetSize( size ) ;
	  region.SetIndex( index ) ;
	}
      itk::ImageRegionIterator< ImageType > image_iter( image , region ) ;
      Rcpp::LogicalVector mask( r_mask ) ;
      int numberofpixelspertime = region.GetSize(0) ;
      for( unsigned int i = 1 ; i < Dimension-1 ; ++i )
	{
	  numberofpixelspertime *= region.GetSize(i) ;
	}
      if( mask.size() != 0 && mask.size() != numberofpixelspertime && mask.size() != (int)region.GetNumberOfPixels() )
	{
	  Rcpp::Rcout << "Length of 'mask' vector must be either 0, num-of-voxels-per-time-in-region, num-of-voxels-in-region" << std::endl ;
	  return 1 ;
	}
      Rcpp::NumericVector value( r_value ) ;
      if( value.size() != 1 && value.size() != (int)region.GetNumberOfPixels() && value.size() != std::count( mask.begin() , mask.end() , TRUE ) )
	{
	  Rcpp::Rcout << "Length of 'value' vector must be either 1, num-of-voxels-in-region or num-of-TRUEs-in-mask" << std::endl ;
	  return 1 ;
	}

      if( mask.size() == 0 )
	{
	  if( value.size() == 1 )
	    {
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
		{
		  image_iter.Set( value[0] ) ;
		}
	    }
	  else
	    {
	      unsigned int value_ind = 0 ;
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
		{
		  image_iter.Set( value[value_ind++] ) ;
		}
	    }
	}
      else
	{
	  Rcpp::LogicalVector::iterator mask_iter = mask.begin() ;
	  if( value.size() == 1 )
	    {
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		{
		  if( mask_iter == mask.end() )
		    mask_iter = mask.begin() ;
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[0] ) ;
		}
	    }
	  else if( value.size() == (int)region.GetNumberOfPixels() )
	    {
	      image_iter.GoToBegin() ;
	      for( unsigned long value_ind = 0 ; (int)value_ind < value.size() ; ++image_iter , ++mask_iter , ++value_ind )
		{
		  if( mask_iter == mask.end() )
		    mask_iter = mask.begin() ;
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[value_ind] ) ;
		}
	    }
	  else
	    {
	      unsigned long value_ind = 0 ;
	      for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter , ++mask_iter )
		{
		  if( mask_iter == mask.end() )
		    {
		      mask_iter = mask.begin() ;
		      value_ind = 0 ;
		    }
		  if( *mask_iter == TRUE )
		    image_iter.Set( value[value_ind++] ) ;
		}
	    }
	}
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return 1 ;
    }
  return 0 ;
}

RcppExport SEXP antsImage_SetRegion( SEXP r_antsimage , SEXP r_mask , SEXP r_antsregion , SEXP r_value )
try
{
  if( r_antsimage == NULL || r_mask == NULL || r_antsregion == NULL || r_value == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( 1 ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  antsImage_SetRegion< PixelType , ImageDimension >( *antsimage_xptr , r_mask , r_antsregion , r_value ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
    }
  return r_antsimage ;
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( 1 ) ;
  }


template< class ImageType >
typename ImageType::Pointer antsImage_asantsImage( Rcpp::NumericVector& vector , Rcpp::NumericVector& spacing , Rcpp::NumericVector& origin )
{
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  Rcpp::IntegerVector vector_dim = vector.attr( "dim" ) ;
  typename ImageType::IndexType image_index ;
  typename ImageType::SizeType image_size ;
  typename ImageType::SpacingType image_spacing ;
  typename ImageType::PointType image_origin ;
  for( unsigned int i = 0 ; i < ImageType::ImageDimension ; ++i )
    {
      image_index[i] = 0 ;
      image_size[i] = vector_dim[i] ;
      image_spacing[i] = spacing[i] ;
      image_origin[i] = origin[i] ;
    }
  typename ImageType::RegionType image_region ;
  image_region.SetIndex( image_index ) ;
  image_region.SetSize( image_size ) ;

  ImagePointerType image = ImageType::New() ;
  image->SetRegions( image_region ) ;
  image->SetSpacing( image_spacing ) ;
  image->SetOrigin( image_origin ) ;
  image->Allocate() ;

  itk::ImageRegionIterator< ImageType > image_iter( image , image_region ) ;
  unsigned int vector_ind = 0 ;
  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
    {
      image_iter.Set( static_cast< typename ImageType::PixelType >( vector[vector_ind++] ) ) ;
    }

  return image ;
}

RcppExport SEXP antsImage_asantsImage( SEXP r_vector , SEXP r_pixeltype , SEXP r_spacing , SEXP r_origin )
try
{
  if( r_vector == NULL || r_pixeltype == NULL || r_spacing == NULL || r_origin == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  std::string pixeltype = Rcpp::as< std::string >( r_pixeltype ) ;
  Rcpp::NumericVector vector( r_vector ) ;
  Rcpp::IntegerVector vector_dim = vector.attr( "dim" ) ;
  Rcpp::NumericVector spacing( r_spacing ) ;
  Rcpp::NumericVector origin( r_origin ) ;
  if( spacing.size() != vector_dim.size() || origin.size() != vector_dim.size() )
    {
      Rcpp::Rcout << "spacing or origin is incompatible with the vector" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  if( pixeltype == "double" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "double" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "float" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned int" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( vector_dim.size() == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 4 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 3 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else if( vector_dim.size() == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  ImagePointerType* ptr_ptr_image = new ImagePointerType( antsImage_asantsImage< ImageType >( vector , spacing , origin ) ) ;
	  Rcpp::XPtr< ImagePointerType > xptr( ptr_ptr_image , true ) ;
	  Rcpp::S4 image_r( std::string( "antsImage" ) ) ;
	  image_r.slot( "pixeltype" ) = std::string( "unsigned char" ) ;
	  image_r.slot( "dimension" ) = 2 ;
	  image_r.slot( "pointer" ) = xptr ;
    image_r.slot( "components" ) = 1;
	  return image_r ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }


template< class PixelType , unsigned int Dimension >
SEXP antsImage_RelationalOperators( typename itk::Image< PixelType , Dimension >::Pointer image , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
{
  typedef itk::Image< PixelType , Dimension > ImageType ;
  typedef typename ImageType::Pointer ImagePointerType ;
  typedef itk::PermuteAxesImageFilter< ImageType > PermuteAxesFilterType ;
  typedef typename PermuteAxesFilterType::Pointer PermuteAxesFilterPointerType ;
  typedef typename PermuteAxesFilterType::PermuteOrderArrayType PermuteAxesFilterOrderType ;

  if( image.IsNotNull() )
    {
      // PermuteAxesFilterPointerType permuteaxesfilter = PermuteAxesFilterType::New() ;
      // permuteaxesfilter->SetInput( origimage ) ;
      // PermuteAxesFilterOrderType permuteaxesfilterorder ;
      // permuteaxesfilterorder[0] = 1 ;
      // permuteaxesfilterorder[1] = 0 ;
      // for( unsigned int i = 2 ; i < Dimension ; ++i )
      // 	{
      // 	  permuteaxesfilterorder[i] = i ;
      // 	}
      // permuteaxesfilter->SetOrder( permuteaxesfilterorder ) ;
      // ImagePointerType image = permuteaxesfilter->GetOutput() ;
      // permuteaxesfilter->Update() ;

      typename ImageType::RegionType region ;
      Rcpp::S4 antsregion( r_antsregion ) ;
      Rcpp::IntegerVector indexvector( antsregion.slot( "index" ) ) ;
      Rcpp::IntegerVector sizevector( antsregion.slot( "size" ) ) ;
      if( indexvector.size() == 0 && sizevector.size() == 0 )
	{
	  region = image->GetLargestPossibleRegion() ;
	}
      else if( indexvector.size() != (int)Dimension || sizevector.size() != (int)Dimension )
	{
	  Rcpp::Rcout << "antsRegion provided has dimensions incompatible with the image" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
      else
	{
	  typename ImageType::IndexType index ;
	  typename ImageType::SizeType size ;
	  for( unsigned int i = 0 ; i < Dimension ; ++i )
	    {
	      index[i] = indexvector[i] - 1 ;
	      size[i] = sizevector[i] ;
	    }
	  region.SetSize( size ) ;
	  region.SetIndex( index ) ;
	}
      itk::ImageRegionConstIterator< ImageType > image_iter( image , region ) ;
      Rcpp::LogicalVector vector_r( (int)region.GetNumberOfPixels() ) ;
      unsigned int ind = 0 ;
      double value = Rcpp::as< double >( r_value ) ;
      if( Rcpp::as< std::string >( r_operator ) == "==" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() == value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "!=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() != value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "<=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() <= value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == ">=" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() >= value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == "<" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() < value ) ;
	    }
	}
      else if( Rcpp::as< std::string >( r_operator ) == ">" )
	{
 	  for( image_iter.GoToBegin() ; !image_iter.IsAtEnd() ; ++image_iter )
	    {
	      vector_r[ind++] = ( image_iter.Get() > value ) ;
	    }
	}
      else
	{
	  Rcpp::Rcout << "unsupported operator" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
      // Rcpp::IntegerVector dims( Dimension ) ;
      // for( unsigned int i = 0 ; i < Dimension ; ++i )
      // 	{
      // 	  dims[i] = region.GetSize( i )  ;
      // 	}
      // vector_r.attr( "dim" ) = dims ;
      return vector_r ;
    }
  else
    {
      // Rcpp::Rcout << "Empty image" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}

RcppExport SEXP antsImage_RelationalOperators( SEXP r_antsimage , SEXP r_value , SEXP r_antsregion , SEXP r_operator )
try
{
  if( r_antsimage == NULL || r_value == NULL || r_antsregion == NULL )
    {
      Rcpp::Rcout << "Unspecified Arguments" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }

  Rcpp::S4 antsimage( r_antsimage ) ;
  std::string pixeltype = Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) ) ;

  if( pixeltype == "double" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef double PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "float" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef float PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned int" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned int PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else if( pixeltype == "unsigned char" )
    {
      if( dimension == 4 )
	{
	  const int ImageDimension = 4 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 3 )
	{
	  const int ImageDimension = 3 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else if( dimension == 2 )
	{
	  const int ImageDimension = 2 ;
	  typedef unsigned char PixelType ;
	  typedef itk::Image< PixelType , ImageDimension > ImageType ;
	  typedef ImageType::Pointer ImagePointerType ;
	  Rcpp::XPtr< ImagePointerType > antsimage_xptr( static_cast< SEXP >( antsimage.slot( "pointer" ) ) ) ;
	  return antsImage_RelationalOperators< PixelType , ImageDimension >( *antsimage_xptr , r_value , r_antsregion , r_operator ) ;
	}
      else
	{
	  Rcpp::Rcout << "Unsupported Dimension" << std::endl ;
	  return Rcpp::wrap( NA_REAL ) ;
	}
    }
  else
    {
      Rcpp::Rcout << "Unsupported PixelType" << std::endl ;
      return Rcpp::wrap( NA_REAL ) ;
    }
}
catch( const std::exception& exc )
  {
    Rcpp::Rcout<< exc.what() << std::endl ;
    return Rcpp::wrap( NA_REAL ) ;
  }
