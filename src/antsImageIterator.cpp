
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

template< class ImageType >
SEXP antsImageIterator( SEXP r_antsimage )
{
  Rcpp::S4 iterator_r(std::string( "antsImageIterator"));
  Rcpp::S4 antsimage( r_antsimage );
  iterator_r.slot("pixeltype") = Rcpp::as< std::string >( antsimage.slot("pixeltype") );
  iterator_r.slot("dimension") = Rcpp::as< unsigned int >( antsimage.slot("dimension") );
  iterator_r.slot("components") = Rcpp::as< unsigned int >( antsimage.slot("components") );

  typedef typename ImageType::Pointer ImagePointerType;
  ImagePointerType image = Rcpp::as<ImagePointerType>( r_antsimage );

  typedef typename itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
  IteratorType iterator(image, image->GetLargestPossibleRegion());
  IteratorType* rawPointer = new IteratorType( iterator );

  Rcpp::XPtr< IteratorType > xptr( rawPointer, false );
  iterator_r.slot("pointer") = xptr;

  return Rcpp::wrap(iterator_r);
}


RcppExport SEXP antsImageIterator( SEXP r_antsimage )
{
try
{
  if( r_antsimage == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 image( r_antsimage );

  std::string pixeltype = Rcpp::as< std::string >( image.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( image.slot("dimension") );
  //unsigned int components = Rcpp::as< int >( image.slot("components") );

  if ( (dimension < 1) || (dimension > 4) )
    {
    Rcpp::stop("Unsupported image dimension");
    }

  //typedef itk::Image<float,4>         ImageType;
  //return antsImageIterator<ImageType>( r_antsimage );


  if( pixeltype == "double" )
    {
    typedef double ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage  );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage  );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator<ImageType>( r_antsimage );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return  antsImageIterator<ImageType>( r_antsimage );
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



template< class ImageType >
SEXP antsImageIterator_Get( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  return Rcpp::wrap( iterator.Get() );
}


RcppExport SEXP antsImageIterator_Get( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Get<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }


  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_Set( SEXP r_antsimageiterator, SEXP r_value )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  Rcpp::NumericVector value( r_value );
  iterator.Set( value[0] );
  return Rcpp::wrap( true );
  //return Rcpp::wrap( iterator );
}


RcppExport SEXP antsImageIterator_Set( SEXP r_antsimageiterator, SEXP r_value )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Set<ImageType>( r_antsimageiterator, r_value );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
  }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_Next( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  ++iterator;
  return Rcpp::wrap( iterator );
}

RcppExport SEXP antsImageIterator_Next( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Next<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_Previous( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  --iterator;
  return Rcpp::wrap( iterator );
}

RcppExport SEXP antsImageIterator_Previous( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Previous<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_IsAtEnd( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  return Rcpp::wrap( iterator.IsAtEnd() );
}

RcppExport SEXP antsImageIterator_IsAtEnd( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }


  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_GoToBegin( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  iterator.GoToBegin();
  return Rcpp::wrap( iterator );
}

RcppExport SEXP antsImageIterator_GoToBegin( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_GetIndex( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  typename ImageType::IndexType index = iterator.GetIndex();

  Rcpp::IntegerVector index_r( ImageType::ImageDimension );
  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    index_r[i] = index[i] + 1;
    }

  return Rcpp::wrap( index_r );
}

RcppExport SEXP antsImageIterator_GetIndex( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GetIndex<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }


  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_SetIndex( SEXP r_antsimageiterator, SEXP r_index )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  typename ImageType::IndexType index;
  Rcpp::NumericVector rcpp_index( r_index );

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    index[i] = rcpp_index[i] - 1;
    }

  iterator.SetIndex(index);
  return Rcpp::wrap( iterator );
}

RcppExport SEXP antsImageIterator_SetIndex( SEXP r_antsimageiterator, SEXP r_index )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_SetIndex<ImageType>( r_antsimageiterator, r_index );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }


  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_Remaining( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  return Rcpp::wrap( iterator.Remaining() );
}

RcppExport SEXP antsImageIterator_Remaining( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_Remaining<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_GoToReverseBegin( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  iterator.GoToReverseBegin();
  return Rcpp::wrap( iterator );
}

RcppExport SEXP antsImageIterator_GoToReverseBegin( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_GoToReverseBegin<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
SEXP antsImageIterator_IsAtReverseEnd( SEXP r_antsimageiterator )
{
  typedef itk::ImageRegionIteratorWithIndex<ImageType>    IteratorType;
  IteratorType iterator = Rcpp::as<IteratorType>( r_antsimageiterator );
  return Rcpp::wrap( iterator.IsAtReverseEnd() );
}

RcppExport SEXP antsImageIterator_IsAtReverseEnd( SEXP r_antsimageiterator )
{
try
{
  if( r_antsimageiterator == NULL )
    {
    Rcpp::stop("Unspecified Arguments");
    }

  Rcpp::S4 iterator( r_antsimageiterator );
  std::string pixeltype = Rcpp::as< std::string >( iterator.slot("pixeltype") );
  unsigned int dimension = Rcpp::as< int >( iterator.slot("dimension") );
  unsigned int components = Rcpp::as< int >( iterator.slot("components") );

  if ( components > 1 )
  {
    Rcpp::stop("Iterators do not currently support multichannel images");
  }

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
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
	  }
  else if( pixeltype == "float" )
    {
    typedef float ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned int" )
    {
    typedef unsigned int ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else if( pixeltype == "unsigned char" )
    {
    typedef unsigned char ValueType;
    if( dimension == 4 )
	    {
      typedef itk::Image<ValueType,4>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
      }
    else if( dimension == 3 )
	    {
	    typedef itk::Image<ValueType,3>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    else if( dimension == 2 )
	    {
	    typedef itk::Image<ValueType,2>       ImageType;
      return antsImageIterator_IsAtReverseEnd<ImageType>( r_antsimageiterator );
	    }
    }
  else
    {
    Rcpp::stop("Unsupported PixelType");
    }

  return Rcpp::wrap(NA_REAL);

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
