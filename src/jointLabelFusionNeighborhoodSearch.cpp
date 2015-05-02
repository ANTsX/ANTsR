#include <exception>
#include <vector>
#include <algorithm>
#include <string>
#include "RcppANTsR.h"
#include <ants.h>
#include "itkImage.h"

template< class ImageType >
SEXP jointLabelFusionNeighborhoodSearchHelper(
  SEXP r_intvec,
  SEXP r_center,
  unsigned int rad,
  unsigned int radSearch,
  SEXP r_antsimage,
  SEXP r_antsimageseg)
{
  unsigned int segval = 0;
  typedef typename ImageType::Pointer  ImagePointerType;
  const unsigned int ImageDimension = ImageType::ImageDimension;
  typedef float                        PixelType;
  typedef itk::Image< PixelType, ImageDimension > InputImageType;
  typedef itk::Image< PixelType, ImageDimension > OutputImageType;
  typename ImageType::Pointer image =
    Rcpp::as< ImagePointerType >( r_antsimage );
  typename ImageType::Pointer imageseg =
      Rcpp::as< ImagePointerType >( r_antsimageseg );
  Rcpp::NumericVector intvec( r_intvec );
  Rcpp::NumericVector outvec =
    Rcpp::NumericVector( intvec.size(), Rcpp::NumericVector::get_na() );
  Rcpp::NumericVector bestvec =
    Rcpp::NumericVector( intvec.size(), Rcpp::NumericVector::get_na() );
  Rcpp::NumericVector center( r_center );
  if ( center.size() != ImageDimension )
    Rcpp::stop("jointLabelFusionNeighborhoodSearchHelper dim error.");
  typename itk::NeighborhoodIterator<ImageType>::SizeType nSize;
  typename itk::NeighborhoodIterator<ImageType>::SizeType nSizeSearch;
  typename ImageType::IndexType ind;
  ind.Fill( 0 );
  for ( unsigned int i=0; i<ImageDimension; i++ )
    {
    nSize[i] = rad;
    nSizeSearch[i] = radSearch;
    ind[i] = center[i] - 1; // R coords to ITK
    }
  itk::NeighborhoodIterator<ImageType> nit( nSize, image,
    image->GetLargestPossibleRegion() ) ;
  itk::NeighborhoodIterator<ImageType> nitSearch( nSizeSearch, image,
    image->GetLargestPossibleRegion() ) ;
// for each location in nitSearch, compute the correlation
// of the intvec with the nit neighborhood
  nitSearch.SetLocation( ind );
  PixelType bestcor = -1.e11;
  for( unsigned int i = 0; i < nitSearch.Size(); i++ )
    {
    typename ImageType::IndexType ind2 = nitSearch.GetIndex(i);
    nit.SetLocation( ind2 );
    PixelType outmean = 0;
    PixelType outsd = 0;
    for ( unsigned int i=0; i < intvec.size(); i++ ) {
      PixelType pix = image->GetPixel( nit.GetIndex(i) );
      outvec[i] = pix;
      outmean += pix;
      }
    outmean /= ( static_cast<PixelType>(intvec.size()) );
    for ( unsigned int i=0; i < intvec.size(); i++ ) {
      // should use recursive formula in above loop
      outsd += ( outvec[i] - outmean ) * ( outvec[i] - outmean );
      }
    outsd = sqrt(  outsd / ( static_cast<PixelType>(intvec.size()) - 1 ) );
    //  tv=( tv - mean(tv) )/sdv
    //  locor<-(-1.0 * sum(targetint*tv) )
    if ( outsd > 1.e-10 )
      {
      for ( unsigned int i=0; i < intvec.size(); i++ )
        outvec[i] = ( outvec[i] - outmean ) / outsd;
      PixelType locor = 0;
      for ( unsigned int i=0; i < intvec.size(); i++ ) {
        locor += intvec[i] * outvec[i];
        }
      if ( locor > bestcor )
        {
        segval = imageseg->GetPixel( ind2 );
        for ( unsigned int i=0; i < intvec.size(); i++ ) {
          bestvec[i] = outvec[i];
          }
        bestcor = locor;
        }
//      Rcpp::Rcout << " locor " << locor << " best " << bestcor << std::endl;
      }
    }
  return Rcpp::List::create( Rcpp::Named("segval") = segval,
    Rcpp::Named("values") = bestvec );
}

// nhsearch = .Call("jointLabelFusionNeighborhoodSearch",
//  targetint, cent, max(rad), rSearch, atlasList[[ct]],
//  PACKAGE = "ANTsR" )
RcppExport SEXP jointLabelFusionNeighborhoodSearch(
  SEXP r_intvec,      // intensity vector
  SEXP r_cent,        // dimension-d vector
  SEXP r_rad,         // uinteger
  SEXP r_radSearch,   // uinteger
  SEXP r_antsimage,   // atlas image
  SEXP r_antsimageseg  )  // atlas image seg
{
try
{
  Rcpp::S4 antsimage( r_antsimage );
  std::string pixeltype =
    Rcpp::as< std::string >( antsimage.slot( "pixeltype" ) );
  unsigned int dimension = Rcpp::as< int >( antsimage.slot( "dimension" ) );
  if ( ( pixeltype == "float" ) & ( dimension == 2 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 2;
    typedef itk::Image< PixelType, dim >  ImageType;
    SEXP out = jointLabelFusionNeighborhoodSearchHelper< ImageType>(
      r_intvec, r_cent,
      Rcpp::as< unsigned int >( r_rad ),
      Rcpp::as< unsigned int >( r_radSearch ),
      r_antsimage, r_antsimageseg // atlas
      );
    return( out );
  }
  else if ( ( pixeltype == "float" ) & ( dimension == 3 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim >  ImageType;
    SEXP out = jointLabelFusionNeighborhoodSearchHelper< ImageType>(
      r_intvec, r_cent,
      Rcpp::as< unsigned int >( r_rad ),
      Rcpp::as< unsigned int >( r_radSearch ),
      r_antsimage, r_antsimageseg  // atlas
      );
    return( out );
  }
  else
  {
    Rcpp::stop( "Unsupported image dimension or pixel type." );
  }
}
catch( itk::ExceptionObject & err )
{
  Rcpp::Rcout << "ITK ExceptionObject caught!" << std::endl;
  forward_exception_to_r( err );
}
catch( const std::exception& exc )
{
  Rcpp::Rcout << "STD ExceptionObject caught!" << std::endl;
  forward_exception_to_r( exc );
}
catch( ... )
{
  Rcpp::stop( "C++ exception (unknown reason)");
}
 return Rcpp::wrap(NA_REAL); // not reached
}
