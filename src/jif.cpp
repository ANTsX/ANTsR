#include <exception>
#include <vector>
#include <algorithm>
#include <string>
#include "RcppANTsR.h"
#include <ants.h>

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
  typename ImageType::Pointer image =
    Rcpp::as< ImagePointerType >( r_antsimage );
  typename ImageType::Pointer imageseg =
      Rcpp::as< ImagePointerType >( r_antsimageseg );
  Rcpp::NumericVector intvec( r_intvec );
  Rcpp::NumericVector outvec =
    Rcpp::NumericVector( intvec.size(), Rcpp::NumericVector::get_na() );
  Rcpp::NumericVector bestvec =
    Rcpp::NumericVector( intvec.size(), Rcpp::NumericVector::get_na() );
  Rcpp::NumericVector outsegvec =
    Rcpp::NumericVector( intvec.size(), Rcpp::NumericVector::get_na() );
  Rcpp::NumericVector bestsegvec =
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
    ind[i] = center[i]; // R coords to ITK
    }
  itk::NeighborhoodIterator<ImageType> nit( nSize, image,
    image->GetLargestPossibleRegion() ) ;
  itk::NeighborhoodIterator<ImageType> nitSearch( nSizeSearch, image,
    image->GetLargestPossibleRegion() ) ;
// for each location in nitSearch, compute the correlation
// of the intvec with the nit neighborhood
  nitSearch.SetLocation( ind );
  PixelType bestcor = 1.e11;
  PixelType bestsd = 0;
  PixelType bestmean = 0;
  for( unsigned int i = 0; i < nitSearch.Size(); i++ )
    {
    typename ImageType::IndexType ind2 = nitSearch.GetIndex(i);
    nit.SetLocation( ind2 );
    PixelType outmean = 0;
    PixelType outsd = 0;
    PixelType inmean = 0;
    PixelType insd = 0;
    for ( unsigned int i=0; i < intvec.size(); i++ ) {
      PixelType pix = image->GetPixel( nit.GetIndex(i) );
      outvec[i] = pix;
      outsegvec[i] = imageseg->GetPixel( nit.GetIndex(i) );
      outmean += pix;
      inmean += intvec[i];
      }
    outmean /= ( static_cast<PixelType>(intvec.size()) );
    inmean /= ( static_cast<PixelType>(intvec.size()) );
    for ( unsigned int i=0; i < intvec.size(); i++ ) {
      // should use recursive formula in above loop
      outsd += ( outvec[i] - outmean ) * ( outvec[i] - outmean );
      insd += ( intvec[i] - inmean ) * ( intvec[i] - inmean );
      }
    outsd = sqrt(  outsd  );
    insd = sqrt(  insd  );
    PixelType sum_uv = 0;
    PixelType sum_psearch = 0;
    PixelType ssq_psearch = 0;
    unsigned int n = intvec.size();
    for(unsigned int i = 0; i < n; i++)
      {
      PixelType v = intvec[i];
      PixelType u = outvec[i];
      sum_psearch += u;
      ssq_psearch += u * u;
      sum_uv += u * v;
      }
    PixelType var_u_unnorm = ssq_psearch - sum_psearch * sum_psearch / n;
    if(var_u_unnorm < 1.0e-6)
      var_u_unnorm = 1.0e-6;
    PixelType locor = 0;
    if ( sum_uv > 0 )
      locor = ( -1.0 * (sum_uv * sum_uv) / var_u_unnorm );
    else
      locor = ( sum_uv * sum_uv ) / var_u_unnorm;
//  - (\Sum u_i v_i)^2 / z,   where z = sigma_v^2 * (n-1)
//      locor = locor / ( insd * outsd );
      if ( locor < bestcor )
        {
        segval = imageseg->GetPixel( ind2 );
        for ( unsigned int i=0; i < intvec.size(); i++ ) {
          bestvec[i] = outvec[i];
          bestsegvec[i] = outsegvec[i];
          }
        bestcor = locor;
        bestsd = outsd;
        bestmean = outmean;
        }
    }
  return Rcpp::List::create( Rcpp::Named("segval") = segval,
    Rcpp::Named("values") = bestvec,
    Rcpp::Named("bestmean") = bestmean,
    Rcpp::Named("bestsd") = bestsd,
    Rcpp::Named("bestcor") = bestcor,
    Rcpp::Named("bestsegvec") = bestsegvec  );
}


RcppExport SEXP jointLabelFusionNeighborhoodSearch( SEXP r_intvec, SEXP r_cent, SEXP r_rad, SEXP r_radSearch, SEXP r_antsimage, SEXP r_antsimageseg  )
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






template< class ImageType >
SEXP addNeighborhoodToImageHelper(
  SEXP r_antsimage,
  SEXP r_center,
  SEXP r_rad,
  SEXP r_vec)
{
  typedef typename ImageType::Pointer  ImagePointerType;
  const unsigned int ImageDimension = ImageType::ImageDimension;
  typedef float                        PixelType;
  typename ImageType::Pointer image =
    Rcpp::as< ImagePointerType >( r_antsimage );
  Rcpp::NumericVector center( r_center );
  Rcpp::NumericVector rad( r_rad );
  Rcpp::NumericVector intvec( r_vec );
  if ( center.size() != ImageDimension )
    Rcpp::stop("addNeighborhoodToImageHelper dim error.");
  typename itk::NeighborhoodIterator<ImageType>::SizeType nSize;
  typename ImageType::IndexType ind;
  ind.Fill( 0 );
  for ( unsigned int i=0; i<ImageDimension; i++ )
    {
    nSize[i] = rad[i];
    ind[i] = center[i]; // R coords to ITK
    }
  itk::NeighborhoodIterator<ImageType> nit( nSize, image,
    image->GetLargestPossibleRegion() ) ;
// for each location in nitSearch, compute the correlation
// of the intvec with the nit neighborhood
  nit.SetLocation( ind );
  for( unsigned int i = 0; i < nit.Size(); i++ )
    {
    typename ImageType::IndexType ind2 = nit.GetIndex(i);
    PixelType lval = image->GetPixel( ind2 );
    image->SetPixel( ind2, lval + intvec[i] );
    }
  return 0;
}




// addNeighborhoodToImage( posteriorList[[segct]], cent, rad, lsegprobs )
RcppExport SEXP addNeighborhoodToImage( SEXP r_antsimage, SEXP r_cent, SEXP r_rad, SEXP r_vec  )
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
    addNeighborhoodToImageHelper< ImageType>(
      r_antsimage, r_cent, r_rad, r_vec
      );
    return 0;
  }
  else if ( ( pixeltype == "float" ) & ( dimension == 3 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim >  ImageType;
    addNeighborhoodToImageHelper< ImageType>(
      r_antsimage, r_cent, r_rad, r_vec
      );
    return 0;
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
