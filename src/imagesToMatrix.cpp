#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkImageRegionIteratorWithIndex.h"

template< unsigned int Dimension >
Rcpp::NumericMatrix imagesToMatrixHelper( std::vector< std::string > fns,
  typename itk::Image< float , Dimension >::Pointer mask,
  unsigned int n )
{
  typedef itk::Image< float , Dimension > ImageType;
  typedef typename ImageType::Pointer ImagePointerType;
  if( mask.IsNotNull() )
    {
    // count the voxels
    unsigned long voxct = 0;
    typedef itk::ImageRegionIteratorWithIndex<ImageType>
      Iterator;
    Iterator      mIter( mask, mask->GetLargestPossibleRegion() );
    for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
      {
      if( mIter.Get() >= 0.5 )
        {
        voxct++;
        }
      }
    Rcpp::NumericMatrix mat(n, voxct);
    for ( unsigned int i = 0; i < n; i++ )
      {
      typedef itk::ImageFileReader<ImageType> ReaderType;
      typename ReaderType::Pointer reader = ReaderType::New();
      reader->SetFileName( fns[i] );
      reader->Update();
      ImagePointerType rimg = reader->GetOutput();
      voxct = 0;
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
        {
        if( mIter.Get() >= 0.5 )
          {
          mat( i, voxct ) = rimg->GetPixel( mIter.GetIndex() );
          voxct++;
          }
        }
      }
    return(mat);
    }
  else
    {
    return 0;
    }
}

// [[Rcpp::export]]
RcppExport SEXP imagesToMatrix( SEXP r_fns, SEXP r_mask,
  SEXP r_n )
{
  if( r_mask == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass mask image in " << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  typedef std::vector< std::string > vecstring;
  std::vector< std::string > fns =
    Rcpp::as< vecstring >( r_fns );
  unsigned int n = Rcpp::as< unsigned int >( r_n );
  Rcpp::S4 in_mask( r_mask ) ;
  std::string in_pixeltype = Rcpp::as< std::string >(
    in_mask.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >(
    in_mask.slot( "dimension" ) ) ;
  std::string in_pixeltype2 = std::string("float");
  if (  in_pixeltype.compare(in_pixeltype2) != 0  )
    {
    Rcpp::Rcout << "Images must have float pixel type" << std::endl ;
    Rcpp::wrap( 1 );
    }
  if ( dimension == 2 )
    {
    typedef itk::Image< float , 2 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_mask.slot( "pointer" ) ) ) ;
    return imagesToMatrixHelper<2>( fns, *antsimage_xptr1, n );
    }
  else if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_mask.slot( "pointer" ) ) ) ;
    return imagesToMatrixHelper<3>( fns, *antsimage_xptr1, n );
    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_mask.slot( "pointer" ) ) ) ;
    return imagesToMatrixHelper<4>( fns, *antsimage_xptr1, n );
    }
  else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return Rcpp::wrap( 0 );
}
