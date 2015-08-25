#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "antsSCCANObject.h"
using namespace Rcpp;

RcppExport SEXP robustMatrixTransform( SEXP r_matrix )
{
try
{
  typedef double RealType;
  NumericMatrix M = as< NumericMatrix >( r_matrix );
  NumericMatrix outMat( M.rows(), M.cols() );
  unsigned long rows = M.rows();
  for( unsigned long j = 0; j < M.cols(); j++ )
    {
    NumericVector Mvec = M(_, j);
    NumericVector rank = M(_, j);
    for( unsigned int i = 0; i < rows; i++ )
      {
      RealType   rankval = 0;
      RealType   xi = Mvec(i);
      for( unsigned int k = 0; k < rows; k++ )
        {
        RealType yi = Mvec(k);
        RealType diff = fabs(xi - yi);
        if( diff > 0 )
          {
          RealType val = (xi - yi) / diff;
          rankval += val;
          }
        }
      rank(i) = rankval / rows;
      }
    outMat(_, j) = rank;
    }
  // this passes rcpp data to vnl matrix ... safely?
  if ( 1 == 0 )
    {
    // Further notes on this:
    // 1. see multiChannel cpp for how to pass image list
    // 2. separate implementation for eanat and sccan
    // 3. deal with output as only matrix?
    // 4. .... ?
    std::vector<RealType> dat =
      Rcpp::as< std::vector<RealType> >( outMat );
    const double* _data = &dat[0];
    vnl_matrix<RealType> vnlmat( _data , M.cols(), M.rows()  );
    vnlmat = vnlmat.transpose();
    }
  return wrap( outMat );
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




template< class ImageType, class IntType, class RealType >
SEXP eigenanatomyCppHelper(
  NumericMatrix X,
  SEXP r_mask,
  RealType sparseness,
  IntType nvecs,
  IntType its,
  IntType cthresh,
  RealType z,
  RealType smooth,
//  NumericMatrix initializationMatrix,
  Rcpp::List initializationList,
  IntType covering,
  RealType ell1,
  IntType verbose,
  IntType powerit,
  RealType priorWeight )
{
  enum { Dimension = ImageType::ImageDimension };
  typename ImageType::RegionType region;
  typedef typename ImageType::PixelType PixelType;
  typedef typename ImageType::Pointer ImagePointerType;
  typedef double                                        Scalar;
  typedef itk::ants::antsSCCANObject<ImageType, Scalar> SCCANType;
  typedef typename SCCANType::MatrixType                vMatrix;
  typedef typename SCCANType::VectorType                vVector;
  typename SCCANType::Pointer sccanobj = SCCANType::New();

  typename ImageType::Pointer mask = Rcpp::as<ImagePointerType>( r_mask );
  bool maskisnull = mask.IsNull();
// deal with the initializationList, if any
  unsigned int nImages = initializationList.size();
  if ( ( nImages > 0 ) && ( !maskisnull ) )
    {
    itk::ImageRegionIteratorWithIndex<ImageType> it( mask,
      mask->GetLargestPossibleRegion() );
    vMatrix priorROIMat( nImages , X.cols() );
    priorROIMat.fill( 0 );
    for ( unsigned int i = 0; i < nImages; i++ )
      {
      typename ImageType::Pointer init =
        Rcpp::as<ImagePointerType>( initializationList[i] );
      unsigned long ct = 0;
      it.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        PixelType pix = it.Get();
        if ( pix >= 0.5 )
          {
          pix = init->GetPixel( it.GetIndex() );
          priorROIMat( i, ct ) = pix;
          ct++;
          }
        ++it;
        }
      }
    sccanobj->SetMatrixPriorROI( priorROIMat );
    nvecs = nImages;
    }
  sccanobj->SetPriorWeight( priorWeight );
  sccanobj->SetLambda( priorWeight );
// cast hack from Rcpp type to sccan type
  std::vector<double> xdat =
      Rcpp::as< std::vector<double> >( X );
  const double* _xdata = &xdat[0];
  vMatrix vnlX( _xdata , X.cols(), X.rows()  );
  vnlX = vnlX.transpose();

  sccanobj->SetGetSmall( false  );
  vMatrix priorROIMat;

//    sccanobj->SetMatrixPriorROI( priorROIMat);
//    sccanobj->SetMatrixPriorROI2( priorROIMat );
  sccanobj->SetCovering( covering );
  sccanobj->SetSilent(  ! verbose  );
  if( ell1 > 0 )
    {
    sccanobj->SetUseL1( true );
    }
  else
    {
    sccanobj->SetUseL1( false );
    }
  sccanobj->SetGradStep( vnl_math_abs( ell1 ) );
  sccanobj->SetMaximumNumberOfIterations( its );
  sccanobj->SetRowSparseness( z );
  sccanobj->SetSmoother( smooth );
  if ( sparseness < 0 ) sccanobj->SetKeepPositiveP(false);
  sccanobj->SetSCCANFormulation(  SCCANType::PQ );
  sccanobj->SetFractionNonZeroP( fabs( sparseness ) );
  sccanobj->SetMinClusterSizeP( cthresh );
  sccanobj->SetMatrixP( vnlX );
//  sccanobj->SetMatrixR( r ); // FIXME
  sccanobj->SetMaskImageP( mask );
  RealType truecorr = 0;
  if( powerit == 1 )
    {
    truecorr = sccanobj->SparseReconHome( nvecs );
    }
  else if ( priorWeight > 1.e-12 )
    truecorr = sccanobj->SparseReconPrior( nvecs, true );
  else truecorr = sccanobj->SparseRecon(nvecs);
  /*
  else if( powerit != 0 )
    {
    truecorr = sccanobj->SparseArnoldiSVD(nvecs);
    }
  else if( svd_option == 4  )
    {
    truecorr = sccanobj->NetworkDecomposition( nvecs );
    }
  else if( svd_option == 5  )
    {
    truecorr = sccanobj->LASSO( nvecs );
    }
  else if( svd_option == 2 )
    {
    truecorr = sccanobj->CGSPCA(nvecs);                         // cgspca
    }
  else if( svd_option == 6 )
    {
    truecorr = sccanobj->SparseRecon(nvecs);  // sparse (default)
    }
  else if( svd_option == 7 )
    {
    // sccanobj->SetPriorScaleMat( priorScaleMat);
    sccanobj->SetMatrixPriorROI( priorROIMat);
    sccanobj->SetFlagForSort();
    sccanobj->SetLambda(sccanparser->Convert<double>( option->GetFunction( 0 )->GetParameter( 3 ) ) );
    truecorr = sccanobj->SparseReconPrior(nvecs, true); // Prior
  }
  else
    {
    truecorr = sccanobj->SparseArnoldiSVDGreedy( nvecs );  // sparse (default)
    }
  */

  // solutions should be much smaller so may not be a big deal to copy
  // FIXME - should not copy, should map memory
  vMatrix solV = sccanobj->GetVariatesP();
  NumericMatrix eanatMat( solV.cols(), solV.rows() );
  unsigned long rows = solV.rows();
  for( unsigned long c = 0; c < solV.cols(); c++ )
    {
    for( unsigned int r = 0; r < rows; r++ )
      {
      eanatMat( c, r ) = solV( r, c );
      }
    }
  vMatrix solU = sccanobj->GetMatrixU();
  NumericMatrix eanatMatU( solU.rows(), solU.cols() );
  rows = solU.rows();
  for( unsigned long c = 0; c < solU.cols(); c++ )
    {
    for( unsigned int r = 0; r < rows; r++ )
      {
      eanatMatU( r, c) = solU( r, c);
      }
    }
  return(
      Rcpp::List::create(
        Rcpp::Named("eigenanatomyimages") = eanatMat,
        Rcpp::Named("umatrix") = eanatMatU,
        Rcpp::Named("varex") = truecorr )
      );
}

RcppExport SEXP eigenanatomyCpp(
  SEXP r_X,
  SEXP r_mask,
  SEXP r_sparseness,
  SEXP r_nvecs,
  SEXP r_its,
  SEXP r_cthresh,
  SEXP r_z,
  SEXP r_smooth,
  SEXP r_initializationList,
  SEXP r_mycoption,
  SEXP r_ell1,
  SEXP r_verbose,
  SEXP r_powerit,
  SEXP r_priorWeight )
{
try
{
  typedef float RealType;
  typedef unsigned int IntType;
  NumericMatrix X = as< NumericMatrix >( r_X );
  Rcpp::S4 mask( r_mask );
  IntType dimension = Rcpp::as< IntType >( mask.slot( "dimension" ) ) ;
  RealType sparseness = Rcpp::as< RealType >( r_sparseness );
  IntType nvecs = Rcpp::as< RealType >( r_nvecs );
  IntType its = Rcpp::as< RealType >( r_its );
  IntType cthresh = Rcpp::as< RealType >( r_cthresh );
  RealType z = Rcpp::as< RealType >( r_z );
  RealType smooth = Rcpp::as< RealType >( r_smooth );
  Rcpp::List initializationList( r_initializationList );
  IntType mycoption = Rcpp::as< IntType >( r_mycoption );
  RealType ell1 = Rcpp::as< RealType >( r_ell1 );
  IntType verbose = Rcpp::as< RealType >( r_verbose );
  IntType powerit = Rcpp::as< RealType >( r_powerit );
  RealType priorWeight = Rcpp::as< RealType >( r_priorWeight );

//[1] "projections"        "eigenanatomyimages" "umatrix"
  typedef itk::Image<RealType,3> Image3Type;
  typedef itk::Image<RealType,2> Image2Type;
  if ( dimension == 2 )
    return wrap(
      eigenanatomyCppHelper<Image2Type,IntType,RealType>(
        X,
        r_mask,
        sparseness,
        nvecs,
        its,
        cthresh,
        z,
        smooth,
        initializationList,
        mycoption,
        ell1,
        verbose,
        powerit,
        priorWeight
        )
      );
  if ( dimension == 3 )
    return wrap(
      eigenanatomyCppHelper<Image3Type,IntType,RealType>(
        X,
        r_mask,
        sparseness,
        nvecs,
        its,
        cthresh,
        z,
        smooth,
        initializationList,
        mycoption,
        ell1,
        verbose,
        powerit,
        priorWeight
        )
      );
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







template< class ImageType, class IntType, class RealType >
SEXP sccanCppHelper(
  NumericMatrix X,
  NumericMatrix Y,
  SEXP r_maskx,
  SEXP r_masky,
  RealType sparsenessx,
  RealType sparsenessy,
  IntType nvecs,
  IntType its,
  IntType cthreshx,
  IntType cthreshy,
  RealType z,
  RealType smooth,
  Rcpp::List initializationListx,
  Rcpp::List initializationListy,
  IntType covering,
  RealType ell1,
  IntType verbose,
  RealType priorWeight )
{
  enum { Dimension = ImageType::ImageDimension };
  typename ImageType::RegionType region;
  typedef typename ImageType::PixelType PixelType;
  typedef typename ImageType::Pointer ImagePointerType;
  typedef double                                        Scalar;
  typedef itk::ants::antsSCCANObject<ImageType, Scalar> SCCANType;
  typedef typename SCCANType::MatrixType                vMatrix;
  typedef typename SCCANType::VectorType                vVector;
  typename SCCANType::Pointer sccanobj = SCCANType::New();

  typename ImageType::Pointer maskx = Rcpp::as<ImagePointerType>( r_maskx );
  typename ImageType::Pointer masky = Rcpp::as<ImagePointerType>( r_masky );

  bool maskxisnull = maskx.IsNull();
  bool maskyisnull = masky.IsNull();
// deal with the initializationList, if any
  unsigned int nImagesx = initializationListx.size();
  if ( ( nImagesx > 0 ) && ( !maskxisnull ) )
    {
    itk::ImageRegionIteratorWithIndex<ImageType> it( maskx,
      maskx->GetLargestPossibleRegion() );
    vMatrix priorROIMatx( nImagesx , X.cols() );
    priorROIMatx.fill( 0 );
    for ( unsigned int i = 0; i < nImagesx; i++ )
      {
      typename ImageType::Pointer init =
        Rcpp::as<ImagePointerType>( initializationListx[i] );
      unsigned long ct = 0;
      it.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        PixelType pix = it.Get();
        if ( pix >= 0.5 )
          {
          pix = init->GetPixel( it.GetIndex() );
          priorROIMatx( i, ct ) = pix;
          ct++;
          }
        ++it;
        }
      }
    sccanobj->SetMatrixPriorROI( priorROIMatx );
    nvecs = nImagesx;
    }
  unsigned int nImagesy = initializationListy.size();
  if ( ( nImagesy > 0 ) && ( !maskyisnull ) )
    {
    itk::ImageRegionIteratorWithIndex<ImageType> it( masky,
      masky->GetLargestPossibleRegion() );
    vMatrix priorROIMaty( nImagesy , Y.cols() );
    priorROIMaty.fill( 0 );
    for ( unsigned int i = 0; i < nImagesy; i++ )
      {
      typename ImageType::Pointer init =
        Rcpp::as<ImagePointerType>( initializationListy[i] );
      unsigned long ct = 0;
      it.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        PixelType pix = it.Get();
        if ( pix >= 0.5 )
          {
          pix = init->GetPixel( it.GetIndex() );
          priorROIMaty( i, ct ) = pix;
          ct++;
          }
        ++it;
        }
      }
    sccanobj->SetMatrixPriorROI2( priorROIMaty );
    nvecs = nImagesy;
    }

  sccanobj->SetPriorWeight( priorWeight );
  sccanobj->SetLambda( priorWeight );
// cast hack from Rcpp type to sccan type
  std::vector<double> xdat =
      Rcpp::as< std::vector<double> >( X );
  const double* _xdata = &xdat[0];
  vMatrix vnlX( _xdata , X.cols(), X.rows()  );
  vnlX = vnlX.transpose();
  std::vector<double> ydat =
      Rcpp::as< std::vector<double> >( Y );
  const double* _ydata = &ydat[0];
  vMatrix vnlY( _ydata , Y.cols(), Y.rows()  );
  vnlY = vnlY.transpose();
// cast hack done
  sccanobj->SetGetSmall( false  );
  sccanobj->SetCovering( covering );
  sccanobj->SetSilent(  ! verbose  );
  if( ell1 > 0 )
    {
    sccanobj->SetUseL1( true );
    }
  else
    {
    sccanobj->SetUseL1( false );
    }
  sccanobj->SetGradStep( vnl_math_abs( ell1 ) );
  sccanobj->SetMaximumNumberOfIterations( its );
  sccanobj->SetRowSparseness( z );
  sccanobj->SetSmoother( smooth );
  if ( sparsenessx < 0 ) sccanobj->SetKeepPositiveP(false);
  if ( sparsenessy < 0 ) sccanobj->SetKeepPositiveQ(false);
  sccanobj->SetSCCANFormulation(  SCCANType::PQ );
  sccanobj->SetFractionNonZeroP( fabs( sparsenessx ) );
  sccanobj->SetFractionNonZeroQ( fabs( sparsenessy ) );
  sccanobj->SetMinClusterSizeP( cthreshx );
  sccanobj->SetMinClusterSizeQ( cthreshy );
  sccanobj->SetMatrixP( vnlX );
  sccanobj->SetMatrixQ( vnlY );
//  sccanobj->SetMatrixR( r ); // FIXME
  sccanobj->SetMaskImageP( maskx );
  sccanobj->SetMaskImageQ( masky );
  sccanobj->SparsePartialArnoldiCCA( nvecs );

  // FIXME - should not copy, should map memory
  vMatrix solP = sccanobj->GetVariatesP();
  NumericMatrix eanatMatp( solP.cols(), solP.rows() );
  unsigned long rows = solP.rows();
  for( unsigned long c = 0; c < solP.cols(); c++ )
    {
    for( unsigned int r = 0; r < rows; r++ )
      {
      eanatMatp( c, r ) = solP( r, c );
      }
    }

  vMatrix solQ = sccanobj->GetVariatesQ();
  NumericMatrix eanatMatq( solQ.cols(), solQ.rows() );
  rows = solQ.rows();
  for( unsigned long c = 0; c < solQ.cols(); c++ )
    {
    for( unsigned int r = 0; r < rows; r++ )
      {
      eanatMatq( c, r ) = solQ( r, c );
      }
    }

  return(
      Rcpp::List::create(
        Rcpp::Named("eig1") = eanatMatp,
        Rcpp::Named("eig2") = eanatMatq )
      );
}



RcppExport SEXP sccanCpp(
  SEXP r_X,
  SEXP r_Y,
  SEXP r_maskx,
  SEXP r_masky,
  SEXP r_sparsenessx,
  SEXP r_sparsenessy,
  SEXP r_nvecs,
  SEXP r_its,
  SEXP r_cthreshx,
  SEXP r_cthreshy,
  SEXP r_z,
  SEXP r_smooth,
  SEXP r_initializationListx,
  SEXP r_initializationListy,
  SEXP r_mycoption,
  SEXP r_ell1,
  SEXP r_verbose,
  SEXP r_priorWeight )
{
try
{
  typedef float RealType;
  typedef unsigned int IntType;
  NumericMatrix X = as< NumericMatrix >( r_X );
  NumericMatrix Y = as< NumericMatrix >( r_Y );
  Rcpp::S4 maskx( r_maskx );
  IntType dimension = Rcpp::as< IntType >( maskx.slot( "dimension" ) ) ;
  RealType sparsenessx = Rcpp::as< RealType >( r_sparsenessx );
  RealType sparsenessy = Rcpp::as< RealType >( r_sparsenessy );
  IntType nvecs = Rcpp::as< RealType >( r_nvecs );
  IntType its = Rcpp::as< RealType >( r_its );
  IntType cthreshx = Rcpp::as< RealType >( r_cthreshx );
  IntType cthreshy = Rcpp::as< RealType >( r_cthreshy );
  RealType z = Rcpp::as< RealType >( r_z );
  RealType smooth = Rcpp::as< RealType >( r_smooth );
  Rcpp::List initializationListx( r_initializationListx );
  Rcpp::List initializationListy( r_initializationListy );
  IntType mycoption = Rcpp::as< IntType >( r_mycoption );
  RealType ell1 = Rcpp::as< RealType >( r_ell1 );
  IntType verbose = Rcpp::as< RealType >( r_verbose );
  RealType priorWeight = Rcpp::as< RealType >( r_priorWeight );
  typedef itk::Image<RealType,3> Image3Type;
  typedef itk::Image<RealType,2> Image2Type;
  if ( dimension == 2 )
    return wrap(
      sccanCppHelper<Image2Type,IntType,RealType>(
        X,
        Y,
        r_maskx,
        r_masky,
        sparsenessx,
        sparsenessy,
        nvecs,
        its,
        cthreshx,
        cthreshy,
        z,
        smooth,
        initializationListx,
        initializationListy,
        mycoption,
        ell1,
        verbose,
        priorWeight
        )
      );
  if ( dimension == 3 )
    return wrap(
      sccanCppHelper<Image3Type,IntType,RealType>(
        X,
        Y,
        r_maskx,
        r_masky,
        sparsenessx,
        sparsenessy,
        nvecs,
        its,
        cthreshx,
        cthreshy,
        z,
        smooth,
        initializationListx,
        initializationListy,
        mycoption,
        ell1,
        verbose,
        priorWeight
        )
      );
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
