#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>
#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageMomentsCalculator.h"
#include "itkResampleImageFilter.h"
#include "itkTransformFileWriter.h"
#include "vnl/vnl_inverse.h"


template< unsigned int ImageDimension >
void antsReoHelper(
  typename itk::Image< float , ImageDimension >::Pointer image1,
  SEXP r_txfn, SEXP r_axis , SEXP r_axis2, SEXP r_reflect, SEXP r_scale )
{
  typedef double RealType;
  Rcpp::NumericVector axis( r_axis );
  Rcpp::NumericVector axis2( r_axis2 );
  Rcpp::NumericVector doReflection( r_reflect );
  Rcpp::NumericVector doScale( r_scale );
  typedef itk::Image< float , ImageDimension > ImageType;
  typedef typename ImageType::Pointer ImagePointerType;
  if( image1.IsNotNull()  )
    {
    typedef typename itk::ImageMomentsCalculator<ImageType> ImageCalculatorType;
    typedef itk::AffineTransform<RealType, ImageDimension> AffineType;
    typedef typename ImageCalculatorType::MatrixType       MatrixType;
    typedef itk::Vector<float, ImageDimension>  VectorType;
    VectorType ccg1;
    VectorType cpm1;
    MatrixType cpa1;
    VectorType ccg2;
    VectorType cpm2;
    MatrixType cpa2;
    typename ImageCalculatorType::Pointer calculator1 =
      ImageCalculatorType::New();
    calculator1->SetImage(  image1 );
    typename ImageCalculatorType::VectorType fixed_center;
    fixed_center.Fill(0);
    try
      {
      calculator1->Compute();
      fixed_center = calculator1->GetCenterOfGravity();
      ccg1 = calculator1->GetCenterOfGravity();
      cpm1 = calculator1->GetPrincipalMoments();
      cpa1 = calculator1->GetPrincipalAxes();
      }
    catch( ... )
      {
      std::cerr << " zero image1 error ";
      }
    unsigned int eigind1 = 1;
    unsigned int eigind2 = 1;
    typedef vnl_vector<RealType> EVectorType;
    typedef vnl_matrix<RealType> EMatrixType;
    EVectorType evec1_2ndary = cpa1.GetVnlMatrix().get_row( eigind2 );
    EVectorType evec1_primary = cpa1.GetVnlMatrix().get_row( eigind1 );
    EVectorType evec2_2ndary;
    evec2_2ndary.set_size( ImageDimension );
    evec2_2ndary.fill(0);
    EVectorType evec2_primary;
    evec2_primary.set_size( ImageDimension );
    evec2_primary.fill(0);
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      evec2_primary[i] = axis[i];
    /** Solve Wahba's problem http://en.wikipedia.org/wiki/Wahba%27s_problem */
    EMatrixType B = outer_product( evec2_primary, evec1_primary );
    if( ImageDimension == 3 )
      {
      for ( unsigned int i = 0; i < ImageDimension; i++ )
          evec2_primary[i] = axis2[i];
      B = outer_product( evec2_2ndary, evec1_2ndary )
        + outer_product( evec2_primary, evec1_primary );
      }
      vnl_svd<RealType>    wahba( B );
      vnl_matrix<RealType> A_solution = wahba.V() * wahba.U().transpose();
      A_solution = vnl_inverse( A_solution );
      RealType det = vnl_determinant( A_solution  );
      if( det < 0 )
        {
        vnl_matrix<RealType> id( A_solution );
        id.set_identity();
        for( unsigned int i = 0; i < ImageDimension; i++ )
          {
          if( A_solution( i, i ) < 0 )
            {
            id( i, i ) = -1.0;
            }
          }
          A_solution =  A_solution * id.transpose();
        }
      if ( doReflection[0] == 1 ||  doReflection[0] == 3 )
        {
        vnl_matrix<RealType> id( A_solution );
        id.set_identity();
        id = id - 2.0 * outer_product( evec2_primary , evec2_primary  );
        A_solution = A_solution * id;
        }
      if ( doReflection[0] > 1 )
        {
        vnl_matrix<RealType> id( A_solution );
        id.set_identity();
        id = id - 2.0 * outer_product( evec1_primary , evec1_primary  );
        A_solution = A_solution * id;
        }
      if ( doScale[0] > 0 )
        {
        vnl_matrix<RealType> id( A_solution );
        id.set_identity();
        id = id * doScale[0];
        A_solution = A_solution * id;
        }
      det = vnl_determinant( A_solution  );
      Rcpp::Rcout << " det " << det << std::endl;
      Rcpp::Rcout << " A_solution " << std::endl;
      Rcpp::Rcout << A_solution << std::endl;
      typename AffineType::Pointer affine1 = AffineType::New();
      typename AffineType::OffsetType trans = affine1->GetOffset();
      itk::Point<RealType, ImageDimension> trans2;
      trans2.Fill(0);
      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        trans2[i] =  fixed_center[i] * ( 1 );
        }
      affine1->SetIdentity();
      affine1->SetOffset( trans );
      affine1->SetMatrix( A_solution );
      affine1->SetCenter( trans2 );
      // write tx
      typedef itk::TransformFileWriter TransformWriterType;
      typename TransformWriterType::Pointer transformWriter = TransformWriterType::New();
      transformWriter->SetInput( affine1 );
      transformWriter->SetFileName( Rcpp::as< std::string >(r_txfn).c_str() );
      transformWriter->Update();
      /*
      typedef itk::ResampleImageFilter<ImageType, ImageType> ResampleFilterType;
      typename ResampleFilterType::Pointer resample = ResampleFilterType::New();
      resample->SetTransform( affine1 );
      resample->SetInput( image1 );
      resample->SetOutputParametersFromImage(  image1 );
      resample->SetDefaultPixelValue( 0 );
      resample->UpdateLargestPossibleRegion();
      image2 = resample->GetOutput();
      */
  }
  else
  {
    return;
  }
}


RcppExport SEXP reorientImage( SEXP r_in_image1, SEXP r_txfn,
  SEXP r_axis1, SEXP r_axis2, SEXP rrfl, SEXP rscl )
{
  if( r_in_image1 == NULL  )
    {
      Rcpp::Rcout << "Invalid Arguments: pass 2 images in " << std::endl ;
      Rcpp::wrap( 1 ) ;
    }
  Rcpp::S4 in_image1( r_in_image1 ) ;
  std::string in_pixeltype = Rcpp::as< std::string >(
    in_image1.slot( "pixeltype" ) ) ;
  unsigned int dimension = Rcpp::as< unsigned int >(
    in_image1.slot( "dimension" ) ) ;
  if ( dimension == 2 )
    {
    typedef itk::Image< float , 2 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    antsReoHelper<2>(*antsimage_xptr1, r_txfn,
      r_axis1, r_axis2, rrfl, rscl );
    }
  else if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType3;
    typedef ImageType3::Pointer ImagePointerType3;
    Rcpp::XPtr< ImagePointerType3 > antsimage_xptr1_3(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    antsReoHelper<3>(*antsimage_xptr1_3, r_txfn,
      r_axis1, r_axis2, rrfl, rscl );
    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType4;
    typedef ImageType4::Pointer ImagePointerType4;
    Rcpp::XPtr< ImagePointerType4 > antsimage_xptr1_4(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    antsReoHelper<4>(*antsimage_xptr1_4, r_txfn,
      r_axis1, r_axis2, rrfl, rscl );
    }
  else Rcpp::Rcout << " Dimension " << dimension << " not supported " << std::endl;
  return 0;
}
