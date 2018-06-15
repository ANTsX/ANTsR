#include <exception>
#include <vector>
#include <string>
#include <algorithm>
#include <ants.h>
#include "antsUtilities.h"
#include "ReadWriteData.h"
#include "itkDeformationFieldGradientTensorImageFilter.h"
#include "itkDeterminantTensorImageFilter.h"
#include "itkGeometricJacobianDeterminantImageFilter.h"
#include "itkLogImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkCastImageFilter.h"
#include "RcppANTsR.h"


template< class ImageType >
SEXP cdjHelper(
    SEXP r_inimg,
    SEXP r_outimg,
    std::string txfn,
    bool dolog,
    bool dogeom )
{
  enum { ImageDimension = ImageType::ImageDimension };
  typedef typename ImageType::Pointer       ImagePointerType;
  typedef typename ImageType::PixelType     PixelType;
  typedef double                            RealType;
  typename ImageType::Pointer domainImg =
    Rcpp::as< ImagePointerType >( r_inimg );
  typename ImageType::Pointer outimg =
    Rcpp::as< ImagePointerType >( r_outimg );
  typedef itk::Image<RealType, ImageDimension>   RealImageType;
  typedef itk::Vector<PixelType, ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension> VectorImageType;

  /**
   * Read in vector field
   */
  typedef itk::ImageFileReader<VectorImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( txfn.c_str() );
  reader->Update();

  typename RealImageType::Pointer jacobian = nullptr;

  typename RealImageType::Pointer minimumConstantImage = RealImageType::New();
  minimumConstantImage->CopyInformation( reader->GetOutput() );
  minimumConstantImage->SetRegions( reader->GetOutput()->GetRequestedRegion() );
  minimumConstantImage->Allocate();
  minimumConstantImage->FillBuffer( 0.001 );


  if( dogeom > 0 )
    {
    typedef itk::GeometricJacobianDeterminantImageFilter
      <VectorImageType, RealType, RealImageType> JacobianFilterType;
    typename JacobianFilterType::Pointer jacobianFilter = JacobianFilterType::New();
    jacobianFilter->SetInput( reader->GetOutput() );

    jacobian = jacobianFilter->GetOutput();
    jacobian->Update();
    jacobian->DisconnectPipeline();
    }
  else
    {
    typedef itk::DeformationFieldGradientTensorImageFilter<VectorImageType, RealType> JacobianFilterType;
    typename JacobianFilterType::Pointer jacobianFilter = JacobianFilterType::New();
    jacobianFilter->SetInput( reader->GetOutput() );
    jacobianFilter->SetCalculateJacobian( true );
    jacobianFilter->SetUseImageSpacing( true );
    jacobianFilter->SetOrder( 2 );
    jacobianFilter->SetUseCenteredDifference( true );

    typedef itk::DeterminantTensorImageFilter<typename JacobianFilterType::OutputImageType, RealType>
      DeterminantFilterType;
    typename DeterminantFilterType::Pointer determinantFilter = DeterminantFilterType::New();
    determinantFilter->SetInput( jacobianFilter->GetOutput() );
    determinantFilter->Update();

    minimumConstantImage->FillBuffer( 0.0 );

    typedef itk::MaximumImageFilter<RealImageType, RealImageType, RealImageType> MaxFilterType;
    typename MaxFilterType::Pointer maxFilter = MaxFilterType::New();
    maxFilter->SetInput1( determinantFilter->GetOutput() );
    maxFilter->SetInput2( minimumConstantImage );

    jacobian = maxFilter->GetOutput();
    jacobian->Update();
    jacobian->DisconnectPipeline();
    }

  if ( dolog > 0 )
    {
    minimumConstantImage->FillBuffer( 0.001 );

    typedef itk::MaximumImageFilter<RealImageType, RealImageType, RealImageType> MaxFilterType;
    typename MaxFilterType::Pointer maxFilter = MaxFilterType::New();
    maxFilter->SetInput1( jacobian );
    maxFilter->SetInput2( minimumConstantImage );

    typedef itk::LogImageFilter<RealImageType, RealImageType> LogFilterType;
    typename LogFilterType::Pointer logFilter = LogFilterType::New();
    logFilter->SetInput( maxFilter->GetOutput() );
    logFilter->Update();
    typedef itk::CastImageFilter<RealImageType, ImageType> CasterType;
    typename CasterType::Pointer caster = CasterType::New();
    caster->SetInput( logFilter->GetOutput() );
    caster->Update();
    outimg = caster->GetOutput();
    r_outimg = Rcpp::wrap( outimg  );
    return( r_outimg );
    }
  else
    {
    typedef itk::CastImageFilter<RealImageType, ImageType> CasterType;
    typename CasterType::Pointer caster = CasterType::New();
    caster->SetInput( jacobian );
    caster->Update();
    outimg = caster->GetOutput();
    r_outimg = Rcpp::wrap( outimg  );
    return( r_outimg );
    }

  return Rcpp::wrap(NA_REAL);
}

RcppExport SEXP createJacobianDeterminantImageR(
  SEXP r_domainImg,
  SEXP r_tx,
  SEXP r_dolog,
  SEXP r_dogeom )
{
try
{
  Rcpp::S4 antsImage( r_domainImg );
  Rcpp::S4 r_outimg( r_domainImg );
  std::string pixeltype = Rcpp::as< std::string >( antsImage.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< int >( antsImage.slot( "dimension" ) );
  bool dolog = Rcpp::as< bool >( r_dolog );
  bool dogeom = Rcpp::as< bool >( r_dogeom );
  std::string txfn = Rcpp::as< std::string >( r_tx );

  if ( (pixeltype == "float") & ( dimension == 2 ) )
    {
    typedef float PixelType;
    const unsigned int dim = 2;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = cdjHelper< ImageType >(
        antsImage, r_outimg, txfn, dolog, dogeom);
    return( outimg );
    }
  else if ( (pixeltype == "float") & ( dimension == 3 ) )
    {
    typedef float PixelType;
    const unsigned int dim = 3;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = cdjHelper< ImageType >(
        antsImage, r_outimg, txfn, dolog, dogeom);
    return( outimg );
    }
  else if ( (pixeltype == "float") & ( dimension == 4 ) )
    {
    typedef float PixelType;
    const unsigned int dim = 4;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = cdjHelper< ImageType >(
        antsImage, r_outimg, txfn, dolog, dogeom);
    return( outimg );
    }
  else
    {
    Rcpp::stop("Unsupported image dimension or pixel type.");
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
 return Rcpp::wrap(NA_REAL); // should not be reached
}
