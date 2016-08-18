#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include <algorithm>
#include <vnl/vnl_inverse.h>
#include <vnl/vnl_cross.h>
#include "itkImageMaskSpatialObject.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkArray.h"
#include "itkGradientImageFilter.h"
#include "itkBSplineControlPointImageFilter.h"
#include "itkBayesianClassifierImageFilter.h"
#include "itkBayesianClassifierInitializationImageFilter.h"
#include "itkBilateralImageFilter.h"
#include "itkCSVNumericObjectFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkCompositeValleyFunction.h"
#include "itkConjugateGradientLineSearchOptimizerv4.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkConvolutionImageFilter.h"
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkDemonsImageToImageMetricv4.h"
#include "itkExpImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkGaussianImageSource.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkHistogram.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkImage.h"
#include "itkImageClassifierBase.h"
#include "itkImageDuplicator.h"
#include "itkImageFileWriter.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkImageKmeansModelEstimator.h"
#include "itkImageMomentsCalculator.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkLabelContourImageFilter.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkListSample.h"
#include "itkMRFImageFilter.h"
#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkMaskImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkMedianImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkMultiStartOptimizerv4.h"
#include "itkNeighborhood.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodIterator.h"
#include "itkNormalVariateGenerator.h"
#include "itkOptimizerParameterScalesEstimator.h"
#include "itkOtsuThresholdImageFilter.h"
#include "itkRGBPixel.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSampleToHistogramFilter.h"
#include "itkScalarImageKmeansImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkSimilarity3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkSize.h"
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkTranslationTransform.h"
#include "itkVariableSizeMatrix.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkTransformFactory.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkCenteredAffineTransform.h"
#include "itkCompositeTransform.h"

#include <fstream>
#include <iostream>
#include <map> // Here I'm using a map but you could choose even other containers
#include <sstream>
#include <string>

template <class TComputeType, unsigned int ImageDimension>
class SimilarityTransformTraits
{
  // Don't worry about the fact that the default option is the
  // affine Transform, that one will not actually be instantiated.
public:
  typedef itk::AffineTransform<TComputeType, ImageDimension> TransformType;
};


template <>
class SimilarityTransformTraits<double, 2>
{
public:
  typedef itk::Similarity2DTransform<double> TransformType;
};

template <>
class SimilarityTransformTraits<float, 2>
{
public:
  typedef itk::Similarity2DTransform<float> TransformType;
};

template <>
class SimilarityTransformTraits<double, 3>
{
public:
  typedef itk::Similarity3DTransform<double> TransformType;
};

template <>
class SimilarityTransformTraits<float, 3>
{
public:
  typedef itk::Similarity3DTransform<float> TransformType;
};


template< unsigned int ImageDimension, class AffineType >
SEXP invariantSimilarityHelper(
  typename itk::Image< float , ImageDimension >::Pointer image1,
  typename itk::Image< float , ImageDimension >::Pointer image2,
  SEXP r_thetas, SEXP r_thetas2, SEXP r_thetas3,
  SEXP r_lsits, SEXP r_WM, SEXP r_scale,
  SEXP r_doreflection, SEXP r_txfn  )
{
  unsigned int mibins = 20;
  unsigned int localSearchIterations =
    Rcpp::as< unsigned int >( r_lsits ) ;
  std::string whichMetric = Rcpp::as< std::string >( r_WM );
  std::string txfn = Rcpp::as< std::string >( r_txfn );
  bool useprincaxis = true;
  typedef typename itk::ImageMaskSpatialObject<ImageDimension>::ImageType
    maskimagetype;
  typename maskimagetype::Pointer mask = ITK_NULLPTR;
  Rcpp::NumericVector thetas( r_thetas );
  Rcpp::NumericVector thetas2( r_thetas2 );
  Rcpp::NumericVector thetas3( r_thetas3 );
  Rcpp::IntegerVector doReflection( r_doreflection );
  typedef float  PixelType;
  typedef double RealType;
  RealType bestscale = Rcpp::as< RealType >( r_scale ) ;
  typedef itk::Image< PixelType , ImageDimension > ImageType;
  if( image1.IsNotNull() & image2.IsNotNull() )
    {
    typedef typename itk::ImageMomentsCalculator<ImageType> ImageCalculatorType;
    typedef itk::AffineTransform<RealType, ImageDimension> AffineType0;
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
    typename ImageCalculatorType::Pointer calculator2 =
      ImageCalculatorType::New();
    calculator1->SetImage(  image1 );
    calculator2->SetImage(  image2 );
    typename ImageCalculatorType::VectorType fixed_center;
    fixed_center.Fill(0);
    typename ImageCalculatorType::VectorType moving_center;
    moving_center.Fill(0);
    try
      {
      calculator1->Compute();
      fixed_center = calculator1->GetCenterOfGravity();
      ccg1 = calculator1->GetCenterOfGravity();
      cpm1 = calculator1->GetPrincipalMoments();
      cpa1 = calculator1->GetPrincipalAxes();
      try
        {
        calculator2->Compute();
        moving_center = calculator2->GetCenterOfGravity();
        ccg2 = calculator2->GetCenterOfGravity();
        cpm2 = calculator2->GetPrincipalMoments();
        cpa2 = calculator2->GetPrincipalAxes();
        }
      catch( ... )
        {
        fixed_center.Fill(0);
        }
      }
    catch( ... )
      {
      // Rcpp::Rcerr << " zero image1 error ";
      }
    if ( vnl_math_abs( bestscale - 1.0 ) < 1.e-6 )
      {
      RealType volelt1 = 1;
      RealType volelt2 = 1;
      for ( unsigned int d=0; d<ImageDimension; d++)
        {
        volelt1 *= image1->GetSpacing()[d];
        volelt2 *= image2->GetSpacing()[d];
        }
      bestscale =
        ( calculator2->GetTotalMass() * volelt2 )/
        ( calculator1->GetTotalMass() * volelt1 );
      RealType powlev = 1.0 / static_cast<RealType>(ImageDimension);
      bestscale = vcl_pow( bestscale , powlev );
    }
    unsigned int eigind1 = 1;
    unsigned int eigind2 = 1;
    if( ImageDimension == 3 )
      {
      eigind1 = 2;
      }
    typedef vnl_vector<RealType> EVectorType;
    typedef vnl_matrix<RealType> EMatrixType;
    EVectorType evec1_2ndary = cpa1.GetVnlMatrix().get_row( eigind2 );
    EVectorType evec1_primary = cpa1.GetVnlMatrix().get_row( eigind1 );
    EVectorType evec2_2ndary  = cpa2.GetVnlMatrix().get_row( eigind2 );
    EVectorType evec2_primary = cpa2.GetVnlMatrix().get_row( eigind1 );
    /** Solve Wahba's problem http://en.wikipedia.org/wiki/Wahba%27s_problem */
    EMatrixType B = outer_product( evec2_primary, evec1_primary );
    if( ImageDimension == 3 )
      {
      B = outer_product( evec2_2ndary, evec1_2ndary )
        + outer_product( evec2_primary, evec1_primary );
      }
    vnl_svd<RealType>    wahba( B );
    vnl_matrix<RealType> A_solution = wahba.V() * wahba.U().transpose();
    A_solution = vnl_inverse( A_solution );
    RealType det = vnl_determinant( A_solution  );
    if( ( det < 0 ) )
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
    typename AffineType::Pointer affine1 = AffineType::New();
    typename AffineType::OffsetType trans = affine1->GetOffset();
    itk::Point<RealType, ImageDimension> trans2;
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      trans[i] = moving_center[i] - fixed_center[i];
      trans2[i] =  fixed_center[i] * ( 1 );
      }
    affine1->SetIdentity();
    affine1->SetOffset( trans );
    if( useprincaxis )
      {
      affine1->SetMatrix( A_solution );
      }
    affine1->SetCenter( trans2 );
    if( ImageDimension > 3  )
      {
      return EXIT_SUCCESS;
      }
    vnl_vector<RealType> evec_tert;
    if( ImageDimension == 3 )
      { // try to rotate around tertiary and secondary axis
      evec_tert = vnl_cross_3d( evec1_primary, evec1_2ndary );
      }
    if( ImageDimension == 2 )
      { // try to rotate around tertiary and secondary axis
      evec_tert = evec1_2ndary;
      evec1_2ndary = evec1_primary;
      }
    itk::Vector<RealType, ImageDimension> axis1;
    itk::Vector<RealType, ImageDimension> axis2;
    itk::Vector<RealType, ImageDimension> axis3;
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      axis1[d] = evec_tert[d];
      axis2[d] = evec1_2ndary[d];
      axis3[d] = evec1_primary[d];
      }
    typename AffineType::Pointer simmer = AffineType::New();
    simmer->SetIdentity();
    simmer->SetCenter( trans2 );
    simmer->SetOffset( trans );
    typename AffineType0::Pointer affinesearch = AffineType0::New();
    affinesearch->SetIdentity();
    affinesearch->SetCenter( trans2 );
    typedef  itk::MultiStartOptimizerv4         OptimizerType;
    typename OptimizerType::MetricValuesListType metricvalues;
    typename OptimizerType::Pointer  mstartOptimizer = OptimizerType::New();
    typedef itk::CorrelationImageToImageMetricv4
      <ImageType, ImageType, ImageType> GCMetricType;
    typedef itk::MattesMutualInformationImageToImageMetricv4
      <ImageType, ImageType, ImageType> MetricType;
    typename MetricType::ParametersType newparams(  affine1->GetParameters() );
    typename GCMetricType::Pointer gcmetric = GCMetricType::New();
    gcmetric->SetFixedImage( image1 );
    gcmetric->SetVirtualDomainFromImage( image1 );
    gcmetric->SetMovingImage( image2 );
    gcmetric->SetMovingTransform( simmer );
    gcmetric->SetParameters( newparams );
    typename MetricType::Pointer mimetric = MetricType::New();
    mimetric->SetNumberOfHistogramBins( mibins );
    mimetric->SetFixedImage( image1 );
    mimetric->SetMovingImage( image2 );
    mimetric->SetMovingTransform( simmer );
    mimetric->SetParameters( newparams );
    if( mask.IsNotNull() )
      {
      typename itk::ImageMaskSpatialObject<ImageDimension>::Pointer so =
        itk::ImageMaskSpatialObject<ImageDimension>::New();
      so->SetImage( const_cast<maskimagetype *>( mask.GetPointer() ) );
      mimetric->SetFixedImageMask( so );
      gcmetric->SetFixedImageMask( so );
      }
    typedef  itk::ConjugateGradientLineSearchOptimizerv4 LocalOptimizerType;
    typename LocalOptimizerType::Pointer  localoptimizer =
      LocalOptimizerType::New();
    RealType     localoptimizerlearningrate = 0.1;
    localoptimizer->SetLearningRate( localoptimizerlearningrate );
    localoptimizer->SetMaximumStepSizeInPhysicalUnits(
      localoptimizerlearningrate );
    localoptimizer->SetNumberOfIterations( localSearchIterations );
    localoptimizer->SetLowerLimit( 0 );
    localoptimizer->SetUpperLimit( 2 );
    localoptimizer->SetEpsilon( 0.1 );
    localoptimizer->SetMaximumLineSearchIterations( 50 );
    localoptimizer->SetDoEstimateLearningRateOnce( true );
    localoptimizer->SetMinimumConvergenceValue( 1.e-6 );
    localoptimizer->SetConvergenceWindowSize( 5 );
    if( true )
      {
      typedef typename MetricType::FixedSampledPointSetType PointSetType;
      typedef typename PointSetType::PointType              PointType;
      typename PointSetType::Pointer      pset(PointSetType::New());
      unsigned int ind=0;
      unsigned int ct=0;
      itk::ImageRegionIteratorWithIndex<ImageType> It(image1,
        image1->GetLargestPossibleRegion() );
      for( It.GoToBegin(); !It.IsAtEnd(); ++It )
        {
        // take every N^th point
        if ( ct % 10 == 0  )
          {
          PointType pt;
          image1->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
          pset->SetPoint(ind, pt);
          ind++;
          }
          ct++;
        }
      mimetric->SetFixedSampledPointSet( pset );
      mimetric->SetUseFixedSampledPointSet( true );
      gcmetric->SetFixedSampledPointSet( pset );
      gcmetric->SetUseFixedSampledPointSet( true );
    }
    if ( whichMetric.compare("MI") == 0  ) {
      mimetric->Initialize();
      typedef itk::RegistrationParameterScalesFromPhysicalShift<MetricType>
      RegistrationParameterScalesFromPhysicalShiftType;
      typename RegistrationParameterScalesFromPhysicalShiftType::Pointer
      shiftScaleEstimator =
      RegistrationParameterScalesFromPhysicalShiftType::New();
      shiftScaleEstimator->SetMetric( mimetric );
      shiftScaleEstimator->SetTransformForward( true );
      typename RegistrationParameterScalesFromPhysicalShiftType::ScalesType
      movingScales( simmer->GetNumberOfParameters() );
      shiftScaleEstimator->EstimateScales( movingScales );
      mstartOptimizer->SetScales( movingScales );
      mstartOptimizer->SetMetric( mimetric );
      localoptimizer->SetMetric( mimetric );
      localoptimizer->SetScales( movingScales );
    }
    if ( whichMetric.compare("MI") != 0  ) {
      gcmetric->Initialize();
      typedef itk::RegistrationParameterScalesFromPhysicalShift<GCMetricType>
        RegistrationParameterScalesFromPhysicalShiftType;
      typename RegistrationParameterScalesFromPhysicalShiftType::Pointer
        shiftScaleEstimator =
        RegistrationParameterScalesFromPhysicalShiftType::New();
      shiftScaleEstimator->SetMetric( gcmetric );
      shiftScaleEstimator->SetTransformForward( true );
      typename RegistrationParameterScalesFromPhysicalShiftType::ScalesType
      movingScales( simmer->GetNumberOfParameters() );
      shiftScaleEstimator->EstimateScales( movingScales );
      mstartOptimizer->SetScales( movingScales );
      mstartOptimizer->SetMetric( gcmetric );
      localoptimizer->SetMetric( gcmetric );
      localoptimizer->SetScales( movingScales );
    }
    typename OptimizerType::ParametersListType parametersList =
      mstartOptimizer->GetParametersList();
    affinesearch->SetIdentity();
    affinesearch->SetCenter( trans2 );
    affinesearch->SetOffset( trans );
    for ( unsigned int i = 0; i < thetas.size(); i++ )
      {
      RealType ang1 = thetas[i];
      RealType ang2 = 0; // FIXME should be psi
      RealType ang3 = 0; // FIXME should be psi
      if( ImageDimension == 3 )
        {
        for ( unsigned int jj = 0; jj < thetas2.size(); jj++ )
        {
        ang2=thetas2[jj];
        for ( unsigned int kk = 0; kk < thetas3.size(); kk++ )
        {
        ang3=thetas3[kk];
        affinesearch->SetIdentity();
        affinesearch->SetCenter( trans2 );
        affinesearch->SetOffset( trans );
        if( useprincaxis )
          {
          affinesearch->SetMatrix( A_solution );
          }
        affinesearch->Rotate3D(axis1, ang1, 1);
        affinesearch->Rotate3D(axis2, ang2, 1);
        affinesearch->Rotate3D(axis3, ang3, 1);
        affinesearch->Scale( bestscale );
        simmer->SetMatrix(  affinesearch->GetMatrix() );
        parametersList.push_back( simmer->GetParameters() );
        } // kk
        }
        }
      if( ImageDimension == 2 )
        {
        affinesearch->SetIdentity();
        affinesearch->SetCenter( trans2 );
        affinesearch->SetOffset( trans );
        if( useprincaxis )
          {
          affinesearch->SetMatrix( A_solution );
          }
        affinesearch->Rotate2D( ang1, 1);
//        affinesearch->Scale( bestscale );
        simmer->SetMatrix(  affinesearch->GetMatrix() );
        typename AffineType::ParametersType pp =
          simmer->GetParameters();
        parametersList.push_back( simmer->GetParameters() );
        }
      }
    mstartOptimizer->SetParametersList( parametersList );
    if( localSearchIterations > 0 )
      {
      mstartOptimizer->SetLocalOptimizer( localoptimizer );
      }
    mstartOptimizer->StartOptimization();
    typename AffineType::Pointer bestaffine = AffineType::New();
    bestaffine->SetCenter( trans2 );
    bestaffine->SetParameters( mstartOptimizer->GetBestParameters() );
    if ( txfn.length() > 3 )
      {
      typename AffineType::Pointer bestaffine = AffineType::New();
      bestaffine->SetCenter( trans2 );
      bestaffine->SetParameters( mstartOptimizer->GetBestParameters() );
      typedef itk::TransformFileWriter TransformWriterType;
      typename TransformWriterType::Pointer transformWriter =
        TransformWriterType::New();
      transformWriter->SetInput( bestaffine );
      transformWriter->SetFileName( txfn.c_str() );
      transformWriter->Update();
      }
    metricvalues = mstartOptimizer->GetMetricValuesList();
    unsigned int ncols = mstartOptimizer->GetBestParameters().Size() +
      1 + ImageDimension;
    Rcpp::NumericMatrix outMat( metricvalues.size(), ncols );
    for ( unsigned int k = 0; k < metricvalues.size(); k++ )
      {
      outMat( k, 0 ) = metricvalues[ k ];
      typename MetricType::ParametersType resultParams =
        mstartOptimizer->GetParametersList( )[ k ];
      for ( unsigned int kp = 0; kp < resultParams.Size(); kp++ )
        {
        outMat( k, kp + 1 ) = resultParams[ kp ];
        }
      // set the fixed parameters
      unsigned int baseind = resultParams.Size() + 1;
      for ( unsigned int kp = 0; kp < ImageDimension; kp++ )
        outMat( k, baseind + kp ) = bestaffine->GetFixedParameters()[ kp ];
      }
    return Rcpp::wrap( outMat );
    }
  else
    {
    Rcpp::NumericMatrix outMat( 1, 1 );
    outMat( 0, 0 ) = 0;
    return Rcpp::wrap( outMat );
    }
}

// [[myRcpp::export]]
RcppExport SEXP invariantImageSimilarity( SEXP r_in_image1 ,
  SEXP r_in_image2, SEXP thetas, SEXP thetas2, SEXP thetas3,
  SEXP localSearchIterations,
  SEXP whichMetric, SEXP r_scale, SEXP r_doref, SEXP txfn,
  SEXP whichTransform )
{
  if( r_in_image1 == NULL || r_in_image2 == NULL )
    {
    Rcpp::Rcout << "Invalid Arguments: pass 2 images in " << std::endl;
    Rcpp::wrap( 1 );
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
    Rcpp::Rcout << "Images must have equivalent dimensionality & pixel type"
      << std::endl ;
    Rcpp::wrap( 1 );
    }
  typedef itk::AffineTransform<double, 2> AffineType2D;
  typedef itk::AffineTransform<double, 3> AffineType3D;
  typedef itk::AffineTransform<double, 4> AffineType4D;
  typedef itk::Similarity2DTransform<double> SimilarityType2D;
  typedef itk::Similarity3DTransform<double> SimilarityType3D;
  typedef itk::Euler2DTransform<double> RigidType2D;
  typedef itk::Euler3DTransform<double> RigidType3D;
  // 0=affine, 1 = similarity, 2 = rigid
  unsigned int whichTx = Rcpp::as< unsigned int >( whichTransform );
  if ( dimension == 2 )
    {
    typedef itk::Image< float , 2 > ImageType;
    typedef ImageType::Pointer ImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr2(
      static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    if ( whichTx == 0 )
      return Rcpp::wrap( invariantSimilarityHelper<2,AffineType2D>(
        *antsimage_xptr1, *antsimage_xptr2, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    if ( whichTx == 1 )
      return Rcpp::wrap( invariantSimilarityHelper<2,SimilarityType2D>(
        *antsimage_xptr1, *antsimage_xptr2, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    if ( whichTx == 2 )
      return Rcpp::wrap( invariantSimilarityHelper<2,RigidType2D>(
        *antsimage_xptr1, *antsimage_xptr2, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
  }
  else if ( dimension == 3 )
    {
    typedef itk::Image< float , 3 > ImageType3;
    typedef ImageType3::Pointer ImagePointerType3;
    Rcpp::XPtr< ImagePointerType3 > antsimage_xptr1_3(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType3 > antsimage_xptr2_3(
    static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    if ( whichTx == 0 )
      return Rcpp::wrap(  invariantSimilarityHelper<3,AffineType3D>(
        *antsimage_xptr1_3, *antsimage_xptr2_3, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    if ( whichTx == 1 )
      return Rcpp::wrap(  invariantSimilarityHelper<3,SimilarityType3D>(
        *antsimage_xptr1_3, *antsimage_xptr2_3, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    if ( whichTx == 2 )
      return Rcpp::wrap(  invariantSimilarityHelper<3,RigidType3D>(
        *antsimage_xptr1_3, *antsimage_xptr2_3, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    }
  else if ( dimension == 4 )
    {
    typedef itk::Image< float , 4 > ImageType4;
    typedef ImageType4::Pointer ImagePointerType4;
    Rcpp::XPtr< ImagePointerType4 > antsimage_xptr1_4(
    static_cast< SEXP >( in_image1.slot( "pointer" ) ) ) ;
    Rcpp::XPtr< ImagePointerType4 > antsimage_xptr2_4(
    static_cast< SEXP >( in_image2.slot( "pointer" ) ) ) ;
    if ( whichTx == 0 )
      return Rcpp::wrap(  invariantSimilarityHelper<4,AffineType4D>(
        *antsimage_xptr1_4, *antsimage_xptr2_4, thetas,
        thetas2, thetas3,
        localSearchIterations, whichMetric, r_scale,
        r_doref, txfn ) );
    if ( ( whichTx == 1 ) || ( whichTx == 2 ) )
      {
      Rcpp::Rcout << " In dimension " << dimension <<
          " rigid/similarity is not supported " << std::endl;
        return Rcpp::wrap( 1 );
      }
    }
  else Rcpp::Rcout << " Dimension " << dimension <<
    " is not supported " << std::endl;
  return Rcpp::wrap( 1 );
}


template< class ImageType >
typename ImageType::Pointer convolveImageHelper(
  typename ImageType::Pointer image,
  typename ImageType::Pointer kernel )
{
  enum { Dimension = ImageType::ImageDimension };
  if( image.IsNotNull() & kernel.IsNotNull() )
    {
    typedef itk::ConvolutionImageFilter<ImageType> FilterType;
    typename FilterType::Pointer convolutionFilter = FilterType::New();
    convolutionFilter->SetInput( image );
    convolutionFilter->SetKernelImage( kernel );
    convolutionFilter->Update();
    return convolutionFilter->GetOutput();
    }
  return NULL;
}

// [[myRcpp::export]]
RcppExport SEXP itkConvolveImage( SEXP r_in_image1 ,
  SEXP r_in_image2  )
{
  if( r_in_image1 == NULL || r_in_image2 == NULL  )
    {
    Rcpp::Rcout << " Invalid Arguments: convolveImage.cpp " << std::endl ;
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
    Rcpp::Rcout << " Images must have equivalent dimensionality & pixel type" << std::endl ;
    Rcpp::wrap( 1 );
  }

  // make new out image, result of convolution
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
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
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
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
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
        convolveImageHelper<ImageType>(
          *antsimage_xptr1,*antsimage_xptr2 )
        );

    Rcpp::XPtr< ImagePointerType >
      out_image_xptr( out_image_ptr_ptr , true );
    out_image.slot( "pointer" ) = out_image_xptr;
    }
    else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;
  return out_image;
}
