/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RSfile: ImageMath.cxx,v $
  Language:  C++
  Date:      $Date: 2009/06/02 21:51:08 $
  Version:   $Revision: 1.103 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <Rcpp.h>

#include <map>
// Here I'm using a map but you could choose even other containers
#include <fstream>
#include <sstream>
#include <string>
#include "itkCSVNumericObjectFileWriter.h"
#include <iostream>
#include "itkTDistribution.h"
#include "itkTimeProbe.h"
#include "itkMedianImageFilter.h"
#include "itkVariableSizeMatrix.h"
//#include "itkVectorImageFileReader.h"
#include "itkVector.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkBilateralImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkLabeledPointSetFileReader.h"
#include "itkLabeledPointSetFileWriter.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"

//#include "itkBinaryMorphologicalClosingImageFilter.h"
//#include "itkBinaryMorphologicalOpeningImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkBayesianClassifierImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkBayesianClassifierInitializationImageFilter.h"
#include "itkScalarImageKmeansImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkTranslationTransform.h"
#include "itkImageMomentsCalculator.h"
#include "itkImageDuplicator.h"
#include "itkBinaryThresholdImageFilter.h"
#include "ReadWriteImage.h"
#include "itkBSplineControlPointImageFilter.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkOtsuThresholdImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "Temporary/itkFastMarchingImageFilter.h"
// #include "itkMinimumDecisionRule.h"
// #include "itkEuclideanDistance.h"
// #include "itkSampleClassifier.h"
#include "itkCastImageFilter.h"
// #include "itkScalarImageToListAdaptor.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGaussianImageSource.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkCompositeValleyFunction.h"
#include "itkNormalVariateGenerator.h"
#include "itkArray.h"
#include "itkImageFileWriter.h"
#include "itkSphereSpatialFunction.h"
#include "itkLabelContourImageFilter.h"
#include "itkMaskImageFilter.h"
// #include "itkDecisionRuleBase.h"
// #include "itkMinimumDecisionRule.h"
// #include "itkImageClassifierBase.h"
// #include "itkWellComposedImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

#include "itkImageKmeansModelEstimator.h"

#include "itkDistanceToCentroidMembershipFunction.h"

#include "itkMRFImageFilter.h"
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
// #include "itkMahalanobisDistanceMembershipFunction.h"
// #include "itkMinimumDecisionRule.h"
#include "itkSize.h"
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkImageRegionIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhood.h"

#include "itkRGBPixel.h"
#include "ReadWriteImage.h"
#include "TensorFunctions.h"
#include "antsMatrixUtilities.h"

template <class T>
bool from_string(T& t,
                 const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  iss >> f >> t;

  // Check to see that there is nothing left over
  if (!iss.eof()) return false;

  return true;
}

template<class T>
std::string ants_to_string(T t)
{
  std::stringstream istream;
  istream << t;
  return istream.str();
}

std::string ANTSGetFilePrefix(const char *str){

    std::string filename = str;
    std::string::size_type pos = filename.rfind( "." );
    std::string filepre = std::string( filename, 0, pos );
    if ( pos != std::string::npos ){
        std::string extension = std::string( filename, pos, filename.length()-1);
        if (extension==std::string(".gz")){
            pos = filepre.rfind( "." );
            extension = std::string( filepre, pos, filepre.length()-1 );
        }
	//      if (extension==".txt") return AFFINE_FILE;
//        else return DEFORMATION_FILE;
    }
//    else{
    //      return INVALID_FILE;
    //}
    return filepre;
}

template <class TImage>
typename TImage::Pointer BinaryThreshold(typename TImage::PixelType low, typename TImage::PixelType high,typename TImage::PixelType replaceval, typename TImage::Pointer input)
{
	//Rcpp::Rcout << " Binary Thresh " << std::endl;

	typedef typename TImage::PixelType PixelType;
	// Begin Threshold Image
	typedef itk::BinaryThresholdImageFilter<TImage,TImage>  InputThresholderType;
	typename InputThresholderType::Pointer inputThresholder =
		InputThresholderType::New();

	inputThresholder->SetInput( input );
	inputThresholder->SetInsideValue(  replaceval );
	int outval=0;
	if ((float) replaceval == (float) -1) outval=1;
	inputThresholder->SetOutsideValue( outval );

	if (high < low) high=255;
	inputThresholder->SetLowerThreshold((PixelType) low );
	inputThresholder->SetUpperThreshold((PixelType) high);
	inputThresholder->Update();

	return inputThresholder->GetOutput();
}

template<unsigned int ImageDimension>
int GetLargestComponent(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2="";
  unsigned long smallest=50;
  if (argc > argct) { smallest=atoi(argv[argct]); argct++; }

  typename ImageType::Pointer image1 = NULL;
  typename readertype::Pointer reader1 = readertype::New();
  reader1->SetFileName(fn1.c_str());
  reader1->UpdateLargestPossibleRegion();
  try
    {
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }
 // compute the voxel volume
  typename ImageType::SpacingType spacing=image1->GetSpacing();
  float volumeelement=1.0;
  for (unsigned int i=0;  i<spacing.Size(); i++)  volumeelement*=spacing[i];

  typedef float InternalPixelType;
  //  typedef unsigned long PixelType;
  //  typedef Image<PixelType,ImageDimension>  labelimagetype;
  typedef itk::Image<unsigned long , ImageDimension> labelimagetype;
  typedef ImageType InternalImageType;
  typedef ImageType OutputImageType;
  typedef itk::BinaryThresholdImageFilter< InternalImageType, labelimagetype > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< labelimagetype, labelimagetype > FilterType;
  typedef itk::RelabelComponentImageFilter< labelimagetype, ImageType > RelabelType;

  typename ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  typename FilterType::Pointer filter = FilterType::New();
  typename RelabelType::Pointer relabel = RelabelType::New();

  //  InternalPixelType threshold_low, threshold_hi;

  threshold->SetInput (image1);
  threshold->SetInsideValue(1);
  threshold->SetOutsideValue(0);
  threshold->SetLowerThreshold(0.25);
  threshold->SetUpperThreshold(1.e9);
  threshold->Update();

  filter->SetInput (threshold->GetOutput());
  filter->SetFullyConnected( 0 );
  filter->Update();
    relabel->SetInput( filter->GetOutput() );
    relabel->SetMinimumObjectSize( smallest );
    //    relabel->SetUseHistograms(true);

  try
    {
    relabel->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Relabel: exception caught !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }



  //  WriteImage<ImageType>(relabel->GetOutput(),outname.c_str());
  //  return 0;
  typename ImageType::Pointer Clusters=MakeNewImage<ImageType>(relabel->GetOutput(),0);
  //typename ImageType::Pointer Clusters=relabel->GetOutput();
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator vfIter( relabel->GetOutput(),  relabel->GetOutput()->GetLargestPossibleRegion() );

  float maximum=relabel->GetNumberOfObjects();
  Rcpp::Rcout <<" # ob " << maximum << std::endl;
  float maxtstat=0;
  std::vector<unsigned int> histogram((int)maximum+1);
  std::vector<float> clustersum((int)maximum+1);
  for (int i=0; i<=maximum; i++)
    {
      histogram[i]=0;
      clustersum[i]=0;
    }
  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      if (vfIter.Get() > 0 )
	{
	  float vox=image1->GetPixel(vfIter.GetIndex());
	  histogram[(unsigned int)vfIter.Get()]=histogram[(unsigned int)vfIter.Get()]+1;
	  clustersum[(unsigned int)vfIter.Get()]+=vox;
	  if (vox > maxtstat) maxtstat=vox;
	}
    }

  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      if (vfIter.Get() > 0 )
	{

	  Clusters->SetPixel( vfIter.GetIndex(), histogram[(unsigned int)vfIter.Get()]  );
	//  if ( Clusters->GetPixel( vfIter.GetIndex() ) > maximgval )
        //    maximgval=Clusters->GetPixel( vfIter.GetIndex());
	}
      else Clusters->SetPixel(vfIter.GetIndex(),0);
    }


  float maximgval=0;
  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    if ( Clusters->GetPixel( vfIter.GetIndex() ) > maximgval )
      maximgval=Clusters->GetPixel( vfIter.GetIndex());

  Rcpp::Rcout << " max float size " <<  (maximgval*volumeelement) << " long-size: " << (unsigned long) (maximgval*volumeelement)  << std::endl;

 for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    if ( Clusters->GetPixel( vfIter.GetIndex() ) >= maximgval )
      image1->SetPixel( vfIter.GetIndex(), 1);
    else image1->SetPixel( vfIter.GetIndex(), 0);

 if (outname.length() > 3)
   WriteImage<ImageType>(image1,outname.c_str());

 return 0;

}



template<unsigned int ImageDimension>
int ExtractSlice(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  unsigned int slice = atoi(argv[argct]);   argct++;
  Rcpp::Rcout << " Extract slice " << slice << " from dimension" << ImageDimension << std::endl;
  typename ImageType::Pointer image1 = NULL;
  typename OutImageType::Pointer outimage = NULL;

  typedef itk::ExtractImageFilter<ImageType,OutImageType> ExtractFilterType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;


  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;

  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  if ( slice >= timedims ) {
    Rcpp::Rcout << " max slice number is " << timedims << std::endl;
    return 1;
  }
  typename ImageType::RegionType extractRegion = image1->GetLargestPossibleRegion();
  extractRegion.SetSize(ImageDimension-1, 0);
  extractRegion.SetIndex(ImageDimension-1, slice );

  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetInput( image1 );
  //  extractFilter->SetDirectionCollapseToIdentity();
  extractFilter->SetDirectionCollapseToSubmatrix();
  extractFilter->SetExtractionRegion( extractRegion );
  extractFilter->Update();
  outimage=extractFilter->GetOutput();

  /*  typename ImageType::SpacingType qspc=warpthisimage->GetSpacing();
  typename ImageType::PointType qorg=warpthisimage->GetOrigin();
  typename ImageType::DirectionType qdir=warpthisimage->GetDirection();
  qdir.Fill(0);
  for (unsigned int qq=0; qq<ImageDimension-1; qq++) {
    for (unsigned int pp=0; pp<ImageDimension-1;pp++) {
      qdir[qq][pp]=img_mov->GetDirection()[qq][pp];
    }
      qspc[qq]=img_mov->GetSpacing()[qq];
      qorg[qq]=img_mov->GetOrigin()[qq];
    }
    warpthisimage->SetSpacing(qspc);
    warpthisimage->SetOrigin(qorg);
    warpthisimage->SetDirection(qdir);
  */
 if (outname.length() > 3)
   WriteImage<OutImageType>(outimage,outname.c_str());
 else return 1;

 return 0;

}



template<unsigned int ImageDimension>
int ThresholdAtMean(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float percentofmean=1.0;
  if (argc > argct) { percentofmean=atof(argv[argct]); argct++; }

  typename ImageType::Pointer image1 = NULL;
  typename readertype::Pointer reader1 = readertype::New();
  reader1->SetFileName(fn1.c_str());
  reader1->UpdateLargestPossibleRegion();
  try
    {
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  double mean=0,max=-1.e9,min=1.e9;
  unsigned long ct = 0;
  Iterator vfIter2( image1,  image1->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      double val=vfIter2.Get();
      mean+=val;
      if (val >max) max=val;
      else if (val < min) min=val;
      ct++;
    }
  if (ct > 0) mean/= (float)ct;

  typedef itk::BinaryThresholdImageFilter< ImageType, ImageType > ThresholdFilterType;
  typename ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();

  threshold->SetInput (image1);
  threshold->SetInsideValue(1);
  threshold->SetOutsideValue(0);
  threshold->SetLowerThreshold(mean*percentofmean);
  threshold->SetUpperThreshold(max);
  threshold->Update();
  WriteImage<ImageType>(threshold->GetOutput(),outname.c_str());
  return 0;

}



template<unsigned int ImageDimension>
int FlattenImage(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float percentofmax=1.0;
  if (argc > argct) { percentofmax=atof(argv[argct]); argct++; }

  typename ImageType::Pointer image1 = NULL;
  typename readertype::Pointer reader1 = readertype::New();
  reader1->SetFileName(fn1.c_str());
  reader1->UpdateLargestPossibleRegion();
  try
    {
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  double mean=0,max=-1.e9,min=1.e9;
  unsigned long ct = 0;
  Iterator vfIter2( image1,  image1->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      double val=vfIter2.Get();
      mean+=val;
      if (val >max) max=val;
      else if (val < min) min=val;
      ct++;
    }
  if (ct > 0) mean/= (float)ct;


  typename ImageType::Pointer out=MakeNewImage<ImageType>(image1,0);
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      double val=vfIter2.Get();
      if (val > max*percentofmax) val=(max*percentofmax);
      out->SetPixel(vfIter2.GetIndex(),val);
      ct++;
    }

  Rcpp::Rcout << " Flattening to :  " << percentofmax << std::endl;
  WriteImage<ImageType>(out,outname.c_str());
  return 0;

}

template <unsigned int ImageDimension>
int TruncateImageIntensity( unsigned int argc, char *argv[] )
{
  typedef int PixelType;
  typedef float RealType;

  // usage  ImageMath 3 out.nii.gz  TrunateImageIntensity InImage.nii.gz FractionLo(e.g.0.025) FractionHi(e.g.0.975) Bins Mask
  if (argc < 4 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl  <<   " ImageMath 3 outimage.nii.gz  TruncateImageIntensity inputImage  {lowerQuantile=0.025} {upperQuantile=0.975}  {numberOfBins=65}  {binary-maskImage} " << std::endl;  throw std::exception() ; }

  unsigned int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1=std::string(argv[argct]);   argct++;
  float  lo = 0.025;
  if ( argc > argct ) lo=atof(argv[argct]);   argct++;
  float  hi = 0.975;
  if ( argc > argct )  hi=atof(argv[argct]);  else hi=1.0-lo ;   argct++;
  unsigned int numberOfBins = 64;
  if ( argc > argct )  numberOfBins=atoi(argv[argct]);   argct++;

  //  Rcpp::Rcout << " bin " << numberOfBins << " lo " << lo << " Hi " << hi << std::endl;

  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::Image<RealType, ImageDimension> RealImageType;

  typedef itk::ImageFileReader<RealImageType> ReaderType;
  typename ReaderType::Pointer imageReader = ReaderType::New();
  imageReader->SetFileName( fn1.c_str() );
  imageReader->Update();

  typename ImageType::Pointer mask=NULL;
  if ( argc > argct )
    {
    mask = ImageType::New();
    try
      {
      typedef itk::ImageFileReader<ImageType> ReaderType;
      typename ReaderType::Pointer labelImageReader = ReaderType::New();
      labelImageReader->SetFileName( argv[argct] );
      labelImageReader->Update();
      mask = labelImageReader->GetOutput();
      }
    catch(...)
      {
	Rcpp::Rcout << " can't read mask " << std::endl;
	mask=NULL;
     };
    }
  //  Rcpp::Rcout << " Mask " << std::endl;
  if( !mask )
    {
    mask = ImageType::New();
    mask->SetOrigin( imageReader->GetOutput()->GetOrigin() );
    mask->SetSpacing( imageReader->GetOutput()->GetSpacing() );
    mask->SetRegions( imageReader->GetOutput()->GetLargestPossibleRegion() );
    mask->SetDirection( imageReader->GetOutput()->GetDirection() );
    mask->Allocate();
    mask->FillBuffer( itk::NumericTraits<PixelType>::One );
    }


  //  Rcpp::Rcout << " iterate " << std::endl;

  itk::ImageRegionIterator<RealImageType> ItI( imageReader->GetOutput(),
    imageReader->GetOutput()->GetLargestPossibleRegion() );
  itk::ImageRegionIterator<ImageType> ItM( mask,
    mask->GetLargestPossibleRegion() );

  RealType maxValue = itk::NumericTraits<RealType>::NonpositiveMin();
  RealType minValue = itk::NumericTraits<RealType>::max();
  ItM.GoToBegin();
  for (ItI.GoToBegin(); !ItI.IsAtEnd();  ++ItI )
    {
      //  Rcpp::Rcout << " ind " << ItI.GetIndex() << std::endl;
      if ( ItI.Get() >  0 && ItM.Get() >= 0.5 )
      {
      if ( ItI.Get() < minValue )
        {
        minValue = ItI.Get();
        }
      else if ( ItI.Get() > maxValue )
        {
        maxValue = ItI.Get();
        }
      ItM.Set( itk::NumericTraits<PixelType>::One );
      }
    else
      {
      ItM.Set( itk::NumericTraits<PixelType>::Zero );
      }
    if( vnl_math_isnan( ItI.Get() ) || vnl_math_isinf( ItI.Get() ) )
      {
      ItM.Set( itk::NumericTraits<PixelType>::Zero );
      }
    ++ItM;
    }
  //  Rcpp::Rcout << " label " << std::endl;
  typedef itk::LabelStatisticsImageFilter<RealImageType, ImageType> HistogramGeneratorType;
  typename HistogramGeneratorType::Pointer stats = HistogramGeneratorType::New();
  stats->SetInput( imageReader->GetOutput() );
  stats->SetLabelInput( mask );
  stats->SetUseHistograms( true );
  stats->SetHistogramParameters( numberOfBins, minValue, maxValue );
  stats->Update();
  //  Rcpp::Rcout << " labeld " << std::endl;
  typedef typename HistogramGeneratorType::HistogramType  HistogramType;
  const HistogramType *histogram = stats->GetHistogram( 1 );

  double lowerQuantile = histogram->Quantile( 0, lo );
  double	upperQuantile = histogram->Quantile( 0, hi );

  Rcpp::Rcout << "Lower quantile: " << lowerQuantile << std::endl;
  Rcpp::Rcout << "Upper quantile: " << upperQuantile << std::endl;

  for ( ItI.GoToBegin(); !ItI.IsAtEnd(); ++ItI )
    {
    if ( ItI.Get() <  lowerQuantile )
      {
      ItI.Set(  lowerQuantile );
      }
    if ( ItI.Get() > upperQuantile )
      {
      ItI.Set(  upperQuantile  );
      }

    }

  typedef itk::ImageFileWriter<RealImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( imageReader->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}

template <unsigned int ImageDimension>
int TileImages(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  unsigned int nx = atoi(argv[argct]);   argct++;

  unsigned int numberofimages=0;

  typename ImageType::Pointer averageimage = NULL;
  typename ImageType::Pointer image2 = NULL;
  typename ImageType::SizeType size;
  double meanval=1;
  size.Fill(0);
  unsigned int bigimage=0;
  for (unsigned int j=argct; j< argc; j++)
    {
    numberofimages++;
    // Get the image dimension
    std::string fn = std::string(argv[j]);
    typename itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(fn.c_str(), itk::ImageIOFactory::ReadMode);
    imageIO->SetFileName(fn.c_str());
    imageIO->ReadImageInformation();
    for (unsigned int i=0; i<imageIO->GetNumberOfDimensions(); i++)
      {
      if ( imageIO->GetDimensions(i) > size[i] )
	{
	size[i]=imageIO->GetDimensions(i);
	bigimage=j;
	Rcpp::Rcout << " bigimage " << j << " size " << size << std::endl;
	}
      }
    }

  ReadImage<ImageType>(image2,argv[bigimage]);

  std:: cout << " largest image " << size << std::endl;

/** declare the tiled image */
  unsigned int xsize=size[0];
  unsigned int ysize=size[1];
  typename ImageType::SizeType tilesize;
  unsigned int ny=(unsigned int)((float)numberofimages/(float)nx+0.5);
  if ( nx*ny < numberofimages) ny++;
  Rcpp::Rcout  << " nx " << nx << " ny " << ny << std::endl;
  tilesize[0]=xsize*nx;
  tilesize[1]=ysize*ny;
  typename ImageType::RegionType region;
  region.SetSize( tilesize );

  bool normalizei=false;
  typename ImageType::Pointer tiledimage=ImageType::New();
  tiledimage->SetLargestPossibleRegion( region );
  tiledimage->SetBufferedRegion( region );
  tiledimage->SetSpacing( image2->GetSpacing() );
  tiledimage->SetDirection( image2->GetDirection() );
  tiledimage->SetOrigin( image2->GetOrigin() );
  tiledimage->Allocate();

  unsigned int imagecount=0,imagexct=0,imageyct=0;

  for (unsigned int j=argct; j< argc; j++)
    {
    // Get the image dimension
    std::string fn = std::string(argv[j]);
    ReadImage<ImageType>(image2,fn.c_str());

    unsigned long ct = 0;
    if (normalizei)
      {
      meanval = 0.0;
      Iterator vfIter2( image2,  image2->GetLargestPossibleRegion() );
      for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
	{
	meanval+=vfIter2.Get();
	ct++;
	}
      if (ct > 0) meanval /= (float)ct;
      if (meanval <= 0) meanval=1.0;
      }

    imagexct=imagecount % nx;
    imageyct=imagecount/nx;
    Rcpp::Rcout << "doing " << fn << "  " << imagecount << " x " << imagexct <<  " y " << imageyct << std::endl;
    imagecount++;
    Iterator vfIter( image2,  image2->GetLargestPossibleRegion() );
    for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
      {
      typename ImageType::IndexType locind=vfIter.GetIndex();
      typename ImageType::IndexType globind;
      globind[0]=size[0]*imagexct+locind[0];
      globind[1]=size[1]*imageyct+locind[1];
      double val =  vfIter.Get()/meanval;
      tiledimage->SetPixel(globind,   val );
      }
    }

  WriteImage<ImageType>(tiledimage,outname.c_str());
  return 0;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::RescaleIntensityImageFilter<ImageType,ByteImageType > RescaleFilterType;
  typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
   rescaler->SetInput( tiledimage );

  Rcpp::Rcout << " writing output ";
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  return 0;

}



template <unsigned int ImageDimension>
int ConvertLandmarkFile(unsigned int argc, char *argv[])
{

  unsigned int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string infn=std::string(argv[argct]); argct++;
  float pointp=1;


  typedef itk::PointSet<long,ImageDimension> PointSetType;
  typedef itk::LabeledPointSetFileReader<PointSetType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( infn.c_str() );
  reader->SetRandomPercentage( 1 );
   if ( pointp > 0 && pointp < 1  )
    {
    reader->SetRandomPercentage( pointp );
    }
  reader->Update();

  Rcpp::Rcout << "Number of labels: " << reader->GetNumberOfLabels() << std::endl;
  Rcpp::Rcout << "Labels: ";
  for ( unsigned int i = 0; i < reader->GetNumberOfLabels(); i++ )
    {
    Rcpp::Rcout << reader->GetLabelSet()->operator[](i) << " ";
    }
  Rcpp::Rcout << std::endl;

  typedef itk::LabeledPointSetFileWriter<PointSetType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outname.c_str() );
  writer->SetInput( reader->GetOutput() );
  writer->Update();

  return 0;
}



template <unsigned int ImageDimension>
int TriPlanarView(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  unsigned int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string maskfn=std::string(argv[argct]); argct++;
  Rcpp::Rcout << " file name " << maskfn << std::endl;
  typename ImageType::Pointer mask = NULL;
  typename readertype::Pointer reader2 = readertype::New();
  reader2->SetFileName(maskfn.c_str());
  try
    {
      reader2->UpdateLargestPossibleRegion();
    }
  catch(...)
    {
      Rcpp::Rcout << " Error reading " << maskfn << std::endl;
    }
  mask=reader2->GetOutput();
  //ReadImage<ImageType>(mask,maskfn.c_str());
  //  WriteImage<ImageType>(mask,"temp.nii");
  float clamppercent1 = 0.1;
  if (argc > argct) clamppercent1=atof(argv[argct]);   argct++;
  if (clamppercent1 > 1) clamppercent1=1;
  float clamppercent2=0.1;
  if (argc > argct) clamppercent2=atof(argv[argct]);   argct++;
  if (clamppercent2 > 1) clamppercent2=1;

  typename ImageType::SizeType size=mask->GetLargestPossibleRegion().GetSize();
  unsigned int  xslice = size[0]/2;
  if (argc > argct) xslice=atoi(argv[argct]);   argct++;
  unsigned int  yslice = size[1]/2;
  if (argc > argct) yslice=atoi(argv[argct]);   argct++;
  unsigned int  zslice = size[2]/2;
  if (argc > argct) zslice=atoi(argv[argct]);   argct++;

  typedef itk::RescaleIntensityImageFilter<ImageType,ImageType > RescaleFilterType;
  typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( mask );
  rescaler->Update();
  mask=rescaler->GetOutput();


  //  typedef itk::IntensityWindowingImageFilter<ImageType,ImageType> wFilterType;
  //  typename wFilterType::Pointer wer = wFilterType::New();
  //wer->SetInput(mask);

/** declare the tiled image */
  unsigned long xsize=size[0];
  unsigned long ysize=size[1];
  unsigned long zsize=size[2];
  typename MatrixImageType::SizeType ztilesize;
  ztilesize[0]=xsize;
  ztilesize[1]=ysize;
  typename MatrixImageType::SizeType ytilesize;
  ytilesize[0]=xsize;
  ytilesize[1]=zsize;
  typename MatrixImageType::SizeType xtilesize;
  xtilesize[0]=ysize;
  xtilesize[1]=zsize;
  typename MatrixImageType::SizeType tilesize;
  tilesize[0]=xtilesize[0]+ytilesize[0]+ztilesize[0];
  tilesize[1]=xtilesize[1];
  if ( ytilesize[1] > tilesize[1] ) tilesize[1]=ytilesize[1];
  if ( ztilesize[1] > tilesize[1] ) tilesize[1]=ztilesize[1];
  Rcpp::Rcout << " allocate matrix " << tilesize << std::endl;
  typename MatrixImageType::RegionType region;
  region.SetSize( tilesize );

  typename MatrixImageType::Pointer matimage=MatrixImageType::New();
  matimage->SetLargestPossibleRegion( region );
  matimage->SetBufferedRegion( region );
  typename MatrixImageType::DirectionType mdir;  mdir.Fill(0); mdir[0][0]=1; mdir[1][1]=1;
  typename MatrixImageType::SpacingType mspc;  mspc.Fill(1);
  typename MatrixImageType::PointType morg;  morg.Fill(0);
  matimage->SetSpacing( mspc );
  matimage->SetDirection(mdir);
  matimage->SetOrigin( morg );
  matimage->Allocate();
  unsigned int lowgetridof=(unsigned int) (clamppercent1*256);
  unsigned int higetridof=(unsigned int) (256 - clamppercent2*256);
  //  Rcpp::Rcout << " get rid of " << getridof << std::endl;
  matimage->FillBuffer(lowgetridof);
  // now loop over each slice and put the pixels in the right place in matimage
  typename MatrixImageType::IndexType index2d;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator vfIter2( mask,  mask->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      double val1=vfIter2.Get();
      if ( val1 > higetridof ) vfIter2.Set(higetridof );
      if ( val1 < lowgetridof   ) vfIter2.Set(lowgetridof );
  // first do z-slice
      if ( vfIter2.GetIndex()[2] == (long) zslice) {
      double val=vfIter2.Get();
      typename ImageType::IndexType index=vfIter2.GetIndex();
      index2d[0]=index[0]+xtilesize[0]+ytilesize[0];
      index2d[1]=index[1];
      index2d[1]=tilesize[1]-index2d[1]-1;
      matimage->SetPixel(index2d,val);
      }
      if ( vfIter2.GetIndex()[1] == (long)yslice) {
      double val=vfIter2.Get();
      typename ImageType::IndexType index=vfIter2.GetIndex();
      index2d[0]=index[0]+xtilesize[0];
      index2d[1]=index[2];
      index2d[1]=tilesize[1]-index2d[1]-1;
      matimage->SetPixel(index2d,val);
      }
      if ( vfIter2.GetIndex()[0] == (long)xslice) {
      double val=vfIter2.Get();
      typename ImageType::IndexType index=vfIter2.GetIndex();
      index2d[0]=index[1];
      index2d[1]=index[2];
      index2d[1]=tilesize[1]-index2d[1]-1;
      matimage->SetPixel(index2d,val);
      }
    }


  typedef itk::Image<unsigned char,2> ByteImageType;
  typedef itk::RescaleIntensityImageFilter<MatrixImageType,ByteImageType > RescaleFilterType2;
  typename RescaleFilterType2::Pointer rescaler2 = RescaleFilterType2::New();
  rescaler2->SetOutputMinimum(   0 );
  rescaler2->SetOutputMaximum( 255 );
  rescaler2->SetInput( matimage );
  rescaler2->Update();
  Rcpp::Rcout << " writing output ";
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( rescaler2->GetOutput() );
  writer->Update();

  return 0;

}


template <unsigned int ImageDimension>
int ConvertVectorToImage(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  typedef itk::ImageRegionIteratorWithIndex<MatrixImageType> vIterator;

  int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string maskfn=std::string(argv[argct]); argct++;
  std::string vecfn=std::string(argv[argct]); argct++;
  typename ImageType::Pointer mask = NULL;
  ReadImage<ImageType>(mask,maskfn.c_str());
  typename MatrixImageType::Pointer vecimg = NULL;
  ReadImage<MatrixImageType>(vecimg,vecfn.c_str());
  unsigned long voxct=0,mct=0;
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5) mct++;
  vIterator vIter(vecimg,vecimg->GetLargestPossibleRegion() );
  for(  vIter.GoToBegin(); !vIter.IsAtEnd(); ++vIter )
    voxct++;

  Rcpp::Rcout << " vct " << voxct << " mct " << mct << std::endl;

  typename ImageType::Pointer outimage = NULL;
  ReadImage<ImageType>(outimage,maskfn.c_str());
  outimage->FillBuffer(0);

  vIter.GoToBegin();
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5)
      {
	outimage->SetPixel(mIter.GetIndex(),vIter.Get());
	++vIter;
      }

  WriteImage<ImageType>(outimage,outname.c_str());

  return 0;

}




template<unsigned int ImageDimension>
int CorruptImage(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float noiselevel=1.0;
  if (argc > argct) { noiselevel=atof(argv[argct]); argct++; }
  float smoothlevel=1.0;
  if (argc > argct) { smoothlevel=atof(argv[argct]); argct++; }

  typename ImageType::Pointer image1 = NULL;
  typename readertype::Pointer reader1 = readertype::New();
  reader1->SetFileName(fn1.c_str());
  reader1->UpdateLargestPossibleRegion();
  try
    {
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image1,  image1->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
    double r = (   (double)rand()  / ((double)(RAND_MAX) +(double)(1)) )-0.5;
    iter.Set(iter.Get()+r*noiselevel);
    }

  if (smoothlevel >  0)
    {
	{
	  typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> dgf;
	  typename dgf::Pointer filter = dgf::New();
	  filter->SetVariance(smoothlevel);
	  filter->SetUseImageSpacingOff();
	  filter->SetMaximumError(.01f);
	  filter->SetInput(image1);
	  filter->Update();
	  image1=filter->GetOutput();
	}
    }
  WriteImage<ImageType>(image1,outname.c_str());
  return 0;

}


template<unsigned int ImageDimension>
int Where(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float value=atof(argv[argct]); argct++;
  std::string fn2 = "";
  if (argc > argct)  {fn2=std::string(argv[argct]);   argct++; }
  float tol=0.0;
  if (argc > argct)   { tol=atof(argv[argct]);   argct++; }

  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  if (fn2.length() > 3)   ReadImage<ImageType>(image2, fn2.c_str());


  unsigned long ct=0;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image1,  image1->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      if (!image2)
	{
	  if ( fabs(iter.Get() - value) < tol )
	    {
	      Rcpp::Rcout << iter.GetIndex() << std::endl;
	      ct++;
	    }
	}
      else if (image2->GetPixel(iter.GetIndex()) > 0 &&  fabs(iter.Get() - value) < tol )
	{
	      Rcpp::Rcout << iter.GetIndex() << std::endl;
	      ct++;
	}
    }
  Rcpp::Rcout << ct <<  " voxels have the value " << value << std::endl;
  return 0;

}




template<unsigned int ImageDimension>
int SetOrGetPixel(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float value=atof(argv[argct]);
  std::string Get=std::string(argv[argct]); argct++;
  float indx=atof(argv[argct]); argct++;
  float indy=0;
  if (ImageDimension >= 2) { indy=atof(argv[argct]); argct++ ;}
  float indz=0;
  if (ImageDimension >= 3) { indz=atof(argv[argct]); argct++ ;}
  bool usephyspace=false;
  if (argc > argct ) { usephyspace=atoi(argv[argct]); argct++; }
  bool get=false;
  if (strcmp(Get.c_str(),"Get") == 0) get=true;
  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;
  if (fn1.length() > 3)
    {
    ReadImage<ImageType>(image1, fn1.c_str());
    ReadImage<ImageType>(image2, fn1.c_str());
    }
  if (!image1) {Rcpp::Rcout <<" no image ! " << std::endl; throw std::exception() ;}

  typename ImageType::IndexType index;
  index.Fill(0);
  if (usephyspace==false) {
    index[0]=(long int)indx;
    index[1]=(long int)indy;
    if ( ImageDimension == 3) index[2]=(long int)indz;
  }
  else
    {
      typename ImageType::PointType porig;
      porig[0]=indx;
      porig[1]=indy;
      if (ImageDimension==3) porig[2]=indz;
      image1->TransformPhysicalPointToIndex(porig, index);
    }
  Rcpp::Rcout << " use phy " << usephyspace << " " << indx << " " << indy << " " << indz << std::endl;
  Rcpp::Rcout << " Ind " << index << std::endl;
  bool isinside=true;
  for (unsigned int i=0; i<ImageDimension; i++)
    {
    float shifted=index[i];
    if (shifted < 0 || shifted >  image1->GetLargestPossibleRegion().GetSize()[i]-1  ) isinside=false;
    }

  if ( isinside == true )
    {
    if ( get ) { Rcpp::Rcout <<" GetValue at " << index << " is " << image1->GetPixel(index) << std::endl; }
    else
      {
      Rcpp::Rcout <<" SetValue at " << index << " value " << value << " replaces " <<  image1->GetPixel(index) << std::endl;
      image2->SetPixel(index,value);
      WriteImage<ImageType>(image2,outname.c_str());
      }
    }
  else Rcpp::Rcout <<" not in image " << index << std::endl;

 return 0;

}

template<unsigned int ImageDimension>
int HistogramMatching(int argc, char * argv[])
{

  typedef float PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::HistogramMatchingImageFilter<ImageType, ImageType> MatchingFilterType;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = std::string(argv[argct]);   argct++;
  long bins = 255;
  if (argc > argct) bins =atoi(argv[argct]);   argct++;
  long points = 64;
  if (argc > argct) points =atoi(argv[argct]);   argct++;

  typename ImageType::Pointer source;
  ReadImage<ImageType>(source,fn1.c_str());

  typename ImageType::Pointer reference;
  ReadImage<ImageType>(reference,fn2.c_str());

  typename MatchingFilterType::Pointer match = MatchingFilterType::New();
  match->SetSourceImage(source);
  match->SetReferenceImage(reference);
  match->SetNumberOfHistogramLevels(bins);
  match->SetNumberOfMatchPoints(points);
  match->Update();

  WriteImage<ImageType>(match->GetOutput(), outname.c_str());
  return 0;
}


template<unsigned int ImageDimension>
int PadImage(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float padvalue=atof(argv[argct]); argct++;
  std::string fn2 = "";
  if (argc > argct) fn2=std::string(argv[argct]);   argct++;

  typename ImageType::Pointer image1 = NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());

  typename ImageType::PointType origin=image1->GetOrigin();
  typename ImageType::PointType origin2=image1->GetOrigin();

  typename ImageType::SizeType size=image1->GetLargestPossibleRegion().GetSize();
  typename ImageType::SizeType newsize=image1->GetLargestPossibleRegion().GetSize();
  typename ImageType::RegionType newregion;
  // determine new image size
  for (unsigned int i=0; i<ImageDimension;i++)
    {
      float dimsz=(float)size[i];
      newsize[i]=(unsigned int)(dimsz+padvalue*2);
    }
  Rcpp::Rcout << " oldsize " << size <<  " newsize " << newsize << std::endl;
  newregion.SetSize(newsize);
  newregion.SetIndex(image1->GetLargestPossibleRegion().GetIndex());

  typename ImageType::Pointer padimage = ImageType::New();
  padimage->SetSpacing(image1->GetSpacing());
  padimage->SetOrigin(origin2);
  padimage->SetDirection(image1->GetDirection());
  padimage->SetRegions(newregion );
  padimage->Allocate();
  padimage->FillBuffer( 0 );

  typename ImageType::IndexType index;
  typename ImageType::IndexType index2;
  if (padvalue > 0)
    {
      index.Fill(0);
      index2.Fill((unsigned int)fabs(padvalue));
    }
  else
    {
      index2.Fill(0);
      index.Fill((unsigned int)fabs(padvalue));
    }

  typename ImageType::PointType point1,pointpad;
  image1->TransformIndexToPhysicalPoint(index, point1);
  padimage->TransformIndexToPhysicalPoint(index2, pointpad);

  Rcpp::Rcout << " pre " << point1 << " pad " << pointpad << std::endl;
  for (unsigned int i=0; i<ImageDimension; i++)
    origin2[i]+=(point1[i]-pointpad[i]);

  padimage->SetOrigin(origin2);

  Iterator iter( image1,  image1->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {

      typename ImageType::IndexType oindex=iter.GetIndex();
      typename ImageType::IndexType padindex=iter.GetIndex();

      bool isinside=true;
      for (unsigned int i=0; i<ImageDimension; i++)
	{
	  float shifted=((float)oindex[i]+padvalue);
	  if (shifted < 0 || shifted > newsize[i]-1 ) isinside=false;
	    //	  if (shifted < 0) shifted=0;
	    //padindex[i]=
	}
      if (isinside)
	{
	  for (unsigned int i=0; i<ImageDimension; i++)
	    {
	      float shifted=((float)oindex[i]+padvalue);
	      padindex[i]=(unsigned int)shifted;
	    }
	  padimage->SetPixel(padindex,iter.Get());
	}

    }


  WriteImage<ImageType>(padimage,outname.c_str());


  return 0;

}


template<unsigned int ImageDimension>
int CenterImage2inImage1(int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (argc > argct) fn2=std::string(argv[argct]);   argct++;

  typename ImageType::Pointer image1 = NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  image1->FillBuffer(0);
  typename ImageType::Pointer image2 = NULL;
  if (fn2.length() > 3)   ReadImage<ImageType>(image2, fn2.c_str());

  // compute center of mass in image2 in point space
  double iweight=0;
  typename ImageType::PointType cm_point; cm_point.Fill(0);
  Iterator iter( image2,  image2->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      typename ImageType::PointType point;
      image2->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
      for (unsigned int d=0; d<ImageDimension; d++)
	cm_point[d]+=point[d]*iter.Get();
      iweight+=iter.Get();
    }

  // center of image1
  typename ImageType::IndexType image_1_center_index;
  image_1_center_index.Fill(0);
  for (unsigned int d=0; d<ImageDimension; d++) {
    cm_point[d]/=iweight;
    image_1_center_index[d]=image1->GetLargestPossibleRegion().GetSize()[d]/2;
  }
  typename ImageType::PointType image_1_center_point;
  image1->TransformIndexToPhysicalPoint(image_1_center_index, image_1_center_point);

  // now we translate the cm_point to the center of image1
  typename ImageType::PointType trans;
  for (unsigned int d=0; d<ImageDimension; d++)
    trans[d]=image_1_center_point[d]-cm_point[d];

  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      typename ImageType::PointType point;
      image2->TransformIndexToPhysicalPoint(iter.GetIndex(), point);
      for (unsigned int d=0; d<ImageDimension; d++)
	point[d]=point[d]+trans[d];

      typename ImageType::IndexType newindex;
      newindex.Fill(0);
      bool isinside=image1->TransformPhysicalPointToIndex(point,newindex);
      if ( isinside ) {
	image1->SetPixel(newindex,iter.Get());
      }
    }



  WriteImage<ImageType>(image1,outname.c_str());
  return 0;

}


template<unsigned int ImageDimension>
int TimeSeriesSubset(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  unsigned int n_sub_vols = atoi(argv[argct]);   argct++;
  Rcpp::Rcout << " Extract " << n_sub_vols << " subvolumes " << std::endl;
  std::string::size_type idx;
  idx = outname.find_first_of('.');
  std::string tempname = outname.substr(0,idx);
  std::string extension = outname.substr(idx,outname.length());

  typename ImageType::Pointer image1 = NULL;
  typename OutImageType::Pointer outimage = NULL;

  typedef itk::ExtractImageFilter<ImageType,OutImageType> ExtractFilterType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;

  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  float step=(float)timedims/(float)n_sub_vols;
  if ( n_sub_vols >= timedims ) { n_sub_vols=timedims; step=1; }
  for ( unsigned int i=0; i < n_sub_vols; i++) {
    std::string s;
    std::stringstream out;
    out << (100+i);
    s = out.str();
    std::string kname=tempname+s+extension;
    typename ImageType::RegionType extractRegion = image1->GetLargestPossibleRegion();
    extractRegion.SetSize(ImageDimension-1, 0);
    unsigned int sub_vol=(unsigned int)((float)i*step);
    if ( sub_vol >= timedims ) sub_vol=timedims-1;
    extractRegion.SetIndex(ImageDimension-1, sub_vol );

    typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
    extractFilter->SetInput( image1 );
    //    extractFilter->SetDirectionCollapseToIdentity();
    extractFilter->SetDirectionCollapseToSubmatrix();
    extractFilter->SetExtractionRegion( extractRegion );
    extractFilter->Update();
    outimage=extractFilter->GetOutput();
    WriteImage<OutImageType>(outimage,kname.c_str());
  }

  return 0;
}


template<unsigned int ImageDimension>
int ComputeTimeSeriesLeverage(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef typename OutImageType::IndexType OutIndexType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef double Scalar;
  typedef itk::ants::antsMatrixUtilities<ImageType,Scalar>  matrixOpType;
  typename matrixOpType::Pointer matrixOps=matrixOpType::New();

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  unsigned int k_neighbors = atoi(argv[argct]);   argct++;
  typename ImageType::Pointer image1=NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;
  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];

  typedef itk::ExtractImageFilter<ImageType,OutImageType> ExtractFilterType;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  typename ImageType::RegionType extractRegion = image1->GetLargestPossibleRegion();
  extractRegion.SetSize(ImageDimension-1, 0);
  unsigned int sub_vol=0;
  extractRegion.SetIndex(ImageDimension-1, sub_vol );
  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetInput( image1 );
  //    extractFilter->SetDirectionCollapseToIdentity();
  extractFilter->SetDirectionCollapseToSubmatrix();
  extractFilter->SetExtractionRegion( extractRegion );
  extractFilter->Update();
  typename OutImageType::Pointer outimage=extractFilter->GetOutput();
  outimage->FillBuffer(0);

  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  // step 1.  compute , for each image in the time series, the effect on the average.
  // step 2.  the effect is defined as the influence of that point on the average or, more simply, the distance of that image from the average ....
  typedef vnl_vector<Scalar>      timeVectorType;
  timeVectorType mSample(timedims,0);
  timeVectorType mLeverage(timedims,0);
  timeVectorType kDistance(timedims,0);
  SliceIt vfIter2( outimage, outimage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      IndexType tind;
      // first collect all samples for that location
      for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
      double all_mean=0;
      for (unsigned int t=0; t<timedims; t++){
        tind[ImageDimension-1]=t;
	Scalar pix=image1->GetPixel(tind);
        mSample(t)=pix;
	all_mean+=pix;
      }
      // compute mean time series value at this voxel
      all_mean/=(double)timedims;

      // second compute the leverage for each time point and add that to the total leverage
      //
      // this is a simple approach --- just the difference from the mean.
      //
      for (unsigned int t=0; t<timedims; t++){
	mLeverage(t)+=fabs(all_mean-mSample(t))/(Scalar)timedims;
      }
    }


  // now use k neighbors to get a distance
  kDistance.fill(0);
  for (unsigned int t=0; t<timedims; t++){
    int lo=(int)t-k_neighbors/2;
    int hi=(int)t+k_neighbors/2;
    if ( lo < (int) 0 ) lo=0;
    if ( hi > (int)(timedims-1) ) hi=timedims-1;
    unsigned int ct=0;
    for ( int k=lo; k<hi; k++){
      if ( k != (int)t)
      {
        kDistance(t)+=fabs(mLeverage(t)-mLeverage(k));
	ct++;
      }
    }
    kDistance(t)/=(double)ct;
    kDistance(t)=kDistance(t)/mLeverage(t);
  }

  // now write the mLeverage value for each time point ...
  std::ofstream logfile;
  logfile.open(outname.c_str());
  if ( logfile.good() ) {
    Rcpp::Rcout << "Raw_Leverage,K_Neighbors_Distance"<<  std::endl;
    logfile << "Raw_Leverage,K_Neighbors_Distance"<<  std::endl;
    for (unsigned int t=0; t<timedims; t++){
      Rcpp::Rcout <<  mLeverage(t) << ","<< kDistance(t) << std::endl;
      logfile <<  mLeverage(t) << ","<< kDistance(t) << std::endl;
    }
  }
  logfile.close();
  return 0;
}

template<unsigned int ImageDimension>
int TimeSeriesToMatrix(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef typename OutImageType::IndexType OutIndexType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef double Scalar;
  typedef itk::ants::antsMatrixUtilities<ImageType,Scalar>  matrixOpType;
  typename matrixOpType::Pointer matrixOps=matrixOpType::New();

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string ext = itksys::SystemTools::GetFilenameExtension( outname );
  if  (strcmp(ext.c_str(),".csv") != 0 ) {
    Rcpp::Rcout << " must use .csv as output file extension "<<std::endl;
    return EXIT_FAILURE;
  }
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string maskfn = std::string(argv[argct]);   argct++;
  typename ImageType::Pointer image1=NULL;
  typename OutImageType::Pointer mask=NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;
  if (maskfn.length() > 3)   ReadImage<OutImageType>(mask, maskfn.c_str());
  else return 1;
  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  unsigned long voxct=0;
  typedef itk::ExtractImageFilter<ImageType,OutImageType> ExtractFilterType;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;
  SliceIt mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5) voxct++;


  typename ImageType::RegionType extractRegion = image1->GetLargestPossibleRegion();
  extractRegion.SetSize(ImageDimension-1, 0);
  unsigned int sub_vol=0;
  extractRegion.SetIndex(ImageDimension-1, sub_vol );
  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetInput( image1 );
  //    extractFilter->SetDirectionCollapseToIdentity();
  extractFilter->SetDirectionCollapseToSubmatrix();
  extractFilter->SetExtractionRegion( extractRegion );
  extractFilter->Update();
  typename OutImageType::Pointer outimage=extractFilter->GetOutput();
  outimage->FillBuffer(0);

  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  typedef vnl_vector<Scalar>      timeVectorType;
  timeVectorType mSample(timedims,0);
  typedef itk::Array2D<double> MatrixType;
  std::vector<std::string> ColumnHeaders;
  MatrixType matrix(timedims,voxct);
  matrix.Fill(0);
  SliceIt vfIter2( outimage, outimage->GetLargestPossibleRegion() );
  voxct=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( mask->GetPixel(ind) >= 0.5 ) {
      IndexType tind;
      // first collect all samples for that location
      for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
      for (unsigned int t=0; t<timedims; t++){
        tind[ImageDimension-1]=t;
	Scalar pix=image1->GetPixel(tind);
        mSample(t)=pix;
        matrix[t][voxct]=pix;
      }
      std::string colname=std::string("V")+ants_to_string<unsigned int>(voxct);
      ColumnHeaders.push_back( colname );
      voxct++;
      }// check mask
    }

    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname );
    writer->SetInput( &matrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }

  return 0;
}



template<unsigned int ImageDimension>
int CompCorrAuto(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef typename OutImageType::IndexType OutIndexType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef double Scalar;
  typedef itk::ants::antsMatrixUtilities<ImageType,Scalar>  matrixOpType;
  typename matrixOpType::Pointer matrixOps=matrixOpType::New();

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn_label = std::string(argv[argct]);   argct++;
  unsigned int n_comp_corr_vecs = 4;// number of eigenvectors to get from high variance voxels  
  if ( argc > argct ) n_comp_corr_vecs = atoi(argv[argct]);   argct++;
  std::string::size_type idx;
  idx = outname.find_first_of('.');
  std::string tempname = outname.substr(0,idx);
  std::string extension = outname.substr(idx,outname.length());

  typename ImageType::Pointer image1 = NULL;
  typename OutImageType::Pointer outimage = NULL;
  typename OutImageType::Pointer outimage2 = NULL;
  typename OutImageType::Pointer label_image = NULL;
  typename OutImageType::Pointer var_image = NULL;

  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;
  if (fn_label.length() > 3)   ReadImage<OutImageType>(label_image, fn_label.c_str());
  else return 1;
  if (fn_label.length() > 3)   ReadImage<OutImageType>(outimage, fn_label.c_str());
  if (fn_label.length() > 3)   ReadImage<OutImageType>(outimage2, fn_label.c_str());
  if (fn_label.length() > 3)   ReadImage<OutImageType>(var_image, fn_label.c_str());
  var_image->FillBuffer(0);
  outimage->FillBuffer(0);
  outimage2->FillBuffer(0);
  Rcpp::Rcout << " read images " << std::endl;
  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  Rcpp::Rcout << "timedims " << timedims << " size " <<image1->GetLargestPossibleRegion().GetSize() << std::endl;

  // first, count the label numbers
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> labIterator;
  labIterator vfIter2( label_image,  label_image->GetLargestPossibleRegion() );
  unsigned long ct_nuis=0;
  unsigned long ct_vox=0;
  Rcpp::Rcout << " verify input " << std::endl;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() == 1 ) { // in brain
	ct_vox++;
      }
    }
  Rcpp::Rcout << " counted " << ct_vox << " voxels " <<  std::endl;
  if ( ct_vox == 0 ) 
    {
      Rcpp::Rcout << ct_vox << " not enough voxels labeled as gm (or brain) " <<std::endl;
      return 1;
    }
  // step 1.  compute , in label 3 ( the nuisance region ), the representative value of the time series over the region.  at the same time, compute the average value in label 2 ( the reference region ).
  // step 2.  factor out the nuisance region from the activation at each voxel in the ROI (nonzero labels).
  // step 3.  compute the correlation of the reference region with every voxel in the roi.
  typedef vnl_matrix<Scalar>      timeMatrixType;
  typedef vnl_vector<Scalar>      timeVectorType;
  timeMatrixType mSample(timedims,ct_vox,0);
  unsigned long nuis_vox=0;
  timeVectorType sample(timedims,0);
  //  FIRST -- get high variance (in time) voxels 
  float maxvar=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() > 0 ) { // in-brain
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	float total=0;
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          sample(t)=pix;
	  total+=pix;
	}
	float mean=total/(float)timedims;
	float var=0;
	for (unsigned int t=0; t<timedims; t++){
	  var+=((sample(t)-mean)*(sample(t)-mean));
	}
	var/=(float)(timedims);
	var=sqrt(var);
	if ( var > maxvar ) maxvar=var;
	var_image->SetPixel(ind,var);
      }
    }
  Rcpp::Rcout << " got var " << std::endl;
  // now build the histogram
  unsigned int histsize=50;
  float binsize=maxvar/histsize;
  timeVectorType varhist(histsize,0);
  float varhistsum=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      float var=var_image->GetPixel(ind);
      if ( vfIter2.Get() > 0 && var > 0 ) { // in-brain
	int bin=(int)( var/binsize )-1;
	if ( bin < 0 ) bin=0;
	varhist[bin]+=1;
	varhistsum+=1;
      }
    }
  varhist=varhist/varhistsum;
  Rcpp::Rcout << " got var hist " << std::endl;
  float temp=0;
  float varval_csf=0;
  for (unsigned int j=0; j<histsize; j++)
    {
      temp+=varhist(j);
      float varth=(float)j/(float)histsize*maxvar;
      Rcpp::Rcout <<" j " << j << " temp " << temp << " varth " << varth << std::endl;
      if ( temp >= 0.95 && varval_csf <=  0 ) varval_csf=(float)j*binsize;
    }

  Rcpp::Rcout <<" maxvar " << maxvar << " varval_csf " << varval_csf << std::endl;
  ct_nuis=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( var_image->GetPixel(ind) > varval_csf  ) { // nuisance
	ct_nuis++;
      }
    } 
  timeMatrixType mNuisance(timedims,ct_nuis,0);
  nuis_vox=0;
  unsigned long brain_vox=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( var_image->GetPixel(ind) > varval_csf  ) { // nuisance
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          mNuisance(t,nuis_vox)=pix;
	}
	nuis_vox++;
      }
      if ( vfIter2.Get() > 0  ) { // in brain
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          mSample(t,brain_vox)=pix;
	}
	brain_vox++;
      }
    }
  // factor out the nuisance variables by OLS
  timeVectorType vGlobal=matrixOps->AverageColumns(mSample);
  //  typedef itk::Array2D<double> csvMatrixType;
  timeMatrixType reducedNuisance(timedims,n_comp_corr_vecs+1);
  std::vector<std::string> ColumnHeaders;
  std::string colname=std::string("GlobalSignal");
  ColumnHeaders.push_back( colname );
  reducedNuisance.set_column(0,vGlobal);
  if ( ct_nuis <= 0 ) n_comp_corr_vecs=1;
  for (unsigned int i=0; i<n_comp_corr_vecs;i++) {
    timeVectorType nuisi=matrixOps->GetCovMatEigenvector(mNuisance,i);
    reducedNuisance.set_column(i+1,nuisi);
    colname=std::string("CompCorrVec")+ants_to_string<unsigned int>(i+1);
    ColumnHeaders.push_back( colname );
  }

  // write out these nuisance variables
  // write out the array2D object
  typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
  WriterType::Pointer writer = WriterType::New();
  std::string kname=tempname+std::string("_compcorr.csv");
  writer->SetFileName( kname );
  writer->SetInput( &reducedNuisance );
  writer->SetColumnHeaders( ColumnHeaders );
  try
    {
      writer->Write();
    }
  catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
  

  timeMatrixType RRt=matrixOps->ProjectionMatrix(reducedNuisance);
  mSample=matrixOps->NormalizeMatrix(mSample);
  mSample=mSample-RRt*mSample;
  brain_vox=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() > 0 ) {
	timeVectorType samp=mSample.get_column(brain_vox);
// correct the original image
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  image1->SetPixel(tind,samp[t]);
	}
	brain_vox++;
      }
    }
  kname=tempname+std::string("_corrected")+extension;
  WriteImage<ImageType>(image1,kname.c_str());
  kname=tempname+std::string("_variance")+extension;
  WriteImage<OutImageType>(var_image,kname.c_str());
  return 0;

}



template<unsigned int ImageDimension>
int CompCorr(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef typename OutImageType::IndexType OutIndexType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef float Scalar;
  typedef itk::ants::antsMatrixUtilities<ImageType,Scalar>  matrixOpType;
  typename matrixOpType::Pointer matrixOps=matrixOpType::New();

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn_label = std::string(argv[argct]);   argct++;
  float compcorr_sigma = 0; 
  if ( argc > argct ) compcorr_sigma = atof(argv[argct]);   argct++;
  std::string::size_type idx;
  idx = outname.find_first_of('.');
  std::string tempname = outname.substr(0,idx);
  std::string extension = outname.substr(idx,outname.length());

  typename ImageType::Pointer image1 = NULL;
  typename OutImageType::Pointer outimage = NULL;
  typename OutImageType::Pointer outimage2 = NULL;
  typename OutImageType::Pointer label_image = NULL;
  typename OutImageType::Pointer var_image = NULL;

  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  else return 1;
  if (fn_label.length() > 3)   ReadImage<OutImageType>(label_image, fn_label.c_str());
  else return 1;
  if (fn_label.length() > 3)   ReadImage<OutImageType>(outimage, fn_label.c_str());
  if (fn_label.length() > 3)   ReadImage<OutImageType>(outimage2, fn_label.c_str());
  if (fn_label.length() > 3)   ReadImage<OutImageType>(var_image, fn_label.c_str());
  var_image->FillBuffer(0);
  outimage->FillBuffer(0);
  outimage2->FillBuffer(0);
  Rcpp::Rcout << " read images " << std::endl;
  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  Rcpp::Rcout << "timedims " << timedims << " size " <<image1->GetLargestPossibleRegion().GetSize() << std::endl;

  // first, count the label numbers
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> labIterator;
  labIterator vfIter2( label_image,  label_image->GetLargestPossibleRegion() );
  unsigned long ct_nuis=0;
  unsigned long ct_ref=0;
  unsigned long ct_gm=0;
  Rcpp::Rcout << " verify input " << std::endl;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() == 3 ) { // nuisance
	ct_nuis++;
      }
      if ( vfIter2.Get() == 2 ) { // reference
	ct_ref++;
      }
      if ( vfIter2.Get() > 0 ) { // gm roi
	ct_gm++;
      }
    }
  Rcpp::Rcout << " counted " << ct_gm << " gm voxels " << ct_ref << " reference region voxels " << std::endl;
  if ( ct_gm == 0 ) 
    {
      Rcpp::Rcout << ct_gm << " not enough voxels labeled as gm (or brain) " << ct_gm <<std::endl;
      return 1;
    }
  if ( ct_ref == 0 ) 
    {
      Rcpp::Rcout << ct_ref << " not enough voxels labeled as reference region " <<std::endl;
      return 1;
    }
  // step 1.  compute , in label 3 ( the nuisance region ), the representative value of the time series over the region.  at the same time, compute the average value in label 2 ( the reference region ).
  // step 2.  factor out the nuisance region from the activation at each voxel in the ROI (nonzero labels).
  // step 3.  compute the correlation of the reference region with every voxel in the roi.
  typedef vnl_matrix<Scalar>      timeMatrixType;
  typedef vnl_vector<Scalar>      timeVectorType;
  timeMatrixType mReference(timedims,ct_ref,0);
  timeMatrixType mSample(timedims,ct_gm,0);
  unsigned long nuis_vox=0;
  unsigned long ref_vox=0;
  unsigned long gm_vox=0;
  timeVectorType smoother(timedims,0);
  timeVectorType smoother_out(timedims,0);
  //  FIRST -- get high variance (in time) voxels 
  float maxvar=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() > 0 ) { // in-brain
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	float total=0;
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          smoother(t)=pix;
	  total+=pix;
	}
	float mean=total/(float)timedims;
	float var=0;
	for (unsigned int t=0; t<timedims; t++){
	  var+=((smoother(t)-mean)*(smoother(t)-mean));
	}
	var/=(float)(timedims);
	var=sqrt(var);
	if ( var > maxvar ) maxvar=var;
	var_image->SetPixel(ind,var);
      }
    }
  Rcpp::Rcout << " got var " << std::endl;
  // now build the histogram
  unsigned int histsize=50;
  float binsize=maxvar/histsize;
  timeVectorType varhist(histsize,0);
  float varhistsum=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      float var=var_image->GetPixel(ind);
      if ( vfIter2.Get() > 0 && var > 0 ) { // in-brain
	int bin=(int)( var/binsize )-1;
	if ( bin < 0 ) bin=0;
	varhist[bin]+=1;
	varhistsum+=1;
      }
    }
  varhist=varhist/varhistsum;
  Rcpp::Rcout << " got var hist " << std::endl;
  float temp=0;
  float varval_csf=0;
  for (unsigned int j=0; j<histsize; j++)
    {
      temp+=varhist(j);
      float varth=(float)j/(float)histsize*maxvar;
      Rcpp::Rcout <<" j " << j << " temp " << temp << " varth " << varth << std::endl;
      if ( temp >= 0.95 && varval_csf <=  0 ) varval_csf=(float)j*binsize;
    }

  Rcpp::Rcout <<" maxvar " << maxvar << " varval_csf " << varval_csf << std::endl;
  //  WriteImage<OutImageType>(var_image,"varimage.nii.gz");
  //
  ct_nuis=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( var_image->GetPixel(ind) > varval_csf  ) { // nuisance
	ct_nuis++;
      }
    } 
  timeMatrixType mNuisance(timedims,ct_nuis,0);
  Rcpp::Rcout <<" begin smoothing " << std::endl;
  if ( compcorr_sigma > 1.e-5 )
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() > 0 ) { // in-brain
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          smoother(t)=pix;
	}
	for (unsigned int t=0; t<timedims; t++){
	  int offset=25;
	  int lo=t-offset; if ( lo < 0 ) lo=0; 
          int hi=t+offset; if ( hi > timedims-1 ) hi=timedims-1; 
	  float total=0;
  	  for (unsigned int s=lo; s<hi; s++){
	    float diff=(float)s-(float)t;
            float wt=exp(-1.0*diff*diff/(2.0*compcorr_sigma*compcorr_sigma));
	    total+=wt;
	    smoother_out(t)+=wt*smoother(s);
	  }
	  smoother_out(t)/=total;
	}
      }
    }
  Rcpp::Rcout <<" smooth done " << std::endl;
  ref_vox=0; nuis_vox=0; gm_vox=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      //      if ( vfIter2.Get() == 3 ) { // nuisance
      if ( var_image->GetPixel(ind) > varval_csf  ) { // nuisance
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          mNuisance(t,nuis_vox)=pix;
	}
	nuis_vox++;
      }
      if ( vfIter2.Get() == 2 ) { // reference
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          mReference(t,ref_vox)=pix;
	}
	ref_vox++;
      }
      if ( vfIter2.Get() > 0  ) { // in brain
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  Scalar pix=image1->GetPixel(tind);
          mSample(t,gm_vox)=pix;
	}
	gm_vox++;
      }
    }
  // factor out the nuisance variables by OLS
  unsigned int nnuis=3;// number of eigenvectors to get from high variance voxels 
  if ( ct_nuis <= 0 ) nnuis=1;
  timeMatrixType reducedNuisance(timedims,nnuis);
  for (unsigned int i=0; i<nnuis;i++) {
    timeVectorType nuisi=matrixOps->GetCovMatEigenvector(mNuisance,i);
    reducedNuisance.set_column(i,nuisi);
  }
  timeVectorType vGlobal=matrixOps->AverageColumns(mSample);
  reducedNuisance.set_column(nnuis-1,vGlobal);
  timeMatrixType RRt=matrixOps->ProjectionMatrix(reducedNuisance);
  mReference=matrixOps->NormalizeMatrix(mReference);
  mReference=mReference-RRt*mReference;
  mSample=matrixOps->NormalizeMatrix(mSample);
  mSample=mSample-RRt*mSample;
  // reduce your reference region to the first & second eigenvector
  timeVectorType vReference=matrixOps->GetCovMatEigenvector(mReference,0);
  timeVectorType vReference2=matrixOps->AverageColumns(mReference);
  Scalar testcorr=matrixOps->PearsonCorr(vReference,vReference2);
  if ( testcorr < 0 ) vReference=vReference*(-1);
  if ( vReference.size() != timedims ) { Rcpp::Rcout << " CompCorr Error exiting " << std::endl; throw std::exception() ; }
  gm_vox=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( vfIter2.Get() > 0 ) {
	timeVectorType samp=mSample.get_column(gm_vox);
// correct the original image
	IndexType tind;
	for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
	for (unsigned int t=0; t<timedims; t++){
          tind[ImageDimension-1]=t;
	  image1->SetPixel(tind,samp[t]);
	}
// compute the gm-reference correlation
	Scalar corr=matrixOps->PearsonCorr(samp,vReference);
	//	Scalar corr2=matrixOps->PearsonCorr(samp,vReference2);
	outimage->SetPixel(ind,corr);
	//	outimage2->SetPixel(ind,corr2);
	outimage2->SetPixel(ind,samp.two_norm());// the power of the time series
	gm_vox++;
      }
    }
  Rcpp::Rcout<<"write results"<<std::endl;
  std::string kname=tempname+std::string("first_evec")+extension;
  WriteImage<OutImageType>(outimage,kname.c_str());
  //  kname=tempname+std::string("second_evec")+extension;
  kname=tempname+std::string("power")+extension;
  WriteImage<OutImageType>(outimage2,kname.c_str());
  kname=tempname+std::string("_corrected")+extension;
  WriteImage<ImageType>(image1,kname.c_str());
  kname=tempname+std::string("_variance")+extension;
  WriteImage<OutImageType>(var_image,kname.c_str());
  return 0;

}


template<unsigned int ImageDimension>
int StackImage(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (argc > argct) fn2=std::string(argv[argct]);   argct++;

  typename ImageType::Pointer image1 = NULL;
  if (fn1.length() > 3)   ReadImage<ImageType>(image1, fn1.c_str());
  typename ImageType::Pointer image2 = NULL;
  if (fn2.length() > 3)   ReadImage<ImageType>(image2, fn2.c_str());

  typename ImageType::PointType origin=image1->GetOrigin();
  typename ImageType::PointType origin2=image1->GetOrigin();

  typename ImageType::SizeType size=image1->GetLargestPossibleRegion().GetSize();
  typename ImageType::SizeType newsize=image1->GetLargestPossibleRegion().GetSize();
  typename ImageType::RegionType newregion;
  // determine new image size

  newsize[ImageDimension-1]=(unsigned int)newsize[ImageDimension-1]+image2->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  Rcpp::Rcout << " oldsize " << size <<  " newsize " << newsize << std::endl;
  newregion.SetSize(newsize);
  newregion.SetIndex(image1->GetLargestPossibleRegion().GetIndex());

  typename ImageType::Pointer padimage = ImageType::New();
  padimage->SetSpacing(image1->GetSpacing());
  padimage->SetOrigin(origin2);
  padimage->SetDirection(image1->GetDirection());
  padimage->SetRegions(newregion );
  padimage->Allocate();
  padimage->FillBuffer( 0 );
  typename ImageType::IndexType index; index.Fill(0);
  typename ImageType::IndexType index2; index2.Fill(0);
  typename ImageType::PointType point1,pointpad;
  image1->TransformIndexToPhysicalPoint(index, point1);
  padimage->TransformIndexToPhysicalPoint(index2, pointpad);
  //  Rcpp::Rcout << " pre " << point1 << " pad " << pointpad << std::endl;
  for (unsigned int i=0; i<ImageDimension; i++)
    origin2[i]+=(point1[i]-pointpad[i]);
  padimage->SetOrigin(origin2);
  float padvalue=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  Iterator iter( padimage,  padimage->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      typename ImageType::IndexType oindex=iter.GetIndex();
      typename ImageType::IndexType padindex=iter.GetIndex();
      bool isinside=false;
      if ( oindex[ImageDimension-1]  >= padvalue ) isinside=true;
      if (isinside)
	{
	  float shifted=((float)oindex[ImageDimension-1]-padvalue);
	  padindex[ImageDimension-1]=(unsigned int)shifted;
	  padimage->SetPixel(oindex,image2->GetPixel(padindex));
	}
      else padimage->SetPixel(oindex,image1->GetPixel(oindex));

    }


  WriteImage<ImageType>(padimage,outname.c_str());


  return 0;

}


template<unsigned int ImageDimension>
int MakeImage(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  unsigned int sizevalx = atoi(argv[argct]);   argct++;
  unsigned int sizevaly=0;
  if (argc >argct) { sizevaly=atoi(argv[argct]); argct++; }
   unsigned int sizevalz=0;
   if (argc >argct && ImageDimension == 3) { sizevalz=atoi(argv[argct]); argct++; }

  typename ImageType::SizeType size;
  size[0]=sizevalx;
  size[1]=sizevaly;
  if ( ImageDimension == 3) size[2]=sizevalz;
  typename ImageType::RegionType newregion;
  Rcpp::Rcout << " size " << size << std::endl;
  newregion.SetSize(size);

  typename ImageType::Pointer padimage = ImageType::New();
  typename ImageType::SpacingType spacing;
  spacing.Fill(1);
  typename ImageType::PointType origin;
  origin.Fill(0);
  padimage->SetSpacing(spacing);
  padimage->SetOrigin(origin);
  padimage->SetRegions(newregion );
  padimage->Allocate();
  padimage->FillBuffer( 0 );

  WriteImage<ImageType>(padimage,outname.c_str());

  return 0;

}



template <class TImage>
typename TImage::Pointer
LabelSurface(typename TImage::Pointer input, typename TImage::Pointer input2  )
{
Rcpp::Rcout << " Label Surf " << std::endl;
  typedef TImage ImageType;
  enum { ImageDimension = ImageType::ImageDimension };
  typename   ImageType::Pointer     Image = ImageType::New();
  Image->SetLargestPossibleRegion(input->GetLargestPossibleRegion()  );
  Image->SetBufferedRegion(input->GetLargestPossibleRegion());
  Image->Allocate();
  Image->SetSpacing(input->GetSpacing());
  Image->SetOrigin(input->GetOrigin());
  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (int j=0; j<ImageDimension; j++) rad[j]=1;
  iteratorType GHood(rad, input,input->GetLargestPossibleRegion());
  GHood.GoToBegin();
  while (!GHood.IsAtEnd())
  {
    typename TImage::PixelType p = GHood.GetCenterPixel();
    typename TImage::IndexType ind = GHood.GetIndex();
    typename TImage::IndexType ind2;
    if ( p >= 0.5 )
    {
      bool atedge=false;

      for (unsigned int i = 0; i < GHood.Size(); i++)
      {
        ind2=GHood.GetIndex(i);
        float dist=0.0;
        for (int j=0; j<ImageDimension; j++)
          dist+=(float)(ind[j]-ind2[j])*(float)(ind[j]-ind2[j]);
        dist=sqrt(dist);
	bool secondval=true;
	if (input2) if (input2->GetPixel(ind2) >= 0.5 ) secondval=true;
	if (GHood.GetPixel(i) < 0.5 && dist <  2. && secondval  )
        {
          atedge=true;
        }
      }
      if (atedge && p >=  0.5) Image->SetPixel(ind,1);
      else  Image->SetPixel(ind,0);
    }
    ++GHood;
  }
  return Image;
}


template<unsigned int ImageDimension>
int FitSphere(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::string(argv[1]) << std::endl; return 1; }
  /*
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (argc > argct) fn2=std::string(argv[argct]);   argct++;
  float MaxRad=5;
  if (argc > argct) MaxRad = atof(argv[argct]);   argct++;

  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer radimage = NULL;
  typename ImageType::Pointer radimage2 = NULL;
  typename ImageType::Pointer priorimage = NULL;
  typename ImageType::Pointer wmimage = NULL;
  if (fn2.length() > 3)   ReadImage<ImageType>(wmimage, fn2.c_str());
  Rcpp::Rcout <<"  read " << fn1 << " MXR " << MaxRad << std::endl;
  ReadImage<ImageType>(image1, fn1.c_str());
  ReadImage<ImageType>(radimage, fn1.c_str());
  ReadImage<ImageType>(radimage2, fn1.c_str());
  ReadImage<ImageType>(priorimage, fn1.c_str());
  radimage->FillBuffer(0);
  radimage2->FillBuffer(0);
  priorimage->FillBuffer(0);
  typename ImageType::SpacingType spacing=image1->GetSpacing();

  typename ImageType::Pointer surf = LabelSurface<ImageType>(image1,wmimage);

  typedef itk::LinearInterpolateImageFunction<ImageType,float> ScalarInterpolatorType;
  typename ScalarInterpolatorType::Pointer ginterp =  ScalarInterpolatorType::New();
  ginterp->SetInputImage(image1);
  typename ScalarInterpolatorType::ContinuousIndexType  Y1;
  typename ScalarInterpolatorType::ContinuousIndexType  Y2;
  typename ScalarInterpolatorType::ContinuousIndexType  GMx;
  typename ScalarInterpolatorType::ContinuousIndexType  WMx;
  typename ScalarInterpolatorType::Pointer winterp=NULL;
  if (wmimage)
    {
      winterp=ScalarInterpolatorType::New();
      winterp->SetInputImage(wmimage);
    }
  //  float x=0,y=0,z=0;
  //  float xc=0,yc=0,zc=0;
  float globalbestrad=0;
  typename ImageType::IndexType bestind;
  bestind.Fill(0);
  typename ImageType::IndexType ind2;
  ind2.Fill(0);
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image1,  image1->GetLargestPossibleRegion() );
  Rcpp::Rcout <<"  Begin " << std::endl;
  unsigned long npx=0;
//  float pi=3.141;
 unsigned long numpx=image1->GetBufferedRegion().GetNumberOfPixels();
  //unsigned int prog=0;

//  float gmtotal,wmtotal,gvol,wvol,warea,garea;
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      npx++;
      typename ImageType::IndexType ind=iter.GetIndex();
//      float val=surf->GetPixel(ind);//iter.Get();
//      float minrad=1.e9;
*/
      /*
      if (val > 0.5 && fabs((float)ind[2]-80.) < 6 )
	{
	  //float  bestrad=0;
	  //parameterize sphere at this index
	  float epspi=pi*0.1;
	  float epsrad=0.2;
	  gvol=0;wvol=0;warea=0;garea=0;
	  float svol=0;//4./3.*pi*MaxRad*MaxRad*MaxRad;
	  //	  float sarea=0;//4.*pi*MaxRad*MaxRad;
	  //float bestvol=0;
	  //	  float wdiff=0;
	  //	  for (float theta=0; theta<=pi; theta+=epspi)
	  float theta=0;
	  gmtotal=0;
	  wmtotal=0;
	  float glength=0,wlength=0;
	  while ( theta < pi )
	    {
	  garea=0;warea=0;
	  float psi=0;
	  while ( psi < pi )
	    {
	      glength=0;wlength=0;
	      float rr=0;
	      bool raddone=false;
	      GMx.Fill(0);  WMx.Fill(0);
	      while ( rr <= MaxRad && !raddone)
		{
		  // integrate the wm/gm probability along this radius, at these angles
		  Y1[0]=(float)ind[0]/spacing[0]+rr*cos(psi)*sin(theta);
		  Y1[1]=(float)ind[1]/spacing[1]+rr*sin(psi)*sin(theta);
		  Y1[2]=(float)ind[2]/spacing[2]+rr*cos(theta);
		  Y2[0]=(float)ind[0]/spacing[0]+rr*cos(psi+pi)*sin(theta);
		  Y2[1]=(float)ind[1]/spacing[1]+rr*sin(psi+pi)*sin(theta);
		  Y2[2]=(float)ind[2]/spacing[2]+rr*cos(theta);
		  float gval1 = ginterp->EvaluateAtContinuousIndex( Y1 );
		  float gval2 = ginterp->EvaluateAtContinuousIndex( Y2 );
		  float wval1=0,wval2=0;
		  if (wmimage) wval1 = winterp->EvaluateAtContinuousIndex( Y1 );
		  if (wmimage) wval2 = winterp->EvaluateAtContinuousIndex( Y2 );
		  glength+=(gval1+gval2)*epsrad*2;
		  wlength+=(wval2+wval2)*epsrad*2;
		  gmtotal+=(gval1+gval2);
		  wmtotal+=(wval1+wval2);
		  for (unsigned int dd=0; dd<ImageDimension; dd++)
		    {
		      GMx[dd]+=(Y1[dd]*gval1+Y2[dd]*gval2);
		      WMx[dd]+=(Y1[dd]*wval1+Y2[dd]*wval2);
		    }
		  rr+=epsrad;
		}//update rr

	      garea+=glength*epspi/pi;
	      warea+=wlength*epspi/pi;
	      psi+=epspi;
	    }//update psi
	  gvol+=garea;//epspi/pi;
	  wvol+=warea;//epspi/pi;
	  svol+=(gvol+wvol);
	  theta+=epspi;
	    }//update theta

		  for (unsigned int dd=0; dd<ImageDimension; dd++)
		    {
		      GMx[dd]/=gmtotal;
		      WMx[dd]/=wmtotal;
		    }
		  float cmdist=0;
		  for (unsigned int dd=0; dd<ImageDimension; dd++)
		    {
		      cmdist+=(GMx[dd]-WMx[dd])*(GMx[dd]-WMx[dd]);
		    }
		  cmdist=sqrt(cmdist);
		  //		  Rcpp::Rcout << " GMT " << gmtotal << " WMT " << wmtotal << " dist " << cmdist << std::endl;
	  float gmrad=pow( 3.*gvol/(4.*pi) , 1./3.);
	  float gwrat=0,gvrat=0;
	  if (warea > 0) gwrat=garea/warea;
	  if (wvol > 0) gvrat=gvol/wvol;
	  priorimage->SetPixel(ind,gmtotal/(wmtotal+gmtotal));
	  radimage->SetPixel(ind,cmdist);

	}
  */
/*
	  //	  radimage2->SetPixel(ind,gvrat);
      if (image1->GetPixel(ind) >= 0.5)
	{
      bool okfit=true;
      float dorad=1;
      float bestrad=1;
      while (okfit && dorad <= MaxRad )
	    {
	      typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
	      typename iteratorType::RadiusType rad,rad2;
	      rad2.Fill(0);
	      for (unsigned int j=0; j<ImageDimension; j++) rad[j]=(long unsigned int) dorad;
	      float tardist=(dorad*sqrt((double)2));
	      iteratorType GHood(rad, image1,image1->GetLargestPossibleRegion());
	      GHood.SetLocation(ind);
	      typename ImageType::PixelType p = GHood.GetCenterPixel();
	      unsigned int goodct=0;
	      unsigned int possct=0;
	      if ( p > 0 && radimage->GetPixel(ind) <= 0  )
		{
		  for (unsigned int i = 0; i < GHood.Size(); i++)
		    {
		      ind2=GHood.GetIndex(i);
		      float dist=sqrt(((float)ind2[0]-(float)ind[0])*((float)ind2[0]-(float)ind[0])+
				      ((float)ind2[1]-(float)ind[1])*((float)ind2[1]-(float)ind[1])+
				      ((float)ind2[2]-(float)ind[2])*((float)ind2[2]-(float)ind[2]));
		      if ( GHood.GetPixel(i) == p && dist <= tardist)
			{
			  goodct++;
			  possct++;
			}
		      else if ( dist <= tardist ) possct++;
		      //		  Rcpp::Rcout << " Ind " <<  ind << " : " <<  bestrad << " tardist " << tardist << " gct " << goodct <<" pos " << possct << " dist " << dist << " ind2 " << ind2 << std::endl;
		    }
		  if (goodct==possct)
		    {
		      bestrad=dorad; radimage->SetPixel(ind,bestrad*(-1.0));
		    }
		  else { okfit=false; radimage->SetPixel(ind,radimage->GetPixel(ind)*(-1.0)); }
		}
	      dorad=dorad+1;
	    }
	  if (bestrad >= globalbestrad)
	    {
	      globalbestrad=bestrad;
	      bestind=ind;
	    }

	  if (npx % 10000 == 0)
	    {
	      Rcpp::Rcout <<" prog " << (float)npx/(float)numpx << std::endl;
	      //	      WriteImage<ImageType>(radimage,outname.c_str());
	      //	      WriteImage<ImageType>(radimage2,(std::string("Sphere")+outname).c_str());
	      //WriteImage<ImageType>(priorimage,(std::string("Prior")+outname).c_str());
	    }
	}

    }


  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      float val=iter.Get();
      typename ImageType::IndexType ind=iter.GetIndex();
      if (val > 0)
	{
	  unsigned int dorad=(unsigned int)fabs(radimage->GetPixel(ind));
	  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
	  typename iteratorType::RadiusType rad;
	  for (unsigned int j=0; j<ImageDimension; j++) rad[j]= dorad;
	  float tardist=(dorad*sqrt((double)2));
	  float diameter=2.0*(float)dorad*sqrt((double)2);
	  iteratorType GHood(rad, image1,image1->GetLargestPossibleRegion());
	  GHood.SetLocation(ind);
	  typename ImageType::PixelType p = GHood.GetCenterPixel();
	  for (unsigned int i = 0; i < GHood.Size(); i++)
	    {
	      ind2=GHood.GetIndex(i);
	      float dist=sqrt(((float)ind2[0]-(float)ind[0])*((float)ind2[0]-(float)ind[0])+
			      ((float)ind2[1]-(float)ind[1])*((float)ind2[1]-(float)ind[1])+
			      ((float)ind2[2]-(float)ind[2])*((float)ind2[2]-(float)ind[2]));
	      if ( GHood.GetPixel(i) == p && dist <= tardist && diameter > priorimage->GetPixel(ind2))
		{
		  priorimage->SetPixel(ind2,diameter);
		}
	    }
	}
    }




  // now, make rad image
  Rcpp::Rcout << " Best " << bestind << " gbr " << globalbestrad << std::endl;
  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (unsigned int j=0; j<ImageDimension; j++) rad[j]= (long unsigned int)globalbestrad;
  iteratorType GHood(rad, image1,image1->GetLargestPossibleRegion());
  GHood.SetLocation(bestind);
  typename ImageType::PixelType p = GHood.GetCenterPixel();
  for (unsigned int i = 0; i < GHood.Size(); i++)
    {
      ind2=GHood.GetIndex(i);
      float dist=0;
      dist+=sqrt((float)(ind2[0]-(float)bestind[0])*(float)(ind2[0]-(float)bestind[0])+
		 (float)(ind2[1]-(float)bestind[1])*(float)(ind2[1]-(float)bestind[1])+
		 (float)(ind2[2]-(float)bestind[2])*(float)(ind2[2]-(float)bestind[2]));
      if ( dist <= (globalbestrad*sqrt((double)2)))
	{
	  radimage2->SetPixel(ind2,p);
	}
    }


  //  WriteImage<ImageType>(radimage,outname.c_str());
  WriteImage<ImageType>(radimage2,outname.c_str());
  //WriteImage<ImageType>(priorimage,(std::string("Prior")+outname).c_str());
 */
  return 0;

}




template<unsigned int ImageDimension>
int ImageMath(int argc, char *argv[])
{
  typedef float  PixelType;
  //  const unsigned int ImageDimension = AvantsImageDimension;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2="";
  if (argc > argct) { fn2=std::string(argv[argct]);   }


  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;
  typename ImageType::Pointer varimage = NULL;

  typename readertype::Pointer reader2 = readertype::New();
  typename readertype::Pointer reader1 = readertype::New();
  reader2->SetFileName(fn2.c_str());



  bool isfloat=false;
  float floatval=1.0;
  if(from_string<float>(floatval, fn2, std::dec) )
  	{
    isfloat=true;
  	}
  else
  	{
  	reader2->Update();
  	image2 = reader2->GetOutput();
  	}

  reader1->SetFileName(fn1.c_str());
  try
    {
      reader1->UpdateLargestPossibleRegion();
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  varimage=ImageType::New();
  varimage->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
  varimage->SetBufferedRegion( image1->GetLargestPossibleRegion() );
  varimage->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
  varimage->Allocate();
  varimage->SetSpacing(image1->GetSpacing());
  varimage->SetOrigin(image1->GetOrigin());
  varimage->SetDirection(image1->GetDirection());


  if (strcmp(operation.c_str(),"mresample") == 0 && !isfloat )
    {
      typename ImageType::SpacingType spc=image2->GetSpacing();
      typedef itk::TranslationTransform< double, ImageDimension> TransformType0;
      typename TransformType0::Pointer             m_Transform0=TransformType0::New();
      typename TransformType0::ParametersType trans=m_Transform0->GetParameters();
      for( unsigned int i=0; i<ImageDimension; i++)
	{
	  trans[i] = 0;
	  spc[i]=image1->GetSpacing()[i]*image1->GetLargestPossibleRegion().GetSize()[i]/image2->GetLargestPossibleRegion().GetSize()[i];
	}
      image2->SetSpacing(spc);
      image2->SetOrigin(image1->GetOrigin());
      image2->SetDirection(image1->GetDirection());
      m_Transform0->SetParameters(trans);
      Rcpp::Rcout <<" trans " << m_Transform0->GetParameters() << " Nspc " << image2->GetSpacing() << std::endl;
      typedef itk::ResampleImageFilter< ImageType, ImageType >	ResampleFilterType;
      typename ResampleFilterType::Pointer resample = ResampleFilterType::New();
      resample->SetTransform( m_Transform0 );
      resample->SetInput( image2 );
      resample->SetSize( image1->GetLargestPossibleRegion().GetSize() );
      resample->SetOutputOrigin(  image1->GetOrigin() );
      resample->SetOutputSpacing( image1->GetSpacing() );
      typename ImageType::IndexType zeroind;  zeroind.Fill(0);
      resample->SetDefaultPixelValue( image1->GetPixel(zeroind) );
      resample->UpdateLargestPossibleRegion();
      image2=resample->GetOutput();

      WriteImage<ImageType>(image2,outname.c_str());
      return 0;
    }

  float volumeelement=1.0;
  for (unsigned int i=0;  i<ImageDimension; i++)  volumeelement*=varimage->GetSpacing()[i];


  float result=0;
  Iterator vfIter2( varimage,  varimage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      IndexType ind=vfIter2.GetIndex();
      float pix2;
      if (isfloat) pix2= floatval;
      else pix2=image2->GetPixel(ind);
      float pix1 = image1->GetPixel(ind);

      if (strcmp(operation.c_str(),"m") == 0) result=pix1*pix2;
     else if (strcmp(operation.c_str(),"+") == 0)  result=pix1+pix2;
     else if (strcmp(operation.c_str(),"-") == 0)  result=pix1-pix2;
     else if (strcmp(operation.c_str(),"/") == 0) { if (pix2 > 0)  result=pix1/pix2; }
     else if (strcmp(operation.c_str(),"^") == 0) {  result=pow(pix1,pix2); }
     else if (strcmp(operation.c_str(),"exp") == 0) {  result=exp(pix1*pix2); }
     else if (strcmp(operation.c_str(),"abs") == 0) {  result=fabs(pix1); }
     else if (strcmp(operation.c_str(),"addtozero") == 0 && pix1 == 0) {  result=pix1+pix2; }
     else if (strcmp(operation.c_str(),"addtozero") == 0 && pix1 != 0) {  result=pix1; }
     else if (strcmp(operation.c_str(),"overadd") == 0 && pix2 != 0) {  result=pix2; }
     else if (strcmp(operation.c_str(),"overadd") == 0 ) {  result=pix1; }
     else if (strcmp(operation.c_str(),"Decision") == 0 ) {  result=1./(1.+exp(-1.0*( pix1-0.25)/pix2));}
     else if (strcmp(operation.c_str(),"total") == 0 ) {  result+=pix1*pix2 ;}

      vfIter2.Set(result);
    }
  if (strcmp(operation.c_str(),"total") == 0 ) Rcpp::Rcout << "total: " << result << " total-volume: " << result*volumeelement << std::endl;
  else Rcpp::Rcout << "operation " << operation << std::endl;
  if ( outname.length() > 3 )
    WriteImage<ImageType>(varimage,outname.c_str());


  return 0;

}

template<unsigned int ImageDimension>
int TensorFunctions(int argc, char *argv[])
{
  typedef float  PixelType;
  // Tensor4DType may contain b0
  typedef float D4TensorType;
  typedef itk::SymmetricSecondRankTensor< float, 3 >  TensorType;
  typedef typename itk::RGBPixel<unsigned char> RGBType;
  typedef itk::Image<TensorType, ImageDimension> TensorImageType;
  typedef itk::Image<D4TensorType, 4>  D4TensorImageType;
  typedef typename TensorImageType::IndexType IndexType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<RGBType,ImageDimension> ColorImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef itk::ImageFileWriter<ColorImageType> ColorWriterType;
  typedef itk::ImageRegionIteratorWithIndex<TensorImageType> Iterator;

  typedef itk::Vector<float , ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension> VectorImageType;
  typedef itk::ImageRegionIteratorWithIndex<VectorImageType> vecIterator;
  typedef itk::Matrix<float,3,3> MatrixType;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;

  typename TensorImageType::Pointer timage = NULL; // input tensor image
  typename ImageType::Pointer       vimage = NULL; // output scalar image
  typename ColorImageType::Pointer  cimage = NULL; // output color image
  typename VectorImageType::Pointer  vecimage = NULL; // output vector image
  typename TensorImageType::Pointer toimage = NULL; // output tensor image

  if (strcmp(operation.c_str(), "4DTensorTo3DTensor") == 0) {
    Rcpp::Rcout <<" Convert a 4D tensor to a 3D tensor --- if there are 7 components to the tensor, we throw away the first component b/c its probably b0 " << std::endl;
    itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(fn1.c_str(), itk::ImageIOFactory::ReadMode);
    imageIO->SetFileName(fn1.c_str());
    imageIO->ReadImageInformation();
    unsigned int dim=imageIO->GetNumberOfDimensions();
    if ( dim == 4 )
      {
	typename D4TensorImageType::Pointer d4img=NULL;
	ReadImage<D4TensorImageType>(d4img, fn1.c_str());
	unsigned int d4size=d4img->GetLargestPossibleRegion().GetSize()[3];
	if ( d4size != 6 && d4size != 7 )
	  {
	    Rcpp::Rcout << " you should not be using this function if the input data is not a tensor. " << std::endl;
	    Rcpp::Rcout << " there is no way for us to really check if your use of this function is correct right now except checking the size of the 4th dimension which should be 6 or 7 (the latter if you store b0 in the first component) --- you should really store tensors not as 4D images but as 3D images with tensor voxel types. " << std::endl;
	    throw std::exception() ;
	  }
	typename TensorImageType::SizeType size;
	typename TensorImageType::RegionType tensorregion;
	typename TensorImageType::SpacingType spacing;
	typename TensorImageType::PointType origin;
	typename TensorImageType::DirectionType direction;
	for ( unsigned int dd=0; dd<ImageDimension; dd++){
  	  size[dd]=d4img->GetLargestPossibleRegion().GetSize()[dd];
	  origin[dd]=d4img->GetOrigin()[dd];
	  spacing[dd]=d4img->GetSpacing()[dd];
  	    for ( unsigned int ee=0; ee<ImageDimension; ee++)
	      direction[dd][ee]=d4img->GetDirection()[dd][ee];
	}
	tensorregion.SetSize(size);
	timage = TensorImageType::New();
	timage->SetRegions( tensorregion );
	timage->Allocate();
	timage->SetSpacing(spacing);
	timage->SetOrigin(origin);
	timage->SetDirection(direction);

	// now iterate through & set the values of the tensors.
	Iterator tIter(timage, timage->GetLargestPossibleRegion() );
	for(  tIter.GoToBegin(); !tIter.IsAtEnd(); ++tIter )
	  {
	    typename TensorImageType::IndexType ind=tIter.GetIndex();
	    typename D4TensorImageType::IndexType ind2;
	    for ( unsigned int dd=0; dd<ImageDimension; dd++) ind2[dd]=ind[dd];
	    TensorType pix6=tIter.Get();
	    if ( d4size == 6 ) {
	      for ( unsigned int ee=0; ee<d4size; ee++)
		{
		  ind2[3]=ee;
		  if ( ee ==  2 )
		  pix6[2]=d4img->GetPixel(ind2);
		  else if ( ee ==  3 )
		  pix6[3]=d4img->GetPixel(ind2);
		  else
		  pix6[ee]=d4img->GetPixel(ind2);
		}
	    }
	    // ITK-way
	    //  xx, xy, yy, xz , yz, zz
	    // VTK/other-way
	    //  xx, xy, xz, yy , yz, zz
	    else if ( d4size == 7 ) {
	      for ( unsigned int ee=1; ee<d4size; ee++)
		{
		  ind2[3]=ee;
		  pix6[ee-1]=d4img->GetPixel(ind2);
		}
	    }
	    timage->SetPixel(ind,pix6);
	  }
	WriteTensorImage<TensorImageType>(timage, outname.c_str() ,false);
	return 0;
      }
    Rcpp::Rcout << " cannot convert --- input image not 4D --- " << fn1 << std::endl;
    return 0;
  }

  if( strcmp(operation.c_str(), "ComponentTo3DTensor") == 0 )
    {
    std::string extension = std::string( ".nii.gz" );
    if( argc > argct )
      {
      extension = std::string( argv[argct++] );
      }

    typename ImageType::Pointer xx, xy, xz, yy, yz, zz;

    std::string fn1xx = fn1 + std::string( "xx" ) + extension;
    std::string fn1xy = fn1 + std::string( "xy" ) + extension;
    std::string fn1xz = fn1 + std::string( "xz" ) + extension;
    std::string fn1yy = fn1 + std::string( "yy" ) + extension;
    std::string fn1yz = fn1 + std::string( "yz" ) + extension;
    std::string fn1zz = fn1 + std::string( "zz" ) + extension;

    ReadImage<ImageType>( xx, fn1xx.c_str() );
    ReadImage<ImageType>( xy, fn1xy.c_str() );
    ReadImage<ImageType>( xz, fn1xz.c_str() );
    ReadImage<ImageType>( yy, fn1yy.c_str() );
    ReadImage<ImageType>( yz, fn1yz.c_str() );
    ReadImage<ImageType>( zz, fn1zz.c_str() );

    timage = TensorImageType::New();
    timage->CopyInformation( xx );
    timage->SetRegions( xx->GetLargestPossibleRegion() );
    timage->Allocate();

    Iterator tIter( timage, timage->GetLargestPossibleRegion() );
    for( tIter.GoToBegin(); !tIter.IsAtEnd(); ++tIter )
      {
      typename TensorImageType::IndexType ind = tIter.GetIndex();
      TensorType pix6 = tIter.Get();

      pix6[0] = xx->GetPixel( ind );
      pix6[1] = xy->GetPixel( ind );
      pix6[2] = xz->GetPixel( ind );
      pix6[3] = yy->GetPixel( ind );
      pix6[4] = yz->GetPixel( ind );
      pix6[5] = zz->GetPixel( ind );

      tIter.Set( pix6 );
      }
    WriteTensorImage<TensorImageType>( timage, outname.c_str() ,false );
    return 0;
    }

  if( strcmp(operation.c_str(), "ExtractComponentFrom3DTensor") == 0 )
    {
    ReadImage<TensorImageType>( timage, fn1.c_str() );

    unsigned int which = 0;
    if( argc > argct )
      {
      std::string component = std::string( argv[argct++] );
      if( component.find( "xx" ) != std::string::npos )
        {
        which = 0;
        }
      else if( component.find( "xy" ) != std::string::npos )
        {
        which = 1;
        }
      else if( component.find( "xz" ) != std::string::npos )
        {
        which = 2;
        }
      else if( component.find( "yy" ) != std::string::npos )
        {
        which = 3;
        }
      else if( component.find( "yz" ) != std::string::npos )
        {
        which = 4;
        }
      else if( component.find( "zz" ) != std::string::npos )
        {
        which = 5;
        }
      else
        {
        Rcpp::Rcout << "Unrecognized component.  Need to specify " <<
          "xx, xy, xz, yy, yz, or zz";
        return EXIT_FAILURE;
        }
      }
    else
      {
      Rcpp::Rcout << "Error:  need to specify component (xx, xy, xz, yy, yz, zz)";
      return EXIT_FAILURE;
      }

    typename ImageType::Pointer componentImage = ImageType::New();
    componentImage->CopyInformation( timage );
    componentImage->SetRegions( timage->GetLargestPossibleRegion() );
    componentImage->Allocate();
    componentImage->FillBuffer( 0.0 );

    Iterator tIter( timage, timage->GetLargestPossibleRegion() );
    for( tIter.GoToBegin(); !tIter.IsAtEnd(); ++tIter )
      {
      typename TensorImageType::IndexType ind = tIter.GetIndex();
      TensorType pix6 = tIter.Get();

      componentImage->SetPixel( ind, pix6[which] );
      }
    WriteTensorImage<TensorImageType>( timage, outname.c_str() ,false );
    return 0;
    }

  unsigned int whichvec=ImageDimension-1;
  if (argc > argct) { whichvec=atoi(argv[argct]);   } argct++;

  ReadTensorImage<TensorImageType>(timage,fn1.c_str(),false);

  if (strcmp(operation.c_str(), "TensorIOTest") == 0) {
    Rcpp::Rcout <<" test function for tensor I/O " << std::endl;
    WriteTensorImage<TensorImageType>(timage, outname.c_str() ,false);
    return 0;
  }
  Rcpp::Rcout << " imagedir " << timage->GetDirection() << std::endl;

  if (strcmp(operation.c_str(), "TensorColor") == 0)
    {
      cimage = ColorImageType::New();
      cimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      cimage->SetBufferedRegion( timage->GetLargestPossibleRegion() );
      cimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      cimage->Allocate();
      cimage->SetSpacing(timage->GetSpacing());
      cimage->SetOrigin(timage->GetOrigin());
      cimage->SetDirection(timage->GetDirection());
    }
  else if (strcmp(operation.c_str(), "TensorToVector") == 0)
    {
      vecimage = VectorImageType::New();
      vecimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      vecimage->SetBufferedRegion( timage->GetLargestPossibleRegion() );
      vecimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      vecimage->Allocate();
      vecimage->SetSpacing(timage->GetSpacing());
      vecimage->SetOrigin(timage->GetOrigin());
      vecimage->SetDirection(timage->GetDirection());
      VectorType zero;  zero.Fill(0);
      vecimage->FillBuffer(zero);
    }
  else if ( (strcmp(operation.c_str(), "TensorToPhysicalSpace") == 0) || 
            (strcmp(operation.c_str(), "TensorToLocalSpace") == 0) )
    {
    toimage = TensorImageType::New();
    toimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
    toimage->SetBufferedRegion( timage->GetLargestPossibleRegion() );
    toimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
    toimage->Allocate();
    toimage->SetSpacing(timage->GetSpacing());
    toimage->SetOrigin(timage->GetOrigin());
    toimage->SetDirection(timage->GetDirection());
    typename TensorImageType::PixelType zero;  
    zero.Fill(0);
    toimage->FillBuffer(zero);
    }
  else
    {
      vimage=ImageType::New();
      vimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      vimage->SetBufferedRegion( timage->GetLargestPossibleRegion() );
      vimage->SetLargestPossibleRegion( timage->GetLargestPossibleRegion() );
      vimage->Allocate();
      vimage->SetSpacing(timage->GetSpacing());
      vimage->SetOrigin(timage->GetOrigin());
      vimage->SetDirection(timage->GetDirection());
    }

  Iterator tIter(timage, timage->GetLargestPossibleRegion() );
  for(  tIter.GoToBegin(); !tIter.IsAtEnd(); ++tIter )
    {

    IndexType ind=tIter.GetIndex();
    float result=0;

    if (strcmp(operation.c_str(),"TensorFA") == 0)
	  {
    	result=GetTensorFA<TensorType>(tIter.Value());
      if (vnl_math_isnan(result)) result=0;
      vimage->SetPixel(ind,result);
  	}
    else if (strcmp(operation.c_str(),"TensorMeanDiffusion") == 0)
	  {
	    result=GetTensorADC<TensorType>(tIter.Value(),0);
      if (vnl_math_isnan(result)) result=0;
      vimage->SetPixel(ind,result);
	  }
    else if (strcmp(operation.c_str(),"TensorFANumerator") == 0)
	  {
	    result=GetTensorFANumerator<TensorType>(tIter.Value());
      if (vnl_math_isnan(result)) result=0;
      vimage->SetPixel(ind,result);
	  }
    else if (strcmp(operation.c_str(),"TensorFADenominator") == 0)
	  {
	    result=GetTensorFADenominator<TensorType>(tIter.Value());
      if (vnl_math_isnan(result)) result=0;
      vimage->SetPixel(ind,result);
	  }
    else if (strcmp(operation.c_str(),"TensorColor") == 0)
	  {
	    RGBType rgb = GetTensorRGB<TensorType>(tIter.Value());
	    cimage->SetPixel(ind,rgb);
	  }
    else if (strcmp(operation.c_str(),"TensorToVector") == 0)
	  {
	    VectorType vv = GetTensorPrincipalEigenvector<TensorType>(tIter.Value(),whichvec);
	    vecimage->SetPixel(ind,vv);
	  }
    else if (strcmp(operation.c_str(),"TensorToVectorComponent") == 0)
	  {
	    if ( whichvec <= 2 ){
	      VectorType vv = GetTensorPrincipalEigenvector<TensorType>(tIter.Value(),2);
	      vimage->SetPixel(ind,vv[whichvec]);
	    }
	    else if ( whichvec > 2 && whichvec < 9){
	      vimage->SetPixel(ind,tIter.Value()[whichvec]);
	    }
	  }
    else if (strcmp(operation.c_str(),"TensorToPhysicalSpace") == 0)
      {
      typename TensorType::EigenValuesArrayType eigenValues;
      typename TensorType::EigenVectorsMatrixType eigenVectors;
      typename TensorType::EigenVectorsMatrixType eigenVectorsPhysical;
      typename TensorType::EigenVectorsMatrixType eigenValuesMatrix;
      eigenValuesMatrix.Fill( 0.0 );
      tIter.Value().ComputeEigenAnalysis( eigenValues, eigenVectors );

      for (unsigned int i=0; i<3; i++)
        {
        eigenValuesMatrix(i,i) = fabs( eigenValues[i] );

        itk::Vector<float, 3> ev;
        for (unsigned int j=0; j<3; j++)
          {
          ev[j] = eigenVectors(j,i);
          }

        itk::Vector<float, 3> evp;
        timage->TransformLocalVectorToPhysicalVector( ev, evp );

        itk::Vector<float, 3> evl;
        timage->TransformPhysicalVectorToLocalVector( evp, evl );

        for (unsigned int j=0; j<3; j++)
          {
          eigenVectorsPhysical(j,i) = evp[j];
          }

        }

      typename TensorType::MatrixType::InternalMatrixType phyTensor 
        = eigenVectorsPhysical.GetTranspose() * eigenValuesMatrix.GetVnlMatrix() * eigenVectorsPhysical.GetVnlMatrix();    
      
      TensorType oTensor = Matrix2Vector<TensorType,TensorType::MatrixType::InternalMatrixType>( phyTensor );
      toimage->SetPixel( tIter.GetIndex(), oTensor );

      }
    else if (strcmp(operation.c_str(),"TensorToLocalSpace") == 0)
      {
      typename TensorType::EigenValuesArrayType eigenValues;
      typename TensorType::EigenVectorsMatrixType eigenVectors;
      typename TensorType::EigenVectorsMatrixType eigenValuesMatrix;
      eigenValuesMatrix.Fill( 0.0 );
      tIter.Value().ComputeEigenAnalysis( eigenValues, eigenVectors );

      for (unsigned int i=0; i<3; i++)
        {
        eigenValuesMatrix(i,i) = fabs( eigenValues[i] );

        itk::Vector<float, 3> ev;
        for (unsigned int j=0; j<3; j++)
          {
          ev[j] = eigenVectors(j,i);
          }

        itk::Vector<float, 3> evp;
        timage->TransformPhysicalVectorToLocalVector( ev, evp );

        for (unsigned int j=0; j<3; j++)
          {
          eigenVectors(j,i) = evp[j];
          }

        }

      typename TensorType::MatrixType::InternalMatrixType lclTensor 
        = eigenVectors.GetTranspose() * eigenValuesMatrix.GetVnlMatrix() * eigenVectors.GetVnlMatrix();
      
      TensorType oTensor = Matrix2Vector<TensorType,TensorType::MatrixType::InternalMatrixType>( lclTensor );
      toimage->SetPixel( tIter.GetIndex(), oTensor );
      }


    }

    if (strcmp(operation.c_str(), "TensorColor") == 0)
    {
      typename ColorWriterType::Pointer cwrite = ColorWriterType::New();
      cwrite->SetInput(cimage);
      cwrite->SetFileName(outname.c_str());
      cwrite->Update();
    }
    else  if (strcmp(operation.c_str(), "TensorToVector") == 0)
    {
      WriteImage<VectorImageType>(vecimage,outname.c_str());
    }
    else if ( (strcmp(operation.c_str(), "TensorToPhysicalSpace") == 0) || 
              (strcmp(operation.c_str(), "TensorToLocalSpace") == 0 ) )
      {
      WriteTensorImage<TensorImageType>(toimage, outname.c_str(), false );
      }
    else
    {
    Rcpp::Rcout << "Writing scalar image" << std::endl;
    WriteImage<ImageType>(vimage,outname.c_str());
    }

  return 0;

}







template<unsigned int ImageDimension>
int CompareHeadersAndImages(int argc, char *argv[])
{

  typedef float  PixelType;
  //  const unsigned int ImageDimension = AvantsImageDimension;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2="";
  if (argc > argct) { fn2=std::string(argv[argct]);   }


  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;

  typename readertype::Pointer reader2 = readertype::New();
  typename readertype::Pointer reader1 = readertype::New();
  reader2->SetFileName(fn2.c_str());

  bool isfloat=false;
  try
    {
      reader2->UpdateLargestPossibleRegion();
    }
  catch(...)
    {
      Rcpp::Rcout << " Error reading " << fn2 << std::endl;
      isfloat=true;
    }

  float floatval=1.0;
  if (isfloat) floatval=atof(argv[argct]);
  else  image2 = reader2->GetOutput();

  reader1->SetFileName(fn1.c_str());
  try
    {
      reader1->UpdateLargestPossibleRegion();
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  // compute error in spacing, in orientation and in offset
  unsigned int failure=0;
  float sperr=0,merr=0,operr=0,orsignerr=0;

  typename ImageType::SpacingType sp1,sp2;
  sp1=image1->GetSpacing();
  sp2=image2->GetSpacing();
  for (unsigned int i=0;i<ImageDimension;i++)
    {
      float temp=sp1[i]-sp2[i];
      sperr+=temp*temp;
    }
  Rcpp::Rcout << " SpacingError: " << sqrt(sperr) << std::endl;

  typename ImageType::PointType op1,op2;
  op1=image1->GetOrigin();
  op2=image2->GetOrigin();
  for (unsigned int i=0;i<ImageDimension;i++)
    {
      float temp=op1[i]-op2[i];
      operr+=temp*temp;
      if ( op1[i] > 0 && op2[i] <= 0) orsignerr+=1;
      else if ( op1[i] < 0 && op2[i] >= 0) orsignerr+=1;
    }
  Rcpp::Rcout << " OriginError: " << sqrt(operr) << std::endl;
  Rcpp::Rcout << " OriginSignError: " << orsignerr << std::endl;

  for (unsigned int i=0;i<ImageDimension;i++)
  for (unsigned int j=0;j<ImageDimension;j++)
    {
      float temp=image1->GetDirection()[i][j]-image2->GetDirection()[i][j];
      merr+=temp*temp;
    }
  Rcpp::Rcout << " OrientError: " << sqrt(merr) << std::endl;

  bool samesize=true;
  for (unsigned int i=0; i<ImageDimension; i++)
    if ( image1->GetLargestPossibleRegion().GetSize()[i] != image2->GetLargestPossibleRegion().GetSize()[i] )
      samesize=false;

  if (samesize )
    {

  // run a quick registration
      if (false){
    typedef typename itk::ImageMomentsCalculator< ImageType >   ImageCalculatorType;
    typename ImageCalculatorType::Pointer calculator = ImageCalculatorType::New();
    calculator->SetImage(  image1 );

    typename ImageCalculatorType::VectorType fixed_center;
    fixed_center.Fill(0);
    typename ImageCalculatorType::VectorType moving_center;
    moving_center.Fill(0);
    try
      {
      calculator->Compute();
      fixed_center = calculator->GetCenterOfGravity();
      calculator->SetImage(  image2 );
      try
	{
	  calculator->Compute();
	  moving_center = calculator->GetCenterOfGravity();
	}
      catch(...)
	{
	  Rcpp::Rcout << " zero image2 error ";
	  fixed_center.Fill(0);
	}
    }
    catch(...)
      {
	Rcpp::Rcout << " zero image1 error ";
      }

    typedef itk::TranslationTransform< double, ImageDimension> TransformType0;
    typename TransformType0::Pointer             m_Transform0=TransformType0::New();
    typename TransformType0::ParametersType trans=m_Transform0->GetParameters();
    for( unsigned int i=0; i<ImageDimension; i++)
      {
        trans[i] = moving_center[i] - fixed_center[i];
      }
    m_Transform0->SetParameters(trans);
    Rcpp::Rcout <<" trans " << m_Transform0->GetParameters() << std::endl;
  typedef itk::ResampleImageFilter< ImageType, ImageType >	ResampleFilterType;
  typename ResampleFilterType::Pointer resample = ResampleFilterType::New();
  resample->SetTransform( m_Transform0 );
  resample->SetInput( image2 );
  resample->SetSize( image1->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  image1->GetOrigin() );
  resample->SetOutputSpacing( image1->GetSpacing() );
  typename ImageType::IndexType zeroind;  zeroind.Fill(0);
  resample->SetDefaultPixelValue( image1->GetPixel(zeroind) );
  resample->UpdateLargestPossibleRegion();
  typename ImageType::Pointer varimage=resample->GetOutput();
      }
  float i1norm=0,i2norm=0,i1i2norm=0; //i3norm=0,i1i3norm=0;
  unsigned long ct1=1,ct2=1,ct12=1; //ct3=1,ct13=1;
  Iterator vfIter2( image1,  image1->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      IndexType ind=vfIter2.GetIndex();
      float pix2;
      if (isfloat) pix2= floatval;
      else pix2=image2->GetPixel(ind);
      if ( vnl_math_isnan(pix2) || vnl_math_isinf(pix2) ) { pix2=0; image2->SetPixel(ind,0);}
      //      float pix3=varimage->GetPixel(ind);
      float pix1 = image1->GetPixel(ind);
      if (pix1 > 0  )  {  i1norm+=fabs(pix1); ct1++; }
      if (pix2 > 0  )  {  i2norm+=fabs(pix2); ct2++; }
      //if (pix3 > 0  )  {  i3norm+=fabs(pix3); ct3++; }
    }
  float mean1=i1norm/ct1;
  if (mean1 == 0) mean1=1;
  float mean2=i2norm/ct2;
  if (mean2 == 0) mean2=1;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      IndexType ind=vfIter2.GetIndex();
      float pix2;
      if (isfloat) pix2= floatval;
      else pix2=image2->GetPixel(ind)/mean2;
      //      float pix3=vfIter2.Get()/mean2;
      float pix1 = image1->GetPixel(ind)/mean1;

      if ( pix1 > 0 || pix2 > 0)
	{
	  i1i2norm+=fabs(pix1-pix2);
	  ct12++;
	}
      //      if ( pix1 > 0 || pix3 > 0)
      //{
      //  i1i3norm+=fabs(pix1-pix3);
      //  ct13++;
      //}
    }
  float idice0 = 1.0 - 2.0*i1i2norm/ct12 /  (  i1norm/ct1 + i2norm / ct2 );
  //  float idice1 = 1.0 - 2.0*i1i3norm/ct13 /  (  i1norm/ct1 + i3norm / ct3 );
  Rcpp::Rcout << " DiceImageDifference: " << idice0 << " IntensityDifference: " << i1i2norm << std::endl;
  //Rcpp::Rcout << " CenterOfMassTransImageDifference: " << idice1 << " and " << i1i3norm << std::endl;
    }

  typename ImageType::PointType fixedorig=image2->GetOrigin();
  if ( orsignerr > 0 )
    {
      failure=1;
      // now fix the error
      for (unsigned int i=0;i<ImageDimension;i++)
	{
	  if ( (op1[i] > 0 && op2[i] < 0 ) || (op1[i] < 0 && op2[i] > 0) )
	    fixedorig[i]*=(-1.0);
	  else if ( (op1[i] > 0 && op2[i] == 0 ) || (op1[i] < 0 && op2[i] == 0) )
	    fixedorig[i]=image1->GetOrigin()[i];
	}
    }
  image2->SetOrigin(fixedorig);
  //  if ( sqrt(operr) > 100) failure=
  if (sqrt(merr) >= 0.7 && failure==0 ) failure=2;
  if (sqrt(merr) >= 0.7 && failure != 0 ) failure=3;
  if (failure > 1)
    {
      image2->SetDirection(image1->GetDirection());
    }
  // write repaired images
  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( image2 );
  writer->Write();
  Rcpp::Rcout << "  FailureState: " << failure << " for " << fn2  << std::endl;
  return failure;

}




// template<class TImage>
// typename TImage::Pointer
// SegmentKMeans(typename TImage::Pointer image , unsigned int nclasses)
// {
//
//   typedef TImage ImageType;
//   typedef typename TImage::PixelType PixelType;
//   enum { ImageDimension = ImageType::ImageDimension };
//   typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
//
//   typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >   AdaptorType;
//
//   typename AdaptorType::Pointer adaptor = AdaptorType::New();
//
//   adaptor->SetImage( image );
//
//   // Define the Measurement vector type from the AdaptorType
//   typedef typename AdaptorType::MeasurementVectorType  MeasurementVectorType;
//
//   // Create the K-d tree structure
//   typedef itk::Statistics::WeightedCentroidKdTreeGenerator<
//                                                       AdaptorType >
//                                                               TreeGeneratorType;
//
//   typename TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();
//
//   treeGenerator->SetSample( adaptor );
//   treeGenerator->SetBucketSize( 16 );
//   treeGenerator->Update();
//
//   typedef typename TreeGeneratorType::KdTreeType TreeType;
//   typedef itk::Statistics::KdTreeBasedKmeansEstimator<TreeType> EstimatorType;
//
//   typename EstimatorType::Pointer estimator = EstimatorType::New();
//
//   typename EstimatorType::ParametersType initialMeans( nclasses );
//
//   Iterator vfIter2( image,  image->GetLargestPossibleRegion() );
//   double mx =-1.e12, mn=1.e12;
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
//     {
//       double px = vfIter2.Get();
//       if (px > mx) mx=px;
//       else if (px < mn) mn=px;
//     }
//   float range=(mx-mn);
//
//   float bins=1.0/((float)nclasses+1.);
//   for (unsigned int i=0;  i<nclasses; i++)
//     {
//     initialMeans[i]=mn+(0+i*bins)*range;
//     Rcpp::Rcout << " Initial Means " << initialMeans[i] << " ";
//     }
//   Rcpp::Rcout << std::endl;
//   estimator->SetParameters( initialMeans );
//
//   estimator->SetKdTree( treeGenerator->GetOutput() );
//   estimator->SetMaximumIteration( 200 );
//   estimator->SetCentroidPositionChangesThreshold(0.0);
//   estimator->StartOptimization();
//
//   typename EstimatorType::ParametersType estimatedMeans = estimator->GetParameters();
//
//
//   typename ImageType::Pointer varimage=ImageType::New();
//   varimage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   varimage->SetBufferedRegion( image->GetLargestPossibleRegion() );
//   varimage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   varimage->Allocate();
//   varimage->SetSpacing(image->GetSpacing());
//   varimage->SetOrigin(image->GetOrigin());
//   varimage->SetDirection(image->GetDirection());
//
// //  float var=sqrt(range);
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
//     {
//     double px = vfIter2.Get();
//       unsigned int best=0;
//       float mindist=1.e9;
//       for ( unsigned int i = 0 ; i < nclasses ; ++i )
// 	{
// 	float dist=fabs(px-estimatedMeans[i]);
// 	if (dist < mindist) { mindist=dist;  best=i; }
// 	//vec[i]=exp(-1.0*dist*dist/var);
// 	}
//       varimage->SetPixel(vfIter2.GetIndex(),best+1);
// //      vecimage->SetPixel(vfIter2.GetIndex(),vec);
//     }
//
//   return varimage;
//
//
// }

template <class TImage>
typename TImage::Pointer  Morphological( typename TImage::Pointer input,float rad, unsigned int option, float dilateval)
{

  typedef TImage ImageType;
  enum { ImageDimension = TImage::ImageDimension };
  typedef typename TImage::PixelType PixelType;

  if (option == 0) Rcpp::Rcout << " binary eroding the image " << std::endl;
  else if (option == 1) Rcpp::Rcout << " binary dilating the image " << std::endl;
  else if (option == 2) Rcpp::Rcout << " binary opening the image " << std::endl;
  else if (option == 3) Rcpp::Rcout << " binary closing the image " << std::endl;
  else if (option == 4) Rcpp::Rcout << " grayscale eroding the image " << std::endl;
  else if (option == 5) Rcpp::Rcout << " grayscale dilating the image " << std::endl;
  else if (option == 6) Rcpp::Rcout << " grayscale opening the image " << std::endl;
  else if (option == 7) Rcpp::Rcout << " grayscale closing the image " << std::endl;

  typedef itk::BinaryBallStructuringElement<
                      PixelType,
                      ImageDimension  >             StructuringElementType;

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryErodeImageFilter<
                            TImage,
                            TImage,
                            StructuringElementType >  ErodeFilterType;

  typedef itk::BinaryDilateImageFilter<
                            TImage,
                            TImage,
                            StructuringElementType >  DilateFilterType;

  //typedef itk::BinaryMorphologicalOpeningImageFilter<
  //                          TImage,
  //                          TImage,
  //                          StructuringElementType >  OpeningFilterType;

  //typedef itk::BinaryMorphologicalClosingImageFilter<
  //                          TImage,
  //                          TImage,
  //                          StructuringElementType >  ClosingFilterType;

  typedef itk::GrayscaleErodeImageFilter<
                            TImage,
                            TImage,
                            StructuringElementType > GrayscaleErodeFilterType;

  typedef itk::GrayscaleDilateImageFilter<
                            TImage,
                            TImage,
                            StructuringElementType > GrayscaleDilateFilterType;

  typename ErodeFilterType::Pointer  binaryErode  = ErodeFilterType::New();
  typename DilateFilterType::Pointer binaryDilate = DilateFilterType::New();
  //typename OpeningFilterType::Pointer  binaryOpen  = OpeningFilterType::New();
  //typename ClosingFilterType::Pointer binaryClose = ClosingFilterType::New();
  typename GrayscaleErodeFilterType::Pointer grayscaleErode = GrayscaleErodeFilterType::New();
  typename GrayscaleDilateFilterType::Pointer grayscaleDilate = GrayscaleDilateFilterType::New();

  StructuringElementType  structuringElement;

  structuringElement.SetRadius((unsigned long) rad );  // 3x3x3 structuring element

  structuringElement.CreateStructuringElement();

  binaryErode->SetKernel(  structuringElement );
  binaryDilate->SetKernel( structuringElement );
  //binaryOpen->SetKernal( structuringElement );
  //binaryClose->SetKernel( structuringElement );
  grayscaleErode->SetKernel( structuringElement );
  grayscaleDilate->SetKernel( structuringElement );

  //  It is necessary to define what could be considered objects on the binary
  //  images. This is specified with the methods \code{SetErodeValue()} and
  //  \code{SetDilateValue()}. The value passed to these methods will be
  //  considered the value over which the dilation and erosion rules will apply
  binaryErode->SetErodeValue( (unsigned int ) dilateval );
  binaryDilate->SetDilateValue(  (unsigned int ) dilateval );

  typename TImage::Pointer temp;
  if (option == 1)
    {
      Rcpp::Rcout << " Dilate " << rad << std::endl;
      binaryDilate->SetInput( input );
      binaryDilate->Update();
      temp = binaryDilate->GetOutput();
    }
  else if (option == 0)
    {
      Rcpp::Rcout << " Erode " << rad << std::endl;
      binaryErode->SetInput( input );//binaryDilate->GetOutput() );
      binaryErode->Update();
      temp = binaryErode->GetOutput();
    }
   else if (option == 2)
    {
      // dilate(erode(img))
      Rcpp::Rcout << " Binary Open " << rad << std::endl;
      //binaryOpen->SetInput( input );//binaryDilate->GetOutput() );
      //binaryOpen->Update();
      binaryErode->SetInput( input );
      binaryDilate->SetInput( binaryErode->GetOutput() );
      binaryDilate->Update();
      temp = binaryDilate->GetOutput();
    }
  else if (option == 3)
    {
      Rcpp::Rcout << " Binary Close " << rad << std::endl;
      //binaryClose->SetInput( input );//binaryDilate->GetOutput() );
      //binaryClose->Update();
      binaryDilate->SetInput( input );
      binaryErode->SetInput( binaryDilate->GetOutput() );
      binaryErode->Update();
      temp = binaryErode->GetOutput();
    }
  else if (option == 4)
    {
    Rcpp::Rcout << " Grayscale Erode " << rad << std::endl;
    grayscaleErode->SetInput( input );//binaryDilate->GetOutput() );
    grayscaleErode->Update();
    temp = binaryErode->GetOutput();
    }
  else if (option == 5)
    {
    Rcpp::Rcout << " Grayscale Dilate " << rad << std::endl;
    grayscaleDilate->SetInput( input );//binaryDilate->GetOutput() );
    grayscaleDilate->Update();
    temp = binaryDilate->GetOutput();
    }
  else if (option == 6)
    {
    Rcpp::Rcout << " Grayscale Open " << rad << std::endl;
    grayscaleErode->SetInput( input );//binaryDilate->GetOutput() );
    grayscaleErode->Update();
    grayscaleDilate->SetInput( grayscaleErode->GetOutput() );
    grayscaleDilate->Update();
    temp = grayscaleDilate->GetOutput();
    }
  else if (option == 7)
    {
    Rcpp::Rcout << " Grayscale Close " << rad << std::endl;
    grayscaleDilate->SetInput( input );//binaryDilate->GetOutput() );
    grayscaleDilate->Update();
    grayscaleErode->SetInput( grayscaleDilate->GetOutput() );
    grayscaleErode->Update();
    temp = grayscaleErode->GetOutput();
    }

  if (option == 0 )
  {
    // FIXME - replace with threshold filter?
    typedef itk::ImageRegionIteratorWithIndex< ImageType > ImageIteratorType ;
    ImageIteratorType o_iter( temp, temp->GetLargestPossibleRegion() );
    o_iter.GoToBegin() ;
    while ( !o_iter.IsAtEnd() )
    {
      if (o_iter.Get() > 0.5 && input->GetPixel(o_iter.GetIndex()) > 0.5)
	      o_iter.Set(1);
      else o_iter.Set(0);
      ++o_iter;
    }
  }

  return temp;

}




// template<class TImage>
// typename TImage::Pointer
// BayesianSegmentation(typename TImage::Pointer image , unsigned int nclasses,  std::string priorfn ,  unsigned int nsmooth = 2 )
// {
//
//   typedef TImage ImageType;
//   typedef typename TImage::PixelType PixelType;
//   enum { ImageDimension = ImageType::ImageDimension };
//   typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
//
//   //  const unsigned int ImageDimension = AvantsImageDimension;
//   typedef itk::Vector<float,ImageDimension>         VectorType;
//   typedef itk::Image<VectorType,ImageDimension>     FieldType;
//   typedef itk::ImageFileReader<ImageType> readertype;
//   typedef itk::ImageFileWriter<ImageType> writertype;
//   typedef  typename ImageType::IndexType IndexType;
//   typedef  typename ImageType::SizeType SizeType;
//   typedef  typename ImageType::SpacingType SpacingType;
//   typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
//   typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
//   typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
//   typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
//
//
//
//   typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >   AdaptorType;
//   typename AdaptorType::Pointer adaptor = AdaptorType::New();
//   adaptor->SetImage( image );
//   // Define the Measurement vector type from the AdaptorType
//   typedef typename AdaptorType::MeasurementVectorType  MeasurementVectorType;
//   // Create the K-d tree structure
//   typedef itk::Statistics::WeightedCentroidKdTreeGenerator<
//                                                       AdaptorType >
//                                                               TreeGeneratorType;
//   typename TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();
//   treeGenerator->SetSample( adaptor );
//   treeGenerator->SetBucketSize( 16 );
//   treeGenerator->Update();
//   typedef typename TreeGeneratorType::KdTreeType TreeType;
//   typedef itk::Statistics::KdTreeBasedKmeansEstimator<TreeType> EstimatorType;
//   typename EstimatorType::Pointer estimator = EstimatorType::New();
//   typename EstimatorType::ParametersType initialMeans( nclasses );
//   Iterator vfIter2( image,  image->GetLargestPossibleRegion() );
//   double mx =-1.e12, mn=1.e12;
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
//     {
//       double px = vfIter2.Get();
//       if (px > mx) mx=px;
//       else if (px < mn) mn=px;
//     }
//   float range=(mx-mn);
//   float bins=1.0/((float)nclasses+1.);
//   for (unsigned int i=0;  i<nclasses; i++)
//     {
//     initialMeans[i]=mn+(0+i*bins)*range;
//     }
//   estimator->SetParameters( initialMeans );
//   estimator->SetKdTree( treeGenerator->GetOutput() );
//   estimator->SetMaximumIteration( 200 );
//   estimator->SetCentroidPositionChangesThreshold(0.0);
//   estimator->StartOptimization();
//   typename EstimatorType::ParametersType estimatedMeans = estimator->GetParameters();
//
//   typename ImageType::Pointer varimage=ImageType::New();
//   varimage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   varimage->SetBufferedRegion( image->GetLargestPossibleRegion() );
//   varimage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   varimage->SetSpacing(image->GetSpacing());
//   varimage->SetOrigin(image->GetOrigin());
//   varimage->SetDirection(image->GetDirection());
//   varimage->Allocate();
//
//   std::vector<double>  estimatedVar(nclasses,0);
//   std::vector<double>  estimatedCounts(nclasses,0);
//
// //  float var=sqrt(range);
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
//     {
//     double px = vfIter2.Get();
//       unsigned int best=0;
//       float mindist=1.e9;
//       for ( unsigned int i = 0 ; i < nclasses ; ++i )
// 	{
// 	float dist=fabs(px-estimatedMeans[i]);
// 	if (dist < mindist) { mindist=dist;  best=i; }
// 	//vec[i]=exp(-1.0*dist*dist/var);
// 	}
//       varimage->SetPixel(vfIter2.GetIndex(),best);
//     }
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
//     {
//     double px = vfIter2.Get();
//     unsigned int i = (unsigned int) varimage->GetPixel(vfIter2.GetIndex());
//     estimatedCounts[i]=estimatedCounts[i]+1;
//     float dist=(px -estimatedMeans[i]);
//     estimatedVar[i]+=dist*dist;
//     }
//
//   for (unsigned int i=0; i<nclasses; i++)
//     {
//     float   ct= estimatedCounts[i];
//     if (ct > 0) estimatedVar[i]=(estimatedVar[i])/ct;
//     else estimatedVar[i]=0;
//     Rcpp::Rcout << " Sample SD Ests " << sqrt(estimatedVar[i]) << " Mean " << estimatedMeans[i] <<  std::endl;
//     }
//
//   typedef float InputPixelType;
//   typedef itk::VectorImage< InputPixelType, ImageDimension > InputImageType;
//   typedef  float  LabelType;
//   typedef float          PriorType;
//   typedef float          PosteriorType;
//   typedef itk::BayesianClassifierImageFilter<
//                               InputImageType,LabelType,
//                               PosteriorType,PriorType >   ClassifierFilterType;
//
//   typename InputImageType::Pointer vecImage = InputImageType::New();
//   typedef typename InputImageType::PixelType VecPixelType;
//   vecImage->SetSpacing(image->GetSpacing());
//   vecImage->SetOrigin(image->GetOrigin());
//   vecImage->SetRegions( image->GetLargestPossibleRegion() );
//   vecImage->SetVectorLength(nclasses);
//   vecImage->Allocate();
//   VecPixelType vvv(nclasses);
//   vvv.Fill(0);
//   vecImage->FillBuffer(vvv);
//
//   for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )  {
//      double px = vfIter2.Get();
//      VecPixelType  probs(nclasses);
//      float total=0;
//      for (unsigned int i=0; i<nclasses; i++)
//        {
//        probs[i]=exp(-1.0*(estimatedMeans[i]-px)*(estimatedMeans[i]-px)/(2.0*estimatedVar[i]));
//        total+=probs[i];
//        }
//      for (unsigned int i=0; i<nclasses; i++)
//        {
//        if (total>0) probs[i]/=total;
//        }
//      vecImage->SetPixel( vfIter2.GetIndex(), probs);
//   }
//
//   typename ClassifierFilterType::Pointer filter = ClassifierFilterType::New();
//
//
//   typedef itk::ImageFileReader< InputImageType >  ReaderType;
//   typename ReaderType::Pointer reader =  ReaderType::New();
//   typename InputImageType::Pointer priors=NULL;
//
//   if (priorfn.length()  > 3 )
//     {
//     Rcpp::Rcout << " Setting Priors " << priorfn << std::endl;
//     bool geometric=false;
//     if ( strcmp(priorfn.c_str(),"Geometric") == 0) geometric=true;
//     if (geometric)
//       {
//       Rcpp::Rcout <<" Using a geometric thickness prior to aid cortical segmentation " << std::endl;
//       typename ImageType::Pointer outbrainmask = BinaryThreshold<TImage>(0,nclasses-3,1,varimage);
//       typename ImageType::Pointer inwmask = BinaryThreshold<TImage>(nclasses-1,nclasses,1,varimage);
//       typename ImageType::Pointer outwmask = BinaryThreshold<TImage>(0,nclasses-2,1,varimage);
//       typedef itk::DanielssonDistanceMapImageFilter<ImageType, ImageType >  FilterType;
//       typename  FilterType::Pointer distmap = FilterType::New();
//       distmap->InputIsBinaryOn();
//       distmap->SetUseImageSpacing(true);
//       distmap->SetInput(outbrainmask);
//       distmap->Update();
//       typename ImageType::Pointer distcortex=distmap->GetOutput();
//
//       typename  FilterType::Pointer distmap2 = FilterType::New();
//       distmap2->InputIsBinaryOn();
//       distmap2->SetUseImageSpacing(true);
//       distmap2->SetInput(inwmask);
//       distmap2->Update();
//       typename ImageType::Pointer distwm=distmap2->GetOutput();
//
//       typedef itk::LaplacianRecursiveGaussianImageFilter<ImageType,ImageType >  dgf;
//       typename dgf::Pointer lfilter = dgf::New();
//       lfilter->SetSigma(1.3);
//       lfilter->SetInput(distwm);
//       lfilter->Update();
//       typename ImageType::Pointer image2=lfilter->GetOutput();
//       typedef itk::RescaleIntensityImageFilter<ImageType,ImageType > RescaleFilterType;
//       typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
//       rescaler->SetOutputMinimum(   0 );
//       rescaler->SetOutputMaximum( 1 );
//       rescaler->SetInput( image2 );
//       rescaler->Update();
//       typename ImageType::Pointer sulci=  rescaler->GetOutput();
//
//       priors= InputImageType::New();
//       typedef typename InputImageType::PixelType VecPixelType;
//       priors->SetSpacing(image->GetSpacing());
//       priors->SetOrigin(image->GetOrigin());
//       priors->SetDirection(image->GetDirection());
//       priors->SetRegions( image->GetLargestPossibleRegion() );
//       priors->SetVectorLength(nclasses);
//       priors->Allocate();
//       Rcpp::Rcout <<" Allocated " << std::endl;
//
//       for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
// 	{
// //	Rcpp::Rcout <<" ind " <<vfIter2.GetIndex() << std::endl;
// //	float outbrain = outbrainmask->GetPixel( vfIter2.GetIndex());
// //	float inw = inwmask->GetPixel( vfIter2.GetIndex());
// 	float distance = distcortex->GetPixel( vfIter2.GetIndex());
// 	float wdistance = distwm->GetPixel( vfIter2.GetIndex());
// 	VecPixelType  probs(nclasses);
// 	probs.Fill(1.0/(float)nclasses);
// 	VecPixelType  posteriors=vecImage->GetPixel(vfIter2.GetIndex());
// 	if (nclasses > 2)
// 	{
// 	float dvar=2;
// 	float basedist=4;
// 	float distmag=basedist-distance;
// //	if (distmag > 0) distmag=0;
// 	distmag*=distmag;
// 	float gdistprob=1.0-exp(-1.0*distmag/dvar);
//
// 	float wdistprob=1.0/(1.0+exp(-1.0*distance/5));
//
// 	float wdistmag=basedist-wdistance;
// 	if (wdistmag > 0) wdistmag=0;
// 	wdistmag*=wdistmag;
// 	float gdistprob2=exp(-1.0*wdistmag/dvar);
//
// 	float sulcprob=sulci->GetPixel( vfIter2.GetIndex());
// 	float sdiff=(0.25-sulcprob);
// 	if (sdiff > 0) sdiff=0;
// 	sdiff*=sdiff;
// 	sulcprob=exp(-1.0*sdiff/0.5);
// //	Rcpp::Rcout << " Sulc " << sulcprob << std::endl;
// //	bool test = (outbrain < 1 && inw > 1);
// 	if (  true  )
// 	  {
// 	  for (unsigned int i=0; i<nclasses; i++)
// 	    {
// 	    if ( i == (nclasses-2)) probs[i]=gdistprob*(gdistprob2);
// 	    else if ( i == (nclasses-1)) probs[i]=wdistprob*wdistprob;
// 	    //else
// 	      if ( i == (nclasses-3)) probs[i]=sulcprob;
// //	    else if (i > 0)  probs[i]=sulcprob;
// 	    }
// 	  }
// 	else
// 	  {
// 	  for (unsigned int i=0; i<nclasses; i++) probs[i]=posteriors[i];//1.0/(float)nclasses;
// 	  }
// 	for (unsigned int i=0; i<nclasses; i++) posteriors[i]*=probs[i];
// 	}
//       float prtotal=0;
//       float pototal=0;
//       for (unsigned int i=0; i<nclasses; i++) {  prtotal+=probs[i];  pototal+=posteriors[i]; }
//
//       for (unsigned int i=0; i<nclasses; i++)
// 	{
// 	if (prtotal>0) probs[i]/=prtotal; else probs[i]=1.0/(float)nclasses;
// 	if (pototal>0) posteriors[i]/=pototal; else posteriors[i]=1.0/(float)nclasses;
// 	}
//       priors->SetPixel( vfIter2.GetIndex(), probs);
//       vecImage->SetPixel( vfIter2.GetIndex(), posteriors);
//       }
//       Rcpp::Rcout << " ok " << std::endl;
//
//
//       }
//     else
//       {
//       reader->SetFileName( priorfn.c_str() );
//       reader->Update();
//       priors=reader->GetOutput();
//
//       for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
// 	{
// 	VecPixelType  posteriors=vecImage->GetPixel(vfIter2.GetIndex());
// 	VecPixelType  probs=priors->GetPixel(vfIter2.GetIndex());
// 	for (unsigned int i=0; i<nclasses; i++) posteriors[i]*=probs[i];
//
// 	float prtotal=0;
// 	float pototal=0;
// 	for (unsigned int i=0; i<nclasses; i++) {  prtotal+=probs[i];  pototal+=posteriors[i]; }
//
// 	for (unsigned int i=0; i<nclasses; i++)
// 	  {
// 	  if (prtotal>0) probs[i]/=prtotal; else probs[i]=1.0/(float)nclasses;
// 	  if (pototal>0) posteriors[i]/=pototal; else posteriors[i]=1.0/(float)nclasses;
// 	  }
// 	vecImage->SetPixel( vfIter2.GetIndex(), posteriors);
// 	}
//       }
// //    if (priors) filter->SetInput( 1,  priors ); // Bug --
// //    classification filter does not actually use priors
//     } else Rcpp::Rcout << " No Priors " << std::endl;
//
//   filter->SetInput(  vecImage );
//
//
//   if( nsmooth >= 1  )
//     {
//     Rcpp::Rcout << " Smoothing Iterations:  " << nsmooth << std::endl;
//     filter->SetNumberOfSmoothingIterations( nsmooth );
//     typedef typename ClassifierFilterType::ExtractedComponentImageType ExtractedComponentImageType;
//     typedef itk::DiscreteGaussianImageFilter<
//       ExtractedComponentImageType, ExtractedComponentImageType >  SmoothingFilterType;
//     typedef itk::BilateralImageFilter<
//       ExtractedComponentImageType, ExtractedComponentImageType >  SmoothingFilterType2;
//     typename SmoothingFilterType::Pointer smoother = SmoothingFilterType::New();
//     smoother->SetVariance(1.0);
//     smoother->SetUseImageSpacingOff();
//     //smoother->SetDomainSigma(1);
//     //smoother->SetRangeSigma(1);
//     filter->SetSmoothingFilter( smoother );
//     }
//
//   // SET FILTER'S PRIOR PARAMETERS
//   // do nothing here to default to uniform priors
//   // otherwise set the priors to some user provided values
//
//   //
//   // Setup writer.. Rescale the label map to the dynamic range of the
//   // datatype and write it
//   //
//   filter->Update();
//
// return  filter->GetOutput();
//
// }


template<unsigned int ImageDimension>
int NegativeImage(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2="";
  if (argc > argct) { fn2=std::string(argv[argct]);  }

  typename ImageType::Pointer image1 = NULL;
  typename readertype::Pointer reader1 = readertype::New();
  reader1->SetFileName(fn1.c_str());
  reader1->UpdateLargestPossibleRegion();
  try
    {
      image1 = reader1->GetOutput();
    }
  catch(...)
    {
      Rcpp::Rcout << " read 1 error ";
    }

  Iterator vfIter2( image1,  image1->GetLargestPossibleRegion() );
  double mx =-1.e12, mn=1.e12;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      double px = vfIter2.Get();
      if (px > mx) mx=px;
      else if (px < mn) mn=px;
    }
  if (mx == mn) { mx=1; mn=0; }

  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      IndexType ind=vfIter2.GetIndex();
      double pix = image1->GetPixel(ind);
      pix = (pix - mn)/(mx-mn);
      pix = (1.0 - pix)*(mx-mn);

      vfIter2.Set(pix);
    }

  typename writertype::Pointer writer = writertype::New();
    writer->SetFileName(outname.c_str());
    writer->SetInput( image1 );
    writer->Write();


  return 0;

}





// template<class TImage>
// typename TImage::Pointer
// //void
// SegmentMRF(typename TImage::Pointer image ,
// 	   typename TImage::Pointer labelimage, unsigned int nclasses, float smf, unsigned int maxits)
// {
//
//   typedef TImage ImageType;
//   typedef typename TImage::PixelType PixelType;
//   enum { ImageDimension = ImageType::ImageDimension };
//   enum { Dimension = ImageType::ImageDimension };
//
//
//   //  const unsigned int NUMBANDS=1;
//   typedef itk::Image<itk::Vector<double,1>,ImageDimension> VecImageType;
//   typedef typename VecImageType::PixelType  VecPixelType;
//
//   /** copy the input image into this vector image.  stupid.  */
//
//   typename VecImageType::Pointer vecImage = VecImageType::New();
//   vecImage->SetSpacing(image->GetSpacing());
//   vecImage->SetOrigin(image->GetOrigin());
//   vecImage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   vecImage->SetBufferedRegion(  image->GetLargestPossibleRegion() );
//   vecImage->Allocate();
//   VecPixelType vvv;
//   vvv.Fill(0);
//   vecImage->FillBuffer(vvv);
//
//   // setup the iterators
//   //  typedef VecImageType::PixelType::VectorType VecPixelType;
//
//   enum { VecImageDimension = VecImageType::ImageDimension };
//   typedef itk::ImageRegionIterator< VecImageType > VecIterator;
//
//   VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
//   for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
//     {
//       vvv[0]=image->GetPixel(outIt.GetIndex());
//       outIt.Set(vvv);
//     }
//
//
//   namespace stat = itk::Statistics;
//   typedef itk::Image<PixelType,ImageDimension> ClassImageType;
//   typedef stat::MahalanobisDistanceMembershipFunction< VecPixelType >
//     MembershipFunctionType ;
//   typedef typename MembershipFunctionType::Pointer MembershipFunctionPointer ;
//   typedef std::vector< MembershipFunctionPointer >   MembershipFunctionPointerVector;
//
//   typedef itk::ImageGaussianModelEstimator<VecImageType,
//     MembershipFunctionType, ClassImageType>
//     ImageGaussianModelEstimatorType;
//
//   typename ImageGaussianModelEstimatorType::Pointer  applyEstimateModel =
//     ImageGaussianModelEstimatorType::New();
//   /*
//   typedef itk::Statistics::WeightedCentroidKdTreeGenerator<
//                                                       AdaptorType >
//                                                               TreeGeneratorType;
//   typedef typename TreeGeneratorType::KdTreeType TreeType;
//   typedef itk::Statistics::KdTreeBasedKmeansEstimator<TreeType> EstimatorType;
//
//   */
//   applyEstimateModel->SetNumberOfModels(nclasses);
//   applyEstimateModel->SetInputImage(vecImage);
//   applyEstimateModel->SetTrainingImage(labelimage);
//
//   //Run the gaussian classifier algorithm
//   applyEstimateModel->Update();
//   applyEstimateModel->Print(Rcpp::Rcout);
//
//   MembershipFunctionPointerVector membershipFunctions =
//     applyEstimateModel->GetMembershipFunctions();
//
//   //----------------------------------------------------------------------
//   //Set the decision rule
//   //----------------------------------------------------------------------
//   typedef typename itk::DecisionRuleBase::Pointer DecisionRuleBasePointer;
//
//   typedef itk::MinimumDecisionRule DecisionRuleType;
//   typename DecisionRuleType::Pointer
//     myDecisionRule = DecisionRuleType::New();
//
//   //----------------------------------------------------------------------
//   // Set the classifier to be used and assigne the parameters for the
//   // supervised classifier algorithm except the input image which is
//   // grabbed from the MRF application pipeline.
//   //----------------------------------------------------------------------
//   //---------------------------------------------------------------------
//   typedef PixelType MeasurementVectorType;
//
//   typedef itk::ImageClassifierBase< VecImageType,
//     ClassImageType > ClassifierType;
//
//   typedef typename itk::ClassifierBase<VecImageType>::Pointer
//     ClassifierBasePointer;
//
//   typedef typename ClassifierType::Pointer ClassifierPointer;
//   ClassifierPointer myClassifier = ClassifierType::New();
//   // Set the Classifier parameters
//   myClassifier->SetNumberOfClasses(nclasses);
//
//   // Set the decison rule
//   myClassifier->
//     SetDecisionRule((DecisionRuleBasePointer) myDecisionRule );
//
//   //Add the membership functions
//   double meanDistance=0;
//   for( unsigned int i=0; i<nclasses; i++ )
//     {
//     myClassifier->AddMembershipFunction( membershipFunctions[i] );
//     meanDistance+=membershipFunctions[i]->GetMean()[0];
//     }
//   meanDistance/=(float)nclasses;
//   Rcpp::Rcout << " mean dist " << meanDistance << std::endl;
//
//
//   //----------------------------------------------------------------------
//   // Set the MRF labeller and populate the parameters
//   //----------------------------------------------------------------------
//
//   //Set the MRF labeller
//   typedef itk::MRFImageFilter<VecImageType,ClassImageType> MRFImageFilterType;
//   typename MRFImageFilterType::Pointer applyMRFImageFilter = MRFImageFilterType::New();
//
//   // Set the MRF labeller parameters
//   applyMRFImageFilter->SetNumberOfClasses( nclasses );
//   applyMRFImageFilter->SetMaximumNumberOfIterations( maxits );
//   applyMRFImageFilter->SetErrorTolerance( 1.e-4 );
//   applyMRFImageFilter->SetSmoothingFactor( smf );
//   applyMRFImageFilter->SetInput(vecImage);
//   applyMRFImageFilter->SetClassifier( myClassifier );
//
//   //For setting up a square/cubic or hypercubic neighborhood
//   applyMRFImageFilter->SetNeighborhoodRadius( 1 );
//   std::vector<double> weights =
//     applyMRFImageFilter->GetMRFNeighborhoodWeight();
//   std::vector<double> testNewNeighborhoodWeight( weights.size(), 1);
//   double totalWeight = 0;
//   for(std::vector< double >::const_iterator wcIt = weights.begin();
//       wcIt != weights.end(); ++wcIt )
//     {
//     totalWeight += *wcIt;
//     }
//   unsigned int jj = 0;
//   for(std::vector< double >::iterator wIt = weights.begin();
//       wIt != weights.end(); wIt++ )
//     {
//      testNewNeighborhoodWeight[jj] = static_cast< double > ( (*wIt) * meanDistance / (2 * totalWeight));
//      //   Rcpp::Rcout << " ow " << weights[jj] << " nw " <<  testNewNeighborhoodWeight[jj] << std::endl;
//     jj++;
//     }
//
// //  applyMRFImageFilter->SetMRFNeighborhoodWeight( testNewNeighborhoodWeight );
//   //  applyMRFImageFilter->SetMRFNeighborhoodWeight( weights );
//
//   //Kick off the MRF labeller function
//   applyMRFImageFilter->Update();
//
//   applyMRFImageFilter->Print(Rcpp::Rcout);
//   Rcpp::Rcout << "Number of Iterations : " << applyMRFImageFilter->GetNumberOfIterations()
//     << std::endl;
//   Rcpp::Rcout << "Stop condition: (1) Maximum number of iterations (2) Error tolerance:  "
//     << applyMRFImageFilter->GetStopCondition() << std::endl;
//
//   typename ClassImageType::Pointer  outClassImage = applyMRFImageFilter->GetOutput();
//
//   //Testing of different parameter access functions in the filter
//   Rcpp::Rcout << "The number of classes labelled was: " <<
//     applyMRFImageFilter->GetNumberOfClasses() << std::endl;
//   Rcpp::Rcout << "The maximum number of iterations were: " <<
//     applyMRFImageFilter->GetMaximumNumberOfIterations() << std::endl;
//   Rcpp::Rcout << "The error tolerace threshold was: " <<
//     applyMRFImageFilter->GetErrorTolerance() << std::endl;
//   Rcpp::Rcout << "The smoothing MRF parameter used was: " <<
//     applyMRFImageFilter->GetSmoothingFactor() << std::endl;
//   Rcpp::Rcout << "The MRF neighborhood weights are: " << std::endl;
//
//
//   return  outClassImage;
//
// }



template<class TImage>
typename TImage::Pointer
//void
itkMRIBiasFieldCorrectionFilter(typename TImage::Pointer image ,
								typename TImage::Pointer labelimage, unsigned int sd=2)
{

	Rcpp::Rcout << "doing Bias corr " << std::endl;
	typedef TImage ImageType;
	enum { ImageDimension = ImageType::ImageDimension };
	typedef itk::ImageRegionIteratorWithIndex< ImageType > ImageIteratorType ;

//	bool SaveImages = false;

	//  WriteImage<ImageType>(image,"temp1.nii");
	//  WriteImage<ImageType>(labelimage,"temp2.nii");

	// class statistics for two classes: a bright sphere and background
	// 3 or 1 classes ? SD
	//unsigned int numclasses=1;
	unsigned int numclasses=4;
	itk::Array<double> classMeans(numclasses);
	itk::Array<double> classSigmas(numclasses);
	itk::Array<unsigned long> classCounts(numclasses);
	classMeans.Fill(0.0);
	classSigmas.Fill(0.0);
	classCounts.Fill(0);


	//******** get output mask from image SD
	typename ImageType::Pointer outputmask = BinaryThreshold<TImage>(0.1,1.e9,1,image);
	//******** get output mask from image SD

	//  WriteImage<TImage>(mask,"outMK1.nii");
	// SD play with mask to see if we can move the artifact line
	//  mask=MorphologicalErosion<TImage>(1.5, mask);
	//mask=MorphologicalErosion<TImage>(3.5, mask);
	//WriteImage<TImage>(mask,"outMK2.nii");

	ImageIteratorType o_iter( labelimage,labelimage->GetLargestPossibleRegion() );
	o_iter.GoToBegin() ;
	while ( !o_iter.IsAtEnd() )
    {
		unsigned int label = (unsigned int) o_iter.Get();
		float pix = image->GetPixel(o_iter.GetIndex());
		// uncomment for using one tissue class only for input mask SD
		/*
			if ( mask->GetPixel(o_iter.GetIndex()) == 1 )
		 {
				label=label-1;
				unsigned int ind=0;
				classCounts[ind] +=1;
				float n = classCounts[ind];
				classMeans[ind] = (n-1.0)/( n )*  classMeans[ind] + 1.0 / n * pix;
				double sqrdf =  ( pix -  classMeans[ind])*( pix -  classMeans[ind]);
				if (n > 1)classSigmas[ind]=  (n-1)/n * classSigmas[ind] +  1.0 / (n-1) * sqrdf;
		 }
		 */

		// uncomment for using 3 tissue classes SD
		if ( outputmask->GetPixel(o_iter.GetIndex()) == 1 )
		{
			//label=label-1;
			classCounts[label] +=1;
			float n = classCounts[label];
			classMeans[label] = (n-1.0)/( n )*  classMeans[label] + 1.0 / n * pix;
			double sqrdf =  ( pix -  classMeans[label])*( pix -  classMeans[label]);
			if (n > 1)classSigmas[label]=  (n-1)/n * classSigmas[label] +  1.0 / (n-1) * sqrdf;
		}

		++o_iter ;
    }

	for (unsigned int k=0; k < numclasses; k++)
    {
		classSigmas[k]=sqrt(classSigmas[k]);
		Rcpp::Rcout << " Initial Means pre-bias " << classMeans[k] << " sig " <<  classSigmas[k] << std::endl;
    }

	// creats a normal random variate generator
	//itk::Statistics::NormalVariateGenerator::Pointer randomGenerator =
	//  itk::Statistics::NormalVariateGenerator::New() ;


	// creates a bias correction filter and run it.
	typedef itk::MRIBiasFieldCorrectionFilter<ImageType, ImageType, ImageType>  FilterType;

	Rcpp::Rcout << "before new filter" << std::endl;
	typename FilterType::Pointer filter = FilterType::New() ;
	Rcpp::Rcout << "after new filter" << std::endl;

	//  typename FilterType::BiasFieldType::CoefficientArrayType

	filter->SetInput( image.GetPointer() ) ;
	//filter->SetInput( image ) ;
	filter->IsBiasFieldMultiplicative( true ) ; // correct with multiplicative bias
	unsigned int biasDegree=4;
	filter->SetBiasFieldDegree( biasDegree ) ; // default value = 3
	filter->SetTissueClassStatistics( classMeans, classSigmas ) ;
	//filter->SetOptimizerGrowthFactor( 1.01 ) ; // default value
	//filter->SetOptimizerInitialRadius( 0.02 ) ; // default value

	// SD debug don't do interslice correction
	//filter->SetUsingInterSliceIntensityCorrection( true ) ; // default value
	filter->SetUsingInterSliceIntensityCorrection( false ) ; // default value




	filter->SetVolumeCorrectionMaximumIteration( 200) ; // default value = 100
	filter->SetInterSliceCorrectionMaximumIteration( 100 ) ; // default value = 100
	filter->SetUsingSlabIdentification( false ) ; // default value = false
												  //filter->SetSlabBackgroundMinimumThreshold( 0 ) ; // default value
												  //filter->SetSlabNumberOfSamples( 10 ) ; // default value
												  //filter->SetSlabTolerance(0.0) ; // default value
	filter->SetSlicingDirection(sd) ; // default value
	filter->SetUsingBiasFieldCorrection( true ) ; // default value
	filter->SetGeneratingOutput( true ) ; // default value

	filter->SetInputMask( outputmask ) ;

	//******** Try different output mask SD
	//  filter->SetOutputMask( labelimage ) ;
	filter->SetOutputMask( outputmask ) ;
	//  filter->SetDebug( true );
	//    WriteImage<ImageType>(mask,"mask.nii");
	//    WriteImage<ImageType>(outputmask,"outputmask.nii");
	//    WriteImage<ImageType>(image,"image.nii");
	//    WriteImage<ImageType>(labelimage,"labelimage.nii");
	//******** Try different output mask SD

	// default schedule is 2 2 2 - 1 1 1, let's change this
	bool setnewsched=false;
	if (setnewsched)
    {
		unsigned int nlev=1;
		typename FilterType::ScheduleType schedule ( nlev, ImageDimension) ;
		schedule.Fill( 4 );
		//      for (unsigned int jj=0; jj<ImageDimension; jj++) schedule[0][jj]=4;
		//for (unsigned int jj=0; jj<ImageDimension; jj++) schedule[1][jj]=2;
		//for (unsigned int jj=0; jj<ImageDimension; jj++) schedule[2][jj]=1;
		filter->SetNumberOfLevels( nlev ) ; // Important to set this first, otherwise the filter rejects the new schedule
		filter->SetSchedule( schedule ) ;
    }
	//  filter->SetInitialBiasFieldCoefficients(initCoefficients);
	filter->SetVolumeCorrectionMaximumIteration( 200 ) ; // default value = 100
	filter->SetInterSliceCorrectionMaximumIteration( 100 ) ; // default value = 100
															 //filter->SetOptimizerInitialRadius( 0.02 ) ; // default value
															 //timing
	long int t1 = time(NULL);
	filter->Update() ;
	long int t2 = time(NULL);
	Rcpp::Rcout << "Run time (in s)" << t2-t1  << std::endl ;

	return filter->GetOutput();
}




// template<class TImage>
// typename TImage::Pointer
// //void
// SegmentMRFKM(typename TImage::Pointer image ,
// 	     typename TImage::Pointer labelimage, unsigned int nclasses, float smf, unsigned int maxit)
// {
//
//   typedef TImage ImageType;
//   typedef typename TImage::PixelType PixelType;
//   enum { ImageDimension = ImageType::ImageDimension };
//   enum { Dimension = ImageType::ImageDimension };
//
//
//   //  const unsigned int NUMBANDS=1;
//   typedef itk::Image<itk::Vector<double,1>,ImageDimension> VecImageType;
//   typedef typename VecImageType::PixelType  VecPixelType;
//
//   /** copy the input image into this vector image.  stupid.  */
//
//   typename VecImageType::Pointer vecImage = VecImageType::New();
//   vecImage->SetSpacing(image->GetSpacing());
//   vecImage->SetOrigin(image->GetOrigin());
//   vecImage->SetLargestPossibleRegion( image->GetLargestPossibleRegion() );
//   vecImage->SetBufferedRegion(  image->GetLargestPossibleRegion() );
//   vecImage->Allocate();
//   VecPixelType vvv;
//   vvv.Fill(0);
//   vecImage->FillBuffer(vvv);
//
//   // setup the iterators
//   //  typedef VecImageType::PixelType::VectorType VecPixelType;
//
//   enum { VecImageDimension = VecImageType::ImageDimension };
//   typedef itk::ImageRegionIterator< VecImageType > VecIterator;
//
//   VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
//   for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
//     {
//       vvv[0]=image->GetPixel(outIt.GetIndex());
//       outIt.Set(vvv);
//     }
//
//
//   namespace stat = itk::Statistics;
//
//   typedef itk::Image<PixelType,ImageDimension> ClassImageType;
//
//
//   //----------------------------------------------------------------------
//   //Set membership function (Using the statistics objects)
//   //----------------------------------------------------------------------
//
//   typedef itk::Statistics::DistanceToCentroidMembershipFunction< VecPixelType >
//     MembershipFunctionType ;
//
//   typedef typename MembershipFunctionType::Pointer MembershipFunctionPointer ;
//
//   typedef std::vector< MembershipFunctionPointer >
//     MembershipFunctionPointerVector;
//
//   //----------------------------------------------------------------------
//   //Set the image model estimator
//   //----------------------------------------------------------------------
//   typedef itk::ImageKmeansModelEstimator< VecImageType,
//     MembershipFunctionType> ImageKmeansModelEstimatorType;
//
//   typename ImageKmeansModelEstimatorType::Pointer
//     applyKmeansModelEstimator = ImageKmeansModelEstimatorType::New();
//
//   //----------------------------------------------------------------------
//   //Set the parameters of the clusterer
//   //----------------------------------------------------------------------
//
//   Rcpp::Rcout << "Starting to build the K-means model ....." << std::endl;
//
//   applyKmeansModelEstimator->SetInputImage( vecImage );
//   applyKmeansModelEstimator->SetNumberOfModels(nclasses);
//   applyKmeansModelEstimator->SetThreshold(0.0001);
//   applyKmeansModelEstimator->Update();
//
//   MembershipFunctionPointerVector membershipFunctions =
//     applyKmeansModelEstimator->GetMembershipFunctions();
//
//   typedef std::vector<double> TempVectorType;
//   typedef TempVectorType::iterator TempVectorIterator;
//   TempVectorIterator  start, end;
//
//   std::vector<double> kmeansResultForClass(membershipFunctions.size());
//
//
//   Rcpp::Rcout << "Result of K-Means clustering" << std::endl;
//
//   double meanDistance=0;
//   for(unsigned int classIndex=0; classIndex < membershipFunctions.size();
//     classIndex++ )
//     {
//     kmeansResultForClass[classIndex] =
//       (double) (membershipFunctions[classIndex]->GetCentroid())[0];
//     meanDistance+=kmeansResultForClass[classIndex];//membershipFunctions[i]->GetMean()[0];
//     }
//   meanDistance/=(float)nclasses;
//   Rcpp::Rcout << " mean dist " << meanDistance << std::endl;
//
//
//   start = kmeansResultForClass.begin();
//   end   = kmeansResultForClass.end();
//
//   std::sort( start, end );
//
//   vnl_vector<double> temp =  membershipFunctions[0]->GetCentroid();
//   for(unsigned int classIndex=0; classIndex < membershipFunctions.size();
//     classIndex++ )
//     {
//     temp[0] = (double) kmeansResultForClass[classIndex];
//     membershipFunctions[classIndex]->SetCentroid(temp);
//     }
//
//   for(unsigned int classIndex=0; classIndex < membershipFunctions.size();
//     classIndex++ )
//     {
//     Rcpp::Rcout <<  (membershipFunctions[classIndex]->GetCentroid())[0] << std::endl;
//     }
//
//   //----------------------------------------------------------------------
//   //Set the decision rule
//   //----------------------------------------------------------------------
//   typedef itk::DecisionRuleBase::Pointer DecisionRuleBasePointer;
//
//   typedef itk::MinimumDecisionRule DecisionRuleType;
//   DecisionRuleType::Pointer
//     classifierDecisionRule = DecisionRuleType::New();
//
//   //------------------------------------------------------
//   //Instantiate the classifier model (as the input image is in right format)
//   //------------------------------------------------------
//
//   //Assign a class label image type
//
//   typedef itk::Image<PixelType,ImageDimension> ClassImageType;
//
//   typedef itk::ImageClassifierBase< VecImageType,ClassImageType >
//     SupervisedClassifierType;
//
//   typename SupervisedClassifierType::Pointer
//     classifierPointer = SupervisedClassifierType::New();
//
//
//   //------------------------------------------------------
//   // Set the Classifier parameters
//   //------------------------------------------------------
//   classifierPointer->SetNumberOfClasses( nclasses );
//   classifierPointer->SetInputImage( vecImage );
//
//   // Set the decison rule
//   classifierPointer->
//     SetDecisionRule( (DecisionRuleBasePointer) classifierDecisionRule );
//
//   MembershipFunctionPointer membershipFunction;
//   //------------------------------------------------------
//   //Set the classifier membership functions
//   //------------------------------------------------------
//   for( unsigned int i=0; i<nclasses; i++ )
//     {
//     classifierPointer->AddMembershipFunction( membershipFunctions[i] );
//     }
//
//   //Do the classification
//   //Run the kmeans classifier algorithm
//   classifierPointer->Update();
//
//   //Get the classified image
//   typedef typename ClassImageType::Pointer ClassifiedImagePointer;
//   ClassifiedImagePointer outClassImage =
//     classifierPointer->GetClassifiedImage();
//
//   //------------------------------------------------------
//   //Mask the output of the classifier
//   //------------------------------------------------------
//
//   // Declare the type for the MaskInput filter
//
//   typedef itk::MaskImageFilter< ClassImageType,
//                            ClassImageType,
//                            ClassImageType  >   MaskFilterType;
//
//   typedef typename ClassImageType::Pointer   MaskedOutputImagePointer;
//   typedef typename MaskFilterType::Pointer        MaskFilterTypePointer;
//
//   // Create an ADD Filter
//   MaskFilterTypePointer maskfilter = MaskFilterType::New();
//
//   // Connect the input images
//   maskfilter->SetInput1( outClassImage );
//   maskfilter->SetInput2( labelimage );
//
//   // Execute the filter
//   maskfilter->Update();
//
//   // Get the Smart Pointer to the Filter Output
//   MaskedOutputImagePointer maskedOutputImage = maskfilter->GetOutput();
//
//   //  this->SetClassifiedImage( maskedOutputImage );
//
//   //------------------------------------------------------
//   //Set the MRF labeller and populate the parameters
//   //------------------------------------------------------
//   //Set the MRF labeller
//   typedef itk::MRFImageFilter<VecImageType,ClassImageType>
//     MRFFilterType;
//
//   typename MRFFilterType::Pointer applyMRFFilter = MRFFilterType::New();
//
//   // Set the MRF labeller parameters
//   applyMRFFilter->SetNumberOfClasses(nclasses);
//   unsigned int m_MaximumNumberOfIterations=maxit;
//   applyMRFFilter->SetMaximumNumberOfIterations(m_MaximumNumberOfIterations);
//   float m_ErrorTolerance=1.e-5;
//   applyMRFFilter->SetErrorTolerance(m_ErrorTolerance);
//   float m_SmoothingFactor=smf;
//   applyMRFFilter->SetSmoothingFactor( m_SmoothingFactor );
//
//   //For setting up a square/cubic or hypercubic neighborhood
//   applyMRFFilter->SetNeighborhoodRadius( 1 );
//   std::vector<double> weights =
//     applyMRFFilter->GetMRFNeighborhoodWeight();
//   std::vector<double> testNewNeighborhoodWeight( weights.size(), 1);
//   double totalWeight = 0;
//   for(std::vector< double >::const_iterator wcIt = weights.begin();
//       wcIt != weights.end(); ++wcIt )
//     {
//     totalWeight += *wcIt;
//     }
//   unsigned int jj = 0;
//   for(std::vector< double >::iterator wIt = weights.begin();
//       wIt != weights.end(); wIt++ )
//     {
//      testNewNeighborhoodWeight[jj] = static_cast< double > ( (*wIt) * meanDistance / (2 * totalWeight));
//      //Rcpp::Rcout << " ow " << weights[jj] << " nw " <<  testNewNeighborhoodWeight[jj] << std::endl;
//     jj++;
//     }
//
//   applyMRFFilter->SetMRFNeighborhoodWeight( testNewNeighborhoodWeight );
//
//   applyMRFFilter->SetInput(vecImage);
//   applyMRFFilter->SetClassifier( classifierPointer );
//
//   //Kick off the MRF labeller function
//   applyMRFFilter->Update();
//
//   applyMRFFilter->Print(Rcpp::Rcout);
//   outClassImage = applyMRFFilter->GetOutput();
//
//   //------------------------------------------------------
//   //Mask the output of the classifier
//   //------------------------------------------------------
//
//   // Declare the type for the MaskInput filter
//
//   // Create an ADD Filter
//   MaskFilterTypePointer maskfilter2 = MaskFilterType::New();
//
//   // Connect the input images
//   maskfilter2->SetInput1( outClassImage );
//   maskfilter2->SetInput2( labelimage );
//
//   // Execute the filter
//   maskfilter2->Update();
//
//   // Get the Smart Pointer to the Filter Output
//   maskedOutputImage = maskfilter2->GetOutput();
//
//   //  this->SetClassifiedImage( maskedOutputImage );
//
//   return outClassImage;//maskedOutputImage;
//
// }




template<unsigned int ImageDimension>
int SmoothImage(int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sigma = 1.0;
  if (argc > argct) sigma=atof(argv[argct]);

  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer varimage = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());

  typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> dgf;
  typename dgf::Pointer filter = dgf::New();
  filter->SetVariance(sigma*sigma);
  bool usespacing=true;
  if (!usespacing) filter->SetUseImageSpacingOff();
  else filter->SetUseImageSpacingOn();
  filter->SetMaximumError(.01f);
  filter->SetInput(image1);
  filter->Update();
  varimage=filter->GetOutput();
  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( varimage );
  writer->Write();

  return 0;

}




template<unsigned int ImageDimension>
int MorphImage(int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sigma = 1.0;
  if (argc > argct) sigma=atof(argv[argct]);

  unsigned int morphopt=1;
  if ( strcmp(operation.c_str(),"ME") == 0) morphopt=0;
  else if ( strcmp(operation.c_str(),"MO") == 0) morphopt=2;
  else if ( strcmp(operation.c_str(),"MC") == 0) morphopt=3;
  else if ( strcmp(operation.c_str(),"GE") == 0) morphopt=4;
  else if ( strcmp(operation.c_str(),"GD") == 0) morphopt=5;
	else if ( strcmp(operation.c_str(),"GO") == 0) morphopt=6;
	else if ( strcmp(operation.c_str(),"GC") == 0) morphopt=7;

  typename ImageType::Pointer image1 = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());

  // SRD set dilatevalue
  float dilateval=1;
  if (argc > argct + 1) dilateval=atof(argv[argct+1]);


  image1=Morphological<ImageType>(image1, sigma, morphopt , dilateval);

 typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( image1 );
  writer->Write();

  return 0;

}


template <unsigned int ImageDimension>
int FastMarchingSegmentation( unsigned int argc, char *argv[] )
{
  typedef float InternalPixelType;
  typedef itk::Image<InternalPixelType, ImageDimension>  InternalImageType;
  typedef itk::Image<InternalPixelType, ImageDimension>  ImageType;

  typedef unsigned char OutputPixelType;
  typedef itk::Image<OutputPixelType, ImageDimension> OutputImageType;

  //  option->SetUsageOption( 0, "[speedImage,seedImage,<stoppingValue=max>,<topologyCheck=0>]" );
  unsigned int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }
  else { Rcpp::Rcout << " not enough parameters -- need label image " << std::endl;  return 0; }
  float stoppingValue=100.0;
  if (  argc > argct) { stoppingValue=atof(argv[argct]);   argct++; }
  int topocheck=0;
  if (  argc > argct) { topocheck=atoi(argv[argct]);   argct++; }


  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( fn1.c_str() );

  typedef itk::FastMarchingImageFilter<ImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader1->GetOutput() );

  typedef typename FilterType::NodeContainer NodeContainer;
  typedef typename FilterType::NodeType NodeType;
  typedef typename FilterType::LabelImageType LabelImageType;

  typedef itk::ImageFileReader<LabelImageType> LabelImageReaderType;
  typename LabelImageReaderType::Pointer labelImageReader =
    LabelImageReaderType::New();
  labelImageReader->SetFileName( fn2.c_str() );
  labelImageReader->Update();

  typedef itk::LabelContourImageFilter<LabelImageType, LabelImageType>
    ContourFilterType;
  typename ContourFilterType::Pointer contour = ContourFilterType::New();
  contour->SetInput( labelImageReader->GetOutput() );
  contour->FullyConnectedOff();
  contour->SetBackgroundValue(
    itk::NumericTraits<typename LabelImageType::PixelType>::Zero );
  contour->Update();

  typename NodeContainer::Pointer alivePoints = NodeContainer::New();
  alivePoints->Initialize();
  unsigned long aliveCount = 0;
  typename NodeContainer::Pointer trialPoints = NodeContainer::New();
  trialPoints->Initialize();
  unsigned long trialCount = 0;

  itk::ImageRegionIteratorWithIndex<LabelImageType> ItL(
    labelImageReader->GetOutput(),
    labelImageReader->GetOutput()->GetLargestPossibleRegion() );
  itk::ImageRegionIteratorWithIndex<LabelImageType> ItC( contour->GetOutput(),
    contour->GetOutput()->GetLargestPossibleRegion() );
  for( ItL.GoToBegin(), ItC.GoToBegin(); !ItL.IsAtEnd(); ++ItL, ++ItC )
    {
    if( ItC.Get() !=
      itk::NumericTraits<typename LabelImageType::PixelType>::Zero )
      {
      typename LabelImageType::IndexType position = ItC.GetIndex();

      NodeType node;
      const double value = 0.0;

      node.SetValue( value );
      node.SetIndex( position );
      trialPoints->InsertElement( trialCount++, node );
      }
    else if( ItL.Get() !=
      itk::NumericTraits<typename LabelImageType::PixelType>::Zero )
      {
      typename LabelImageType::IndexType position = ItL.GetIndex();

      NodeType node;
      const double value = 0.0;

      node.SetValue( value );
      node.SetIndex( position );
      alivePoints->InsertElement( aliveCount++, node );
      }
    }
  filter->SetTrialPoints( trialPoints );
  filter->SetAlivePoints( alivePoints );

  filter->SetStoppingValue( stoppingValue );
  filter->SetTopologyCheck( FilterType::None );
  if( topocheck == 1 )  // Strict
    {
      Rcpp::Rcout << " strict " << std::endl;
    filter->SetTopologyCheck( FilterType::Strict );
    }
  if( topocheck == 2 )  // No handles
    {
      Rcpp::Rcout << " no handles " << std::endl;
    filter->SetTopologyCheck( FilterType::NoHandles );
    }

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception caught !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }

  itk::ImageRegionIteratorWithIndex<ImageType> ItF(
    filter->GetOutput(),
    filter->GetOutput()->GetLargestPossibleRegion() );
  for( ItL.GoToBegin(), ItF.GoToBegin(); !ItL.IsAtEnd(); ++ItL, ++ItF )
    {
    if( ItL.Get() !=
      itk::NumericTraits<typename LabelImageType::PixelType>::Zero )
      {
      ItF.Set( -ItF.Get() );
      }
    }

    {
    typedef itk::ImageFileWriter<ImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname.c_str() );
    writer->SetInput( filter->GetOutput() );
    writer->Update();
    }

  return 0;
}


template<unsigned int ImageDimension>
int PropagateLabelsThroughMask(int argc, char *argv[])
{

 typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  float thresh=0.5;
  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  float stopval=100.0;
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }
  else { Rcpp::Rcout << " not enough parameters -- need label image " << std::endl;  return 0; }
  if (  argc > argct) { stopval=atof(argv[argct]);   argct++; }


  typename ImageType::Pointer speedimage = NULL;
  ReadImage<ImageType>(speedimage, fn1.c_str());
  typename ImageType::Pointer labimage = NULL;
  ReadImage<ImageType>(labimage, fn2.c_str());
  typename ImageType::Pointer fastimage = NULL;
  ReadImage<ImageType>(fastimage, fn1.c_str());
  typename ImageType::Pointer outlabimage = NULL;
  ReadImage<ImageType>(outlabimage, fn2.c_str());
  fastimage->FillBuffer(1.e9);
  // compute max label
  double maxlabel=0;
  Iterator vfIter2( labimage,  labimage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      bool isinside=true;
      double speedval=speedimage->GetPixel(vfIter2.GetIndex());
      double labval=labimage->GetPixel(vfIter2.GetIndex());
      if ( speedval < thresh ) isinside=false;
      if (isinside )
	{
	  if ( labval > maxlabel ) maxlabel=labval;
	}
    }

  for (unsigned int lab=1; lab<=(unsigned int)maxlabel; lab++)
    {
      typedef  itk::FastMarchingImageFilter< ImageType, ImageType >    FastMarchingFilterType;
      typename FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();
      fastMarching->SetInput( speedimage );
      fastMarching->SetTopologyCheck( FastMarchingFilterType::None );

      typedef typename FastMarchingFilterType::NodeContainer           NodeContainer;
      typedef typename FastMarchingFilterType::NodeType                NodeType;
      typename NodeContainer::Pointer seeds = NodeContainer::New();
      seeds->Initialize();
      unsigned long ct=0;
      for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
	{
	  bool isinside=true;
	  double speedval=speedimage->GetPixel(vfIter2.GetIndex());
	  double labval=labimage->GetPixel(vfIter2.GetIndex());
	  if ( speedval < thresh ) isinside=false;
	  if (isinside && (unsigned int) labval == lab )
	    {
	      NodeType node;
	      const double seedValue = 0.0;
	      node.SetValue( seedValue );
	      node.SetIndex( vfIter2.GetIndex() );
	      seeds->InsertElement( ct, node );
	      ct++;
	    }
	}
      fastMarching->SetTrialPoints(  seeds  );
      fastMarching->SetStoppingValue(  stopval );
      fastMarching->Update();
      for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
	{
	  bool isinside=true;
	  double speedval=speedimage->GetPixel(vfIter2.GetIndex());
	  double labval=labimage->GetPixel(vfIter2.GetIndex());
	  if ( speedval < thresh ) isinside=false;
	  if (isinside && labval == 0 )
	    {
	      double fmarrivaltime=fastMarching->GetOutput()->GetPixel( vfIter2.GetIndex() );
	      double mmm= fastimage->GetPixel(vfIter2.GetIndex());
	      if ( fmarrivaltime < mmm )
		{
		  fastimage->SetPixel(vfIter2.GetIndex() ,  fmarrivaltime );
		  outlabimage->SetPixel(vfIter2.GetIndex() , lab );
		}
	    }
	  else  if (!isinside) outlabimage->SetPixel(vfIter2.GetIndex() , 0 );
	}
    }
  std::string::size_type idx;
  idx = outname.find_first_of('.');
  std::string tempname = outname.substr(0,idx);
  std::string extension = outname.substr(idx,outname.length());
  std::string kname=tempname+std::string("_speed")+extension;
  WriteImage<ImageType>(fastimage,kname.c_str());
  WriteImage<ImageType>(outlabimage,outname.c_str());
  return 0;

}

template<unsigned int ImageDimension>
int DistanceMap(int argc, char *argv[])
{

 typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sigma = 1.0;
  if (argc > argct) sigma=atof(argv[argct]);

  typename ImageType::Pointer image1 = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());

  typedef itk::DanielssonDistanceMapImageFilter<ImageType, ImageType >  FilterType;

  typename  FilterType::Pointer filter = FilterType::New();
  filter->InputIsBinaryOff();
  filter->SetUseImageSpacing(true);
  filter->SetInput(image1);
  filter->Update();

  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( filter->GetOutput() );
  writer->Write();

  return 0;

}



template<unsigned int ImageDimension>
int FillHoles(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<int,ImageDimension> LabelImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  typedef itk::CastImageFilter<ImageType,LabelImageType> CastFilterType;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float holeparam = 2.0;
  if (argc > argct) holeparam=atof(argv[argct]);

  typename ImageType::Pointer image1 = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());
  typename ImageType::Pointer imageout = NULL;
  ReadImage<ImageType>(imageout, fn1.c_str());
  image1 = BinaryThreshold<ImageType>(0.5,1.e9,1,image1);


  typedef itk::DanielssonDistanceMapImageFilter<ImageType, ImageType >  FilterType;
  typename  FilterType::Pointer filter = FilterType::New();
  filter->InputIsBinaryOff();
  filter->SetUseImageSpacing(false);
  filter->SetInput(image1);
  filter->Update();
  // algorithm :
  // 1. get distance map of object
  // 2. threshold
  // 3. label connected components
  // 4. label surface
  // 5. if everywhere on surface is next to object then it's a hole
  // 6. make sure it's not the background
  typedef itk::Image<int, ImageDimension> labelimagetype;
  typename ImageType::Pointer dist = filter->GetOutput();
  typename ImageType::Pointer regions = BinaryThreshold<ImageType>(0.001,1.e9,1,dist);

  typedef itk::ConnectedComponentImageFilter< labelimagetype, labelimagetype > ccFilterType;
  typedef itk::RelabelComponentImageFilter< labelimagetype, ImageType > RelabelType;
  typename RelabelType::Pointer relabel = RelabelType::New();
  typename ccFilterType::Pointer ccfilter = ccFilterType::New();

  typename CastFilterType::Pointer castRegions = CastFilterType::New();
  castRegions->SetInput( regions );

  ccfilter->SetInput(castRegions->GetOutput());
  ccfilter->SetFullyConnected( 0 );
  ccfilter->Update();
  relabel->SetInput( ccfilter->GetOutput() );

  relabel->SetMinimumObjectSize( 0 );
  //    relabel->SetUseHistograms(true);
  try
    {
    relabel->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Relabel: exception caught !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }


    // WriteImage<ImageType>(relabel->GetOutput(),"test.nii");

  if (holeparam == 2)
    {
      Rcpp::Rcout <<" Filling all holes "<<  std::endl;
      typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
      Iterator vfIter( relabel->GetOutput(),  relabel->GetOutput()->GetLargestPossibleRegion() );

      for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
	{
	  if (vfIter.Get() > 1 ) imageout->SetPixel(vfIter.GetIndex(),1);
	}

      WriteImage<ImageType>(imageout,outname.c_str());

      return 0;
    }


  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (unsigned int j=0; j<ImageDimension; j++) rad[j]=1;
  iteratorType GHood(rad, relabel->GetOutput(),relabel->GetOutput()->GetLargestPossibleRegion());

  float maximum=relabel->GetNumberOfObjects();
  //now we have the exact number of objects labeled independently
  for (unsigned int lab=2; lab<=maximum; lab++)
    {
      float erat=2;
      if (holeparam  <= 1 )
	{
      GHood.GoToBegin();

      unsigned long objectedge=0;
      unsigned long backgroundedge=0;
      unsigned long totaledge=0;
      unsigned long volume=0;

      while (!GHood.IsAtEnd())
	{
	  typename ImageType::PixelType p = GHood.GetCenterPixel();
	  typename ImageType::IndexType ind = GHood.GetIndex();
	  typename ImageType::IndexType ind2;
	  if ( p == lab )
	    {
	      volume++;
	      for (unsigned int i = 0; i < GHood.Size(); i++)
		{
		  ind2=GHood.GetIndex(i);
		  float val2=image1->GetPixel(ind2);
		  if (val2 >= 0.5 && GHood.GetPixel(i) != lab )
		    {
		      objectedge++;
		      totaledge++;
		    }
		  else if (val2 < 0.5 && GHood.GetPixel(i) != lab )
		    {
		      backgroundedge++;
		      totaledge++;
		    }
		}
	    }
	  ++GHood;
	}
      float vrat=(float)totaledge/(float)volume;
      erat=(float)objectedge/(float)totaledge;
      Rcpp::Rcout <<" Lab " << lab << " volume " << volume << " v-rat " << vrat << " edge " << erat << std::endl;
	}

      if (erat > holeparam)// fill the hole
	{
	  Rcpp::Rcout <<" Filling " << lab << " of " << maximum <<  std::endl;
	  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
	  Iterator vfIter( relabel->GetOutput(),  relabel->GetOutput()->GetLargestPossibleRegion() );

	  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
	    {
	      if (vfIter.Get() == lab ) imageout->SetPixel(vfIter.GetIndex(),1);
	    }

	}

    }


  WriteImage<ImageType>(imageout,outname.c_str());


  return 0;

}




template<unsigned int ImageDimension>
int NormalizeImage(int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float option = 0;
  if (argc > argct) option=atof(argv[argct]);

  typename ImageType::Pointer image = NULL;
  ReadImage<ImageType>(image, fn1.c_str());

  float max=0;
  float min=1.e9;
  float mean = 0.0;
  unsigned long ct = 0;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image,  image->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      float pix=iter.Get();
      //      if (option == 0) if (pix < 0) pix=0;
      mean+=pix;
      ct++;
      if (pix > max ) max=pix;
      if (pix < min)  min=pix;
    }
  mean/=(float)ct;
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
    {
      float pix=iter.Get();
      if (option == 0)
	{
	  pix= (pix - min)/(max-min);
	  iter.Set(pix);
	}
      else iter.Set(pix/mean);
    }

  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( image );
  writer->Write();

  return 0;



}


template <unsigned int ImageDimension>
int PrintHeader(int argc, char *argv[])
{

  typedef  float  outPixelType;
  typedef  float floatPixelType;
  typedef  float inPixelType;
  typedef itk::Image<inPixelType,ImageDimension> ImageType;
  typedef itk::Image<floatPixelType,ImageDimension> IntermediateType;
  typedef itk::Image<outPixelType,ImageDimension> OutImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<OutImageType> writertype;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  if (argc > 20 ) Rcpp::Rcout << " k " << std::endl;
  //  std::string opt = std::string(argv[argct]);   argct++;

  typename readertype::Pointer reader = readertype::New();
  reader->SetFileName(fn1.c_str());
  reader->Update();
   Rcpp::Rcout << " Spacing " << reader->GetOutput()->GetSpacing() << std::endl;
   Rcpp::Rcout << " Origin " << reader->GetOutput()->GetOrigin() << std::endl;
   Rcpp::Rcout << " Direction " << std::endl << reader->GetOutput()->GetDirection() << std::endl;
   Rcpp::Rcout << " Size " << std::endl << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;

  //  if (strcmp(operation.c_str(),"n_last_dim") == 0){
    //unsigned int lastdim=reader->GetOutput()->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
    //   std::ofstream logfile;
    // logfile.open(outname.c_str() );
    //if (logfile.good()  )
    //{
    //  logfile << lastdim << std::endl;
    //}
    //cd Rcpp::Rcout << lastdim << std::endl;
    // }
  return 1;
}


template <unsigned int ImageDimension>
int GradientImage(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sig = 1;
  if (argc > argct) sig=atof(argv[argct]); argct++;
  if (sig <= 0) sig=0.5;
  bool normalize=false;
  if (argc > argct) normalize=atoi(argv[argct]);

  typename ImageType::Pointer image = NULL;
  typename ImageType::Pointer image2 = NULL;
  ReadImage<ImageType>(image, fn1.c_str());

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<ImageType, ImageType> dgf;
  typename dgf::Pointer filter = dgf::New();
  filter->SetSigma(sig);
  filter->SetInput(image);
  filter->Update();
  image2=filter->GetOutput();

  if ( !normalize )
    {
    WriteImage<ImageType>( image2,outname.c_str());
    return 0;
    }

  typedef itk::RescaleIntensityImageFilter<ImageType,ImageType > RescaleFilterType;
  typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 1 );
  rescaler->SetInput( image2 );
  WriteImage<ImageType>( rescaler->GetOutput() ,outname.c_str());
  return 0;

}

template <unsigned int ImageDimension>
int LaplacianImage(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sig = 1;
  if (argc > argct) sig=atof(argv[argct]);
  if (sig <= 0) sig=0.5;
  bool normalize=false;
  if (argc > argct) normalize=atoi(argv[argct]);

  typename ImageType::Pointer image = NULL;
  typename ImageType::Pointer image2 = NULL;
  ReadImage<ImageType>(image, fn1.c_str());


  typedef itk::LaplacianRecursiveGaussianImageFilter<ImageType,ImageType >  dgf;
  typename dgf::Pointer filter = dgf::New();
  filter->SetSigma(sig);
  filter->SetInput(image);
  filter->Update();
  image2=filter->GetOutput();
  if ( !normalize )
    {
    WriteImage<ImageType>( image2,outname.c_str());
    return 0;
    }

  typedef itk::RescaleIntensityImageFilter<ImageType,ImageType > RescaleFilterType;
  typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 1 );
  rescaler->SetInput( image2 );
  WriteImage<ImageType>( rescaler->GetOutput() ,outname.c_str());
  return 0;

}

template <unsigned int ImageDimension>
int PoissonDiffusion( int argc, char *argv[])
{
  if( argc < 6 )
    {
    Rcpp::Rcout << "Usage error---not enough arguments.   See help menu."
      << std::endl;
    throw std::exception() ;
    }

  typedef float PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::Image<int, ImageDimension> LabelImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[4] );
  reader->Update();

  typedef itk::ImageDuplicator<ImageType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( reader->GetOutput() );
  duplicator->Update();

  typename ImageType::Pointer output = duplicator->GetOutput();
  output->DisconnectPipeline();

  typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
  typename LabelReaderType::Pointer labelReader = LabelReaderType::New();
  labelReader->SetFileName( argv[5] );
  labelReader->Update();

  float label = 1.0;
  if( argc > 7 )
    {
    label = atof( argv[7] );
    }
  typedef itk::BinaryThresholdImageFilter<LabelImageType, LabelImageType> ThresholderType;
  typename ThresholderType::Pointer thresholder = ThresholderType::New();
  thresholder->SetInput( labelReader->GetOutput() );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 1 );
  thresholder->SetLowerThreshold( label );
  thresholder->SetUpperThreshold( label );
  thresholder->Update();

  typedef itk::BinaryThresholdImageFilter<ImageType, ImageType> ThresholderType2;
  typename ThresholderType2::Pointer thresholder2 = ThresholderType2::New();
  thresholder2->SetInput( reader->GetOutput() );
  thresholder2->SetOutsideValue( 0 );
  thresholder2->SetInsideValue( 1 );
  thresholder2->SetLowerThreshold( 0.2 );
  thresholder2->SetUpperThreshold( 1.e9 );
  thresholder2->Update();

  float sigma = 1.0;
  if( argc > 6 )
    {
    sigma = atof( argv[6] );
    }

  float convergence = itk::NumericTraits<float>::max();
  float convergenceThreshold = 1e-10;
  if( argc > 9 )
    {
    convergenceThreshold = atof( argv[9] );
    }
  unsigned int maximumNumberOfIterations = 500;
  if( argc > 8 )
    {
    maximumNumberOfIterations = atoi( argv[8] );
    }
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  float lastmean=0;
  unsigned int iterations = 0;
  while( iterations++ < maximumNumberOfIterations && convergence >= convergenceThreshold )
    {
    Rcpp::Rcout << "  Iteration " << iterations << ": " << convergence << std::endl;
    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( vnl_math_sqr( sigma ) );
    smoother->SetMaximumError( 0.01f );
    smoother->SetInput( output );

    /*
    typedef itk::MaximumImageFilter<ImageType, ImageType, ImageType>
      MaximumFilterType;
    typename MaximumFilterType::Pointer maximumFilter = MaximumFilterType::New();
    maximumFilter->SetInput1( smoother->GetOutput() );
    maximumFilter->SetInput2( reader->GetOutput() );

    typedef itk::MultiplyImageFilter<ImageType, LabelImageType, ImageType>
      MultiplierType;
    typename MultiplierType::Pointer multiplier = MultiplierType::New();
    multiplier->SetInput1( maximumFilter->GetOutput() );
    multiplier->SetInput2( thresholder->GetOutput() );

    typedef itk::AddImageFilter<ImageType, ImageType, ImageType>
     AdderType;
    typename AdderType::Pointer adder = AdderType::New();
    adder->SetInput1( reader->GetOutput() );
    adder->SetInput2( multiplier->GetOutput() );

    typedef itk::SubtractImageFilter<ImageType, ImageType, ImageType>
      SubtracterType;
    typename SubtracterType::Pointer subtracter = SubtracterType::New();
    subtracter->SetInput1( adder->GetOutput() );
    subtracter->SetInput2( output );
    */
    typedef itk::LabelStatisticsImageFilter<ImageType, LabelImageType>
      StatsFilterType;
    typename StatsFilterType::Pointer stats = StatsFilterType::New();
    stats->SetInput( smoother->GetOutput() );
    stats->SetLabelInput( thresholder->GetOutput() );
    stats->Update();

    convergence = stats->GetMean( 1 ) - lastmean;
    lastmean=stats->GetMean( 1 );
    output =  smoother->GetOutput();
    output->DisconnectPipeline();

    Iterator vfIter( output,  output->GetLargestPossibleRegion() );
    for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      if (  thresholder2->GetOutput()->GetPixel(vfIter.GetIndex()) == 1 ) {
        vfIter.Set(reader->GetOutput()->GetPixel(vfIter.GetIndex()));
      }
    }


    }

    Iterator vfIter( output,  output->GetLargestPossibleRegion() );
    for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      if (  thresholder2->GetOutput()->GetPixel(vfIter.GetIndex()) == 1 ) {
        vfIter.Set(reader->GetOutput()->GetPixel(vfIter.GetIndex()));
      }
      if (  thresholder->GetOutput()->GetPixel(vfIter.GetIndex()) == 0 ) {
        vfIter.Set(0);
      }
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( output );
  writer->Update();

  return 0;
}





template <unsigned int ImageDimension>
void
RemoveLabelInterfaces(int argc, char *argv[])
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return; }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;


  typename ImageType::Pointer input = NULL;
  ReadImage<ImageType>(input , fn1.c_str());


  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (unsigned int j=0; j<ImageDimension; j++) rad[j]= 1;
  iteratorType GHood(rad, input,input->GetLargestPossibleRegion());

  GHood.GoToBegin();

//  Rcpp::Rcout << " foreg " << (int) foreground;
  while (!GHood.IsAtEnd())
  {
    typename ImageType::PixelType p = GHood.GetCenterPixel();
    typename ImageType::IndexType ind = GHood.GetIndex();
    typename ImageType::IndexType ind2;
    if ( p > 0  )
    {
      bool atedge=false;

      for (unsigned int i = 0; i < GHood.Size(); i++)
      {
        ind2=GHood.GetIndex(i);
	if (GHood.GetPixel(i) > 0 && GHood.GetPixel(i)  != p )
        {
          atedge=true;
        }
      }
      if (atedge) input->SetPixel(ind,0);
    }
    ++GHood;
  }


 WriteImage<ImageType>(input,outname.c_str());

  return;
}


template <unsigned int ImageDimension>
void
EnumerateLabelInterfaces(int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string outname2 = "";
  if (argc  > argct) outname2=std::string(argv[argct]);   argct++;
  float nfrac = 0.1;
  if (argc  > argct) nfrac=atof(argv[argct]);   argct++;

  typename ImageType::Pointer input = NULL;
  ReadImage<ImageType>(input , fn1.c_str());
  typename ImageType::Pointer output = NULL;
  ReadImage<ImageType>(output , fn1.c_str());
  output->FillBuffer(0);
  typename ImageType::Pointer colored = NULL;
  ReadImage<ImageType>(colored , fn1.c_str());
  colored->FillBuffer(0);

  unsigned int max=0;
  Iterator o_iter( input,input->GetLargestPossibleRegion() );
  o_iter.GoToBegin() ;
  while ( !o_iter.IsAtEnd() )
    {
    unsigned int label = (unsigned int) o_iter.Get();
    if ( label > max ) max=label;
    ++o_iter;
    }
  Rcpp::Rcout << " Max Label " << max << std::endl;
  typedef itk::Image<float,2>  myInterfaceImageType;
  typename myInterfaceImageType::SizeType size;
  size[0]=max+1;
  size[1]=max+1;
  typename myInterfaceImageType::RegionType region;
  typename myInterfaceImageType::SpacingType spacing;
  spacing.Fill(1);
  typename myInterfaceImageType::PointType origin;
  origin.Fill(max/2);
  region.SetSize(size);
  typename myInterfaceImageType::Pointer faceimage = myInterfaceImageType::New();
  faceimage->SetSpacing(spacing);
  faceimage->SetOrigin(origin);
  faceimage->SetRegions(region );
  faceimage->Allocate();
  faceimage->FillBuffer( 0 );

typename myInterfaceImageType::Pointer colorimage = myInterfaceImageType::New();
  colorimage->SetSpacing(spacing);
  colorimage->SetOrigin(origin);
  colorimage->SetRegions(region );
  colorimage->Allocate();
  colorimage->FillBuffer( 0 );
  typedef itk::ImageRegionIteratorWithIndex<myInterfaceImageType> FIterator;

// we can use this to compute a 4-coloring of the brain

  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (unsigned int j=0; j<ImageDimension; j++) rad[j]= 1;
  iteratorType GHood(rad, input,input->GetLargestPossibleRegion());

  GHood.GoToBegin();

//  Rcpp::Rcout << " foreg " << (int) foreground;
  while (!GHood.IsAtEnd())
  {
    typename ImageType::PixelType p = GHood.GetCenterPixel();
    typename ImageType::IndexType ind = GHood.GetIndex();
    typename ImageType::IndexType ind2;
    if ( p > 0  )
    {
    bool atedge=false;

    unsigned long linearinda=0,linearindb=0,linearind=0;
    for (unsigned int i = 0; i < GHood.Size(); i++)
      {
      ind2=GHood.GetIndex(i);
      if (GHood.GetPixel(i) > 0 && GHood.GetPixel(i)  != p )
        {
	atedge=true;
	typename myInterfaceImageType::IndexType inda,indb;
	inda[0]=(long int)p;  inda[1]=(long int)GHood.GetPixel(i);
	indb[1]=(long int)p;  indb[0]=(long int)GHood.GetPixel(i);
	faceimage->SetPixel(inda,faceimage->GetPixel(inda)+1);
	faceimage->SetPixel(indb,faceimage->GetPixel(indb)+1);
	linearinda=inda[1]*size[0]+inda[0];
	linearindb=indb[1]*size[0]+indb[0];
	if (linearinda < linearindb) linearind=linearinda;
	else linearind=linearindb;
        }
      }
    if (atedge) output->SetPixel(ind,linearind);
    }
    ++GHood;
  }


// first normalize the interfaces
  typename myInterfaceImageType::IndexType find;
  typename myInterfaceImageType::IndexType find2;
  find2.Fill(0);
  for (unsigned int j=0; j<=max; j++)
    {
    find[0]=j;
    float total=0;
    for (unsigned int i=0; i<=max; i++)
      {
      find[1]=i;
      total+= faceimage->GetPixel(find) ;
//      if ( faceimage->GetPixel(find) > 50 )
      //Rcpp::Rcout << i <<"  :: " << faceimage->GetPixel(find)  << std::endl;
      }
    Rcpp::Rcout  << " total interfaces for label :  " << j << " are " << total << std::endl;
    for (unsigned int i=0; i<=max; i++)
      {
      find[1]=i;
      if (total > 0) faceimage->SetPixel(find,faceimage->GetPixel(find)/total);
      if ( faceimage->GetPixel(find) >  0.01 )
	Rcpp::Rcout << i <<"  :: " << faceimage->GetPixel(find)  << std::endl;
      }
    }



// colors 1 , 2 , 3 , 4
// set all to color 1.
  typedef std::vector<unsigned int>                  ColorSetType;
  colorimage->FillBuffer(0);
//  for (unsigned int j=1; j<=max; j++)
	for (unsigned int j=max; j>=1; j--)
    {
    ColorSetType myColorSet1;
    ColorSetType myColorSet2;
    ColorSetType myColorSet3;
    ColorSetType myColorSet4;
    ColorSetType myColorSet5;
    find[0]=j;
// list all indices that have an interface with j
//    for (unsigned int i=1; i<=max; i++)
   for (unsigned int i=max; i>=1; i--)
      {
      if ( i !=  j)
	{
	find[1]=i;
	find2[0]=i;
	unsigned int color=(unsigned int) colorimage->GetPixel(find2);
	if ( faceimage->GetPixel(find) >  nfrac ) // then color
	  {
	  if ( color == 1)
	    myColorSet1.push_back( (unsigned int) i );
	  if ( color == 2)
	    myColorSet2.push_back( (unsigned int) i );
	  if ( color == 3)
	    myColorSet3.push_back( (unsigned int) i );
	  if ( color == 4)
	    myColorSet4.push_back( (unsigned int) i );
	  if ( color == 5)
	    myColorSet5.push_back( (unsigned int) i );
// now you know all of j's neighbors and their colors
	  }
	}
      }

//we have to recolor j so that his color is different
//than any in the label set
    unsigned int okcolor=6;
    find[0]=j;
    find[1]=0;
    if( myColorSet1.empty()  )
      okcolor=1;
    else if( myColorSet2.empty()  )
      okcolor=2;
    else if( myColorSet3.empty()  )
      okcolor=3;
    else if( myColorSet4.empty()  )
      okcolor=4;
    else if( myColorSet5.empty()  )
      okcolor=5;

    colorimage->SetPixel(find,okcolor);
		Rcpp::Rcout <<" Label " << j << " color " << okcolor << std::endl;
    }

  o_iter.GoToBegin() ;
  colored->FillBuffer(0);
  while ( !o_iter.IsAtEnd() )
    {
    unsigned int label = (unsigned int) o_iter.Get();
    if (label > 0)
      {
      find[0]=label;
      find[1]=0;
      unsigned int color=(unsigned int)colorimage->GetPixel(find);
      colored->SetPixel(o_iter.GetIndex(),color);
      }
      ++o_iter;
    }


 WriteImage<ImageType>(output,outname.c_str());
 WriteImage<myInterfaceImageType>(faceimage,(std::string("face")+outname).c_str());
 if (outname2.length() > 3) WriteImage<ImageType>(colored,outname2.c_str());

  return;
}





template <unsigned int ImageDimension>
int CountVoxelDifference(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }
  std::string maskfn = "";
  if (  argc > argct) { maskfn=std::string(argv[argct]);   argct++; }


  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;
  typename ImageType::Pointer mask = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());
  ReadImage<ImageType>(image2, fn2.c_str());
  ReadImage<ImageType>(mask, maskfn.c_str());

  unsigned long maskct=0;
  float err=0,poserr=0,negerr=0;

  Iterator It ( mask,mask->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
      if( It.Get() > 0  )
	{
	  float p1=image1->GetPixel(It.GetIndex());
	  float p2=image2->GetPixel(It.GetIndex());
	  float locerr=p1-p2;
	  err+=fabs(locerr);
	  if (locerr > 0) poserr+=locerr;
	  if (locerr < 0) negerr+=locerr;
	  maskct++;
	}
    }


  if (maskct ==0) maskct=1;
  std::ofstream logfile;
  logfile.open(outname.c_str() );
  if (logfile.good()  )
    {

      logfile << " Err " <<  " : " << err << " %ER " <<  " : " << err/(maskct)*100. << " NER " <<  " : " << negerr/maskct*100.0 <<std::endl;
    }
  return 0;
}


template <unsigned int ImageDimension>
int DiceAndMinDistSum(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }
  std::string outdistfn = "";
  if (  argc > argct) { outdistfn=std::string(argv[argct]);   argct++; }
  std::string diceimagename=ANTSGetFilePrefix(outname.c_str())+std::string("dice.nii.gz");
  std::string mdsimagename=ANTSGetFilePrefix(outname.c_str())+std::string("mds.nii.gz");

  typename ImageType::Pointer image1 = NULL;
  typename ImageType::Pointer image2 = NULL;
  typename ImageType::Pointer outdist = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());
  if ( fn2.length() > 3) ReadImage<ImageType>(image2, fn2.c_str());
  if ( outdistfn.length() > 3)
    {
      ReadImage<ImageType>(outdist, fn1.c_str());
      outdist->FillBuffer(0);
    }
  // 1: for each label, and each image, compute the distance map
  // 2: sum min-dist(i) over every point in each image
  // 3: take average over all points
  typedef float PixelType;
  typedef std::vector<PixelType>                  LabelSetType;
  LabelSetType myLabelSet1;
  {
/** count the labels in the image */
  Iterator It ( image1,image1->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PixelType label = It.Get();
    if( fabs(label) > 0 )
      {
      if( find( myLabelSet1.begin(), myLabelSet1.end(), label )
	  == myLabelSet1.end() )
	{
	myLabelSet1.push_back( label );
	}
      }
    }}

  LabelSetType myLabelSet2;
  unsigned int labct=0;
  { Iterator It ( image2,image2->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PixelType label = It.Get();
    if( fabs(label) > 0 )
      {
      if( find( myLabelSet2.begin(), myLabelSet2.end(), label )
	  == myLabelSet2.end()   &&
	  find( myLabelSet1.begin(), myLabelSet1.end(), label )
	  != myLabelSet1.end() )
	{
	myLabelSet2.push_back( label );
	labct++;
	}
      }
    }}



  vnl_vector<double> distances(labct,0.0);
  vnl_vector<double> dicevals(labct,0.0);
  vnl_vector<double> rovals(labct,0.0);
  vnl_vector<double> tpvals(labct,0.0);
  vnl_vector<double> tpvals2(labct,0.0);

  /** now we have the common labels */
  std::ofstream logfile;
  logfile.open(outname.c_str() );
  labct=0;
  typename LabelSetType::const_iterator it;
  for( it = myLabelSet2.begin(); it != myLabelSet2.end(); ++it )
    {
    typename ImageType::Pointer mask1 = BinaryThreshold<ImageType>(*it,*it,1,image1);
    typename ImageType::Pointer surf = NULL;
    typename ImageType::Pointer d1 = NULL;
    typename ImageType::Pointer d2 = NULL;

    float count1=0;// count vox in label 1
    float count2=0;// count vox in label 2
    float counti=0;// count vox intersection in label 1 and label 2
    float countu=0;// count vox union label 1 and label 2
    float surfdist=0,surfct=0;
    //    float truepos=0;
    if (outdist)
      {
	surf=LabelSurface<ImageType>(mask1,mask1);
	//	WriteImage<ImageType>(surf,outdistfn.c_str());
	//exit(0);
	typedef itk::DanielssonDistanceMapImageFilter<ImageType, ImageType >  FilterType;
	typename  FilterType::Pointer dfilter1 = FilterType::New();
	dfilter1->InputIsBinaryOn();
	dfilter1->SetUseImageSpacing(true);
	dfilter1->SetInput(mask1);

	typename ImageType::Pointer mask2 = BinaryThreshold<ImageType>(*it,*it,1,image2);
	typename  FilterType::Pointer dfilter2 = FilterType::New();
	dfilter2->InputIsBinaryOn();
	dfilter2->SetUseImageSpacing(true);
	dfilter2->SetInput(mask2);

	dfilter1->Update();
	d1=dfilter1->GetOutput();

	dfilter2->Update();
	d2=dfilter2->GetOutput();
      }

    float dist1=0;
    float dist2=0;
    Iterator It2 ( image2,image2->GetLargestPossibleRegion() );
    for( It2.GoToBegin(); !It2.IsAtEnd(); ++It2 )
      {

	if (It2.Get() == *it || image1->GetPixel(It2.GetIndex()) == *it) countu++;
	if (It2.Get() == *it && image1->GetPixel(It2.GetIndex()) == *it) counti++;

      if (It2.Get() == *it)
	{
	count2++;
	if (d1) dist2+=d1->GetPixel(It2.GetIndex());
	}
      if ( image1->GetPixel(It2.GetIndex()) == *it)
	{
	count1++;
	if (d2)	dist1+=d2->GetPixel(It2.GetIndex());
	}
      if (outdist)
	if ( surf->GetPixel(It2.GetIndex()) > 0 )
	  {
	    float sdist=d2->GetPixel(It2.GetIndex());
	    outdist->SetPixel(It2.GetIndex(),sdist);
	    surfdist+=sdist;
	    surfct+=1;
	  }

      }
    //    Rcpp::Rcout << " sdist " << surfdist << " sct " << surfct << std::endl

    if (outdist) WriteImage<ImageType>(outdist,outdistfn.c_str());

    if (count2+count1 > 0)
      {
	distances[labct]+=(dist2+dist1)/(count2+count1);
	dicevals[labct]=2.0*counti/(count2+count1);
	rovals[labct]=counti/(countu);
	tpvals[labct]=counti/count1;
	tpvals2[labct]=counti/count2;
      }
    labct++;

    }

  labct=0;
  float sum=0,sumdice=0,sumro=0;
  //  std::sort(myLabelSet2.begin(),myLabelSet2.end());
  unsigned long labelcount=0;
  for( it = myLabelSet2.begin(); it != myLabelSet2.end(); ++it ) labelcount++;
  float temp=sqrt((float)labelcount);
  unsigned int squareimagesize=(unsigned int)(temp+1);
  typedef itk::Image<float,2>  TwoDImageType;
  typename TwoDImageType::RegionType newregion;
  typename TwoDImageType::SizeType size;
  size[0]=size[1]=squareimagesize;
  newregion.SetSize(size);
  typename TwoDImageType::Pointer squareimage = TwoDImageType::New();
  typename TwoDImageType::SpacingType spacingb;
  spacingb.Fill(1);
  typename TwoDImageType::PointType origin;
  origin.Fill(0);
  squareimage->SetSpacing(spacingb);
  squareimage->SetOrigin(origin);
  squareimage->SetRegions(newregion );
  squareimage->Allocate();
  squareimage->FillBuffer( 0 );
  typename TwoDImageType::Pointer squareimage2 = TwoDImageType::New();
  squareimage2->SetSpacing(spacingb);
  squareimage2->SetOrigin(origin);
  squareimage2->SetRegions(newregion );
  squareimage2->Allocate();
  squareimage2->FillBuffer( 0 );

  labelcount=0;
  for( it = myLabelSet2.begin(); it != myLabelSet2.end(); ++it )
    {
    if (logfile.good()  )
      {
	if (outdist)
	logfile <<" Label " << *it << " MD " << distances[labct] << " DICE " << dicevals[labct] << std::endl;
	else
	  logfile <<" Label " << *it << " DICE " << dicevals[labct] << "  RO " << rovals[labct] << " TP1 " << tpvals[labct] << " TP2 " << tpvals2[labct] << std::endl;
      }
    sum+=distances[labct];
    sumdice+=dicevals[labct];
    sumro+=rovals[labct];
// square image
    squareimage->GetBufferPointer()[labct]=distances[labct];
    squareimage2->GetBufferPointer()[labct]= dicevals[labct];
    labct++;
    }

  if (labct ==0) labct=1;
  if (logfile.good()  )
    {
      if (outdist)
	logfile <<" AvgMinDist " <<  " : " << sum/(float)labct << " AvgDice " <<  " : " << sumdice/(float)labct << " AvgRO " << " : " << sumro/(float)labct << std::endl;
      else
	logfile << " AvgDice " <<  " : " << sumdice/(float)labct << std::endl;
    }


  WriteImage<TwoDImageType>(squareimage,mdsimagename.c_str());
  WriteImage<TwoDImageType>(squareimage2,diceimagename.c_str());

  return 0;
}

template <unsigned int ImageDimension>
int Lipschitz( int argc, char *argv[] )
{
  if (argc > 20 ) Rcpp::Rcout << " k " << std::endl;
  Rcpp::Rcout << " Compute Lipschitz continuity of the mapping " << std::endl;

  typedef float RealType;
  typedef itk::Image<RealType, ImageDimension> RealImageType;
  typedef itk::Vector<RealType, ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension> VectorImageType;
  typedef itk::ImageRegionIteratorWithIndex<VectorImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string vecname = std::string(argv[argct]);   argct++;


  /**
   * Read in vector field
   */
  typedef itk::ImageFileReader< VectorImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( vecname.c_str() );
  // reader->SetUseAvantsNamingConvention( true );
  reader->Update();
  typename VectorImageType::Pointer vecimage=reader->GetOutput();

  typename RealImageType::Pointer lipcon = RealImageType::New();
  lipcon->SetOrigin( vecimage->GetOrigin() );
  lipcon->SetSpacing( vecimage->GetSpacing() );
  lipcon->SetRegions( vecimage->GetLargestPossibleRegion() );
  lipcon->SetDirection(  vecimage->GetDirection());
  lipcon->Allocate();
  lipcon->FillBuffer(0);

  itk::TimeProbe timer;
  timer.Start();
/** global Lipschitz points */
  typename VectorImageType::PointType gx; gx.Fill(0);
  typename VectorImageType::PointType gxt; gxt.Fill(0);
  typename VectorImageType::PointType gy; gy.Fill(0);
  typename VectorImageType::PointType gyt; gyt.Fill(0);
  typename VectorImageType::PointType lx;  lx.Fill(0);
  typename VectorImageType::PointType lxt;  lxt.Fill(0);
  typename VectorImageType::PointType ly;  ly.Fill(0);
  typename VectorImageType::PointType lyt;  lyt.Fill(0);
  float globalmaxval=0;
  unsigned long ct1=0;
//  unsigned long numpx=vecimage->GetBufferedRegion().GetNumberOfPixels();
    Iterator It1 ( vecimage,vecimage->GetLargestPossibleRegion() );
    for( It1.GoToBegin(); !It1.IsAtEnd(); ++It1 )
      {
      ct1++;
      typename VectorImageType::PointType x;
      typename VectorImageType::PointType xt;
      vecimage->TransformIndexToPhysicalPoint( It1.GetIndex(), x);
      typename VectorImageType::PixelType vecx=It1.Get();
      for (unsigned int i=0; i<ImageDimension;i++) xt[i]=x[i]+vecx[i];

      float localmaxval=0;
      unsigned long ct2=0;
      Iterator It2 ( vecimage,vecimage->GetLargestPossibleRegion() );
      for( It2.GoToBegin(); !It2.IsAtEnd(); ++It2 )
      {
      ct2++;
      if (ct2 != ct1)
      {
      typename VectorImageType::PointType y;
      typename VectorImageType::PointType yt;
      vecimage->TransformIndexToPhysicalPoint( It2.GetIndex(), y);
      typename VectorImageType::PixelType vecy=It2.Get();

      float numer=0;
      float denom=0;
      for (unsigned int i=0; i<ImageDimension;i++)
	{
	yt[i]=y[i]+vecy[i];
	numer+=(yt[i]-xt[i])*(yt[i]-xt[i]);
	denom+=(y[i]-x[i])*(y[i]-x[i]);
	}
      numer=sqrt(numer);
      denom=sqrt(denom);
      float localval=numer/denom;
      if (localval > localmaxval)
	{
	lx=x;
	ly=y;
	lxt=xt;
	lyt=yt;
	localmaxval=localval;
	}
      }
      }
      if (localmaxval > globalmaxval)
	{
	gx=lx;
	gy=ly;
	gxt=lxt;
	gyt=lyt;
	globalmaxval = localmaxval;
	}
      lipcon->SetPixel(It1.GetIndex(),localmaxval);
//      if (ct1 % 1000 == 0) Rcpp::Rcout << " Progress : " << (float ) ct1 / (float) numpx *100.0 << " val " << localmaxval << std::endl;
      }

    Rcpp::Rcout << " Lipschitz continuity related to: " << globalmaxval << std::endl;
    Rcpp::Rcout << " Tx :  " << gxt << "  Ty: " << gyt << std::endl;
    Rcpp::Rcout << " x :  " << gx << "  y: " << gy << std::endl;
    timer.Stop();
//    Rcpp::Rcout << "Elapsed time: " << timer.GetMeanTime()  << std::endl;

    typedef itk::ImageFileWriter<RealImageType> RealImageWriterType;
    typename RealImageWriterType::Pointer realwriter = RealImageWriterType::New();
    realwriter->SetFileName( outname.c_str() );
    realwriter->SetInput( lipcon );
    realwriter->Update();

    return 0;

}


template <unsigned int ImageDimension>
int ExtractVectorComponent( int argc, char *argv[] )
{
  if (argc <= 2 ) { Rcpp::Rcout << " too few options " << std::endl; return 1; }
  typedef float PixelType;
  typedef itk::VectorImage<PixelType, ImageDimension> ImageType;
  typedef itk::Image<PixelType, ImageDimension> RealImageType;
  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string inname = std::string(argv[argct]);   argct++;
  unsigned int whichvec = atoi(argv[argct]);   argct++;
  typedef itk::ImageFileReader< ImageType> ReaderType;
  typename ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( inname.c_str() );
  reader1->Update();
  typename ImageType::Pointer vecimage=reader1->GetOutput();
  if ( whichvec >= vecimage->GetVectorLength() ){
    Rcpp::Rcout << " input image " << inname << " only has " << vecimage->GetVectorLength() << " components " <<std::endl;
    return EXIT_FAILURE;
  }
  else {
    typename RealImageType::Pointer component = RealImageType::New();
    component->SetOrigin( vecimage->GetOrigin() );
    component->SetSpacing( vecimage->GetSpacing() );
    component->SetRegions( vecimage->GetLargestPossibleRegion() );
    component->SetDirection(  vecimage->GetDirection());
    component->Allocate();
    component->FillBuffer(0);
    typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
    Iterator It1 ( vecimage,vecimage->GetLargestPossibleRegion() );
    for( It1.GoToBegin(); !It1.IsAtEnd(); ++It1 )
    {
      component->SetPixel(It1.GetIndex(),It1.Get()[whichvec]);
    }
    typedef itk::ImageFileWriter<RealImageType> RealImageWriterType;
    typename RealImageWriterType::Pointer realwriter = RealImageWriterType::New();
    realwriter->SetFileName( outname.c_str() );
    realwriter->SetInput( component );
    realwriter->Update();
  }
  return EXIT_SUCCESS;
}

template <unsigned int ImageDimension>
int InvId( int argc, char *argv[] )
{
  if (argc > 2 )
  Rcpp::Rcout << " Compute  phi(  phi^{-1}(x)) " << std::endl;
  else return 1;
  typedef float RealType;
  typedef itk::Image<RealType, ImageDimension> RealImageType;
  typedef itk::Vector<RealType, ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension> VectorImageType;
  typedef itk::ImageRegionIteratorWithIndex<VectorImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string vecname1 = std::string(argv[argct]);   argct++;
  std::string vecname2 = std::string(argv[argct]);   argct++;


  /**
   * Read in vector field
   */
  typedef itk::ImageFileReader< VectorImageType> ReaderType;
  typename ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( vecname1.c_str() );
  //  reader1->SetUseAvantsNamingConvention( true );
  reader1->Update();
  typename VectorImageType::Pointer vecimage1=reader1->GetOutput();
  typename ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( vecname2.c_str() );
  //  reader2->SetUseAvantsNamingConvention( true );
  reader2->Update();
  typename VectorImageType::Pointer vecimage2=reader2->GetOutput();

  typename RealImageType::Pointer invid = RealImageType::New();
  invid->SetOrigin( vecimage1->GetOrigin() );
  invid->SetSpacing( vecimage1->GetSpacing() );
  invid->SetRegions( vecimage1->GetLargestPossibleRegion() );
  invid->SetDirection(  vecimage1->GetDirection());
  invid->Allocate();
  invid->FillBuffer(0);

  itk::TimeProbe timer;
  timer.Start();

  typedef itk::VectorLinearInterpolateImageFunction<VectorImageType,float> DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer vinterp =  DefaultInterpolatorType::New();
  vinterp->SetInputImage(vecimage2);


/** global Lipschitz points */
  typename VectorImageType::PointType gx;  gx.Fill(0);
  float globalmaxval=0;
  Iterator It1 ( vecimage1,vecimage1->GetLargestPossibleRegion() );
  for( It1.GoToBegin(); !It1.IsAtEnd(); ++It1 )
    {
    typename VectorImageType::PointType x;
    typename DefaultInterpolatorType::PointType xt,yt;
    vecimage1->TransformIndexToPhysicalPoint( It1.GetIndex(), x);
    typename VectorImageType::PixelType vecx=It1.Get();
    for (unsigned int i=0; i<ImageDimension;i++) xt[i]=x[i]+vecx[i];
//above, we warped the point -- now we index the other field with this
//point

    typename VectorImageType::PixelType disp;
    if (vinterp->IsInsideBuffer(xt)) disp = vinterp->Evaluate(xt);
    else disp.Fill(0);
    for (unsigned int jj=0; jj<ImageDimension; jj++) yt[jj]=xt[jj]+disp[jj];

    float error=0;
    for (unsigned int jj=0; jj<ImageDimension; jj++) error+=(yt[jj]-x[jj])*(yt[jj]-x[jj]);
    error=sqrt(error);
    if (error >  globalmaxval) { globalmaxval=error; gx=x; }
    invid->SetPixel(It1.GetIndex(),error);
    }
  Rcpp::Rcout << " Max error " << globalmaxval << " at " << gx << std::endl;
    timer.Stop();
//    Rcpp::Rcout << "Elapsed time: " << timer.GetMeanTime()  << std::endl;

    typedef itk::ImageFileWriter<RealImageType> RealImageWriterType;
    typename RealImageWriterType::Pointer realwriter = RealImageWriterType::New();
    realwriter->SetFileName( outname.c_str() );
    realwriter->SetInput( invid );
    realwriter->Update();

    return 0;

}

template <unsigned int ImageDimension>
int LabelStats(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string imagename=ANTSGetFilePrefix(outname.c_str())+std::string(".nii.gz");
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }

  typename ImageType::Pointer image = NULL;
  typename ImageType::Pointer valimage = NULL;
  ReadImage<ImageType>(image, fn1.c_str());
  if ( fn2.length() > 3) ReadImage<ImageType>(valimage, fn2.c_str());

  typedef float PixelType;
  typedef std::vector<PixelType>                  LabelSetType;
  LabelSetType myLabelSet;
/** count the labels in the image */
 Iterator It ( image,image->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PixelType label = It.Get();
    if( fabs(label) > 0 )
      {
        if( find( myLabelSet.begin(), myLabelSet.end(), label )
	    == myLabelSet.end() )
          {
          myLabelSet.push_back( label );
          }
      }
    }

  // compute the voxel volume
  typename ImageType::SpacingType spacing=image->GetSpacing();
  float volumeelement=1.0;
  for (unsigned int i=0;  i<spacing.Size(); i++)  volumeelement*=spacing[i];

  std::ofstream logfile;
  logfile.open(outname.c_str() );

  std::sort(myLabelSet.begin(),myLabelSet.end());
  typename LabelSetType::const_iterator it;
  unsigned long labelcount=0;
  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it ) labelcount++;

  float temp=sqrt((float)labelcount);
  unsigned int squareimagesize=(unsigned int)(temp+1);

  typedef itk::Image<float,2>  TwoDImageType;
  typename TwoDImageType::RegionType newregion;
  typename TwoDImageType::SizeType size;
  size[0]=size[1]=squareimagesize;
  newregion.SetSize(size);
  typename TwoDImageType::Pointer squareimage = TwoDImageType::New();
  typename TwoDImageType::SpacingType spacingb;
  spacingb.Fill(1);
  typename TwoDImageType::PointType origin;
  origin.Fill(0);
  squareimage->SetSpacing(spacingb);
  squareimage->SetOrigin(origin);
  squareimage->SetRegions(newregion );
  squareimage->Allocate();
  squareimage->FillBuffer( 0 );

  labelcount=0;
  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it )
    {
    float currentlabel= *it ;
    float totalvolume=0;
    float totalmass=0;
    float totalct=0;
    typename ImageType::PointType myCenterOfMass;
    myCenterOfMass.Fill(0);
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      PixelType label = It.Get();
      if(  label == currentlabel  )
	{
	totalvolume+=volumeelement;
	totalct+=1;
	if ( valimage) totalmass+=valimage->GetPixel(It.GetIndex());

	// compute center of mass
	typename ImageType::PointType point;
	image->TransformIndexToPhysicalPoint(It.GetIndex(), point);
	for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]+=point[i];
	}
      }

    for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]/=(float)totalct;

    if (!valimage)
	Rcpp::Rcout <<" Volume Of Label " << *it << " is " << totalvolume <<   "  Avg-Location " << myCenterOfMass <<std::endl;
    else // if ( totalvolume > 500 &&  totalmass/totalct > 1/500 )  {
      {
          Rcpp::Rcout << " Volume Of Label " << *it << " is " << totalvolume <<   "  Avg-Location " << myCenterOfMass <<" mass is " << totalmass << " average-val is " << totalmass/totalct << std::endl;
      //      Rcpp::Rcout << *it << "  " <<  totalvolume <<  " & " <<  totalmass/totalct   << " \ " << std::endl;
    }

// square image
    squareimage->GetBufferPointer()[labelcount]=totalmass/totalct;
    labelcount++;
    }


  logfile.close();

  WriteImage<TwoDImageType>(squareimage,imagename.c_str());

  return 0;

}

//int is the key, string the return value
std::map<unsigned int,std::string> RoiList(std::string file)
{
  unsigned int wordindex=0;
  std::string tempstring="";
  std::map<unsigned int,std::string> RoiList;
  //  RoiList[0]=std::string("Background");
  char str[2000];
  std::fstream file_op(file.c_str(),std::ios::in);
  while(file_op >> str)
  {
    tempstring=std::string(str);
    RoiList[wordindex]=tempstring;
    wordindex++;
  }
  return RoiList; //returns the maximum index
}
// now words can be accessed like this WordList[n]; where 'n' is the index


template <unsigned int ImageDimension>
int ROIStatistics(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  // first create a label image => ROI value map
  std::map <unsigned int,std::string> cortroimap;
cortroimap[1]=std::string("L. Occipital Lobe");
cortroimap[2]=std::string("R. Occipital Lobe");
cortroimap[3]=std::string("L. Cingulate Gyrus");
cortroimap[4]=std::string("R. Cingulate Gyrus");
cortroimap[5]=std::string("L. Insula");
cortroimap[1]=std::string("CSF");
cortroimap[2]=std::string("CGM");
cortroimap[3]=std::string("WM");
cortroimap[4]=std::string("DGM");
cortroimap[5]=std::string("Cerebellum");
cortroimap[6]=std::string("R. Insula");
cortroimap[7]=std::string("L. Temporal Pole");
cortroimap[8]=std::string("R. Temporal Pole");
cortroimap[9]=std::string("L. Sup. Temp. Gyrus");
cortroimap[10]=std::string("R. Sup. Temp. Gyrus ");
cortroimap[11]=std::string("L. Infero. Temp. Gyrus");
cortroimap[12]=std::string("R. Infero. Temp. Gyrus");
cortroimap[13]=std::string("L. Parahippocampal Gyrus");
cortroimap[14]=std::string("R. Parahippocampal Gyrus");
cortroimap[15]=std::string("L. Frontal Pole");
cortroimap[16]=std::string("R. Frontal Pole");
cortroimap[17]=std::string("L. Sup. Frontal Gyrus");
cortroimap[18]=std::string("R. Sup. Frontal Gyrus");
cortroimap[19]=std::string("L. Mid. Frontal Gyrus");
cortroimap[20]=std::string("R. Mid. Frontal Gyrus");
cortroimap[21]=std::string("L. Inf.  Frontal Gyrus");
cortroimap[22]=std::string("R. Inf. Frontal Gyrus");
cortroimap[23]=std::string("L. Orbital Frontal Gyrus");
cortroimap[24]=std::string("R. Orbital Frontal Gyrus");
cortroimap[25]=std::string("L. Precentral Gyrus");
cortroimap[26]=std::string("R. Precentral Gyrus");
cortroimap[27]=std::string("L. Sup. Parietal Lobe");
cortroimap[28]=std::string("R. Sup. Parietal Lobe");
cortroimap[29]=std::string("L. Inf. Parietal Lobe");
cortroimap[30]=std::string("R. Inf. Parietal Lobe");
cortroimap[31]=std::string("L. Postcentral Gyrus");
cortroimap[32]=std::string("R. Postcentral Gyrus");
cortroimap[33]=std::string("L. Hipp. Head");
cortroimap[34]=std::string("R. Hipp Head");
cortroimap[35]=std::string("L. Hipp Midbody");
cortroimap[36]=std::string("R. Hipp Midbody");
cortroimap[37]=std::string("L. Hipp Tail");
cortroimap[38]=std::string("R. Hipp Tail");
cortroimap[39]=std::string("L. Caudate");
cortroimap[40]=std::string("R. Caudate");
cortroimap[41]=std::string("L. Putamen");
cortroimap[42]=std::string("R. Putamen");
cortroimap[43]=std::string("L. Thalamus");
cortroimap[44]=std::string("R. Thalamus");
cortroimap[45]=std::string("White Matter");


  std::map <unsigned int,std::string> wmroimap;
  wmroimap[1]=std::string("R. corticospinal Tract");
  wmroimap[2]=std::string("R. inferior fronto-occipital fasciculus");
  wmroimap[3]=std::string("R. inferior longitudinal fasciculus");
  wmroimap[4]=std::string("R. superior longitudinal fasciculus");
  wmroimap[5]=std::string("R. uncinate fasciculus");
  wmroimap[6]=std::string("L. corticospinal Tract");
  wmroimap[7]=std::string("L. inferior fronto-occipital fasciculus");
  wmroimap[8]=std::string("L. inferior longitudinal fasciculus");
  wmroimap[9]=std::string("L. superior longitudinal fasciculus");
  wmroimap[10]=std::string("L. uncinate fasciculus");
  wmroimap[11]=std::string("Anterior corpus callosum");
  wmroimap[12]=std::string("Posterior corpus callosum");
  wmroimap[13]=std::string("Mid-body corpus callosum");
  //  if(grade_list.find("Tim") == grade_list.end()) {  Rcpp::Rcout<<"Tim is not in the map!"<<endl; }
  //mymap.find('a')->second
  int argct=2;
  if ( argc < 6 ) 
    {
      Rcpp::Rcout<<" not enough parameters --- usage example 1 :"<<"" <<std::endl;
      Rcpp::Rcout<< argv[0] << " ImageMath  3 output.csv ROIStatistics roinames.txt LabelImage.nii.gz ValueImage.nii.gz  " << std::endl;
      throw std::exception() ;
    }
  std::string outname=std::string(argv[argct]); argct++;
  std::string imagename=ANTSGetFilePrefix(outname.c_str())+std::string(".nii.gz");
  std::string csvname=ANTSGetFilePrefix(outname.c_str())+std::string(".csv");
  typedef vnl_matrix<double>      MatrixType;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn0 = std::string(argv[argct]);   argct++;
  Rcpp::Rcout <<"  fn0 " << fn0 << std::endl;
  std::map <unsigned int,std::string> roimap=RoiList(fn0);
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 = "";
  if (  argc > argct) { fn2=std::string(argv[argct]);   argct++; }
  std::string fn3 = "";
  if (  argc > argct) { fn3=std::string(argv[argct]);   argct++; }
  std::string fn4 = "";
  if (  argc > argct) { fn4=std::string(argv[argct]);   argct++; }
  std::string fn5 = "";
  if (  argc > argct) { fn5=std::string(argv[argct]);   argct++; }

  typename ImageType::Pointer image = NULL;
  typename ImageType::Pointer valimage = NULL;
  typename ImageType::Pointer valimage3 = NULL;
  typename ImageType::Pointer valimage4 = NULL;
  typename ImageType::Pointer valimage5 = NULL;
  ReadImage<ImageType>(image, fn1.c_str());
  if ( fn2.length() > 3) ReadImage<ImageType>(valimage, fn2.c_str());
  if ( fn3.length() > 3) ReadImage<ImageType>(valimage3, fn3.c_str());
  if ( fn4.length() > 3) ReadImage<ImageType>(valimage4, fn4.c_str());
  if ( fn5.length() > 3) ReadImage<ImageType>(valimage5, fn5.c_str());

  typedef float PixelType;
  typedef std::vector<PixelType>                  LabelSetType;
  LabelSetType myLabelSet;
/** count the labels in the image */
  unsigned long maxlab=0;
  Iterator It ( image,image->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PixelType label = It.Get();
    if( fabs(label) > 0 )
      {
        if( find( myLabelSet.begin(), myLabelSet.end(), label )
	    == myLabelSet.end() )
          {
          myLabelSet.push_back( label );
          }
	if (label > maxlab) maxlab=(unsigned long)label;
      }
    }
  // define the output matrix
  // for a csv file of  n_regions for rows
  // with columns of   Volume,CenterOfMassX,CenterOfMassY,CenterOfMassZ,CenterOfMassTime,
  MatrixType mCSVMatrix(maxlab,ImageDimension+1,0);


  //  maxlab=32; // for cortical analysis
  // compute the voxel volume
  typename ImageType::SpacingType spacing=image->GetSpacing();
  float volumeelement=1.0;
  for (unsigned int i=0;  i<spacing.Size(); i++)  volumeelement*=spacing[i];

  vnl_vector<double> pvals(maxlab+1,1.0);
  vnl_vector<double> pvals3(maxlab+1,1.0);
  vnl_vector<double> pvals4(maxlab+1,1.0);
  vnl_vector<double> pvals5(maxlab+1,1.0);
  vnl_vector<double> clusters(maxlab+1,0.0);
  vnl_vector<double> masses(maxlab+1,0.0);
  //  typename ImageType::PointType mycomlist[33];
  std::ofstream logfile;
  logfile.open(outname.c_str() );

  std::sort(myLabelSet.begin(),myLabelSet.end());
  typename LabelSetType::const_iterator it;
  unsigned long labelcount=0;
  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it )
    { 
      labelcount++;
    }


  typedef itk::LinearInterpolateImageFunction<ImageType,float> ScalarInterpolatorType;
  typename ScalarInterpolatorType::Pointer interp =  ScalarInterpolatorType::New();
  if (valimage) 
    interp->SetInputImage(valimage);
  else interp->SetInputImage(image);
  // iterate over the label image and index into the stat image 
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
  {
    PixelType label = It.Get();
    typename ImageType::PointType point;
    image->TransformIndexToPhysicalPoint(It.GetIndex(), point);
    float vv=interp->Evaluate( point );
    //    float vv = valimage->GetPixel(It.GetIndex());
    if(  label > 0  && fabs(vv) > 1.e-9 )
      {
        clusters[(unsigned long) label]=clusters[(unsigned long) label]+1;
        masses[(unsigned long) label]=masses[(unsigned long) label]+vv;
      }
  }
  logfile<<"ROIName,ROINumber,ClusterSize,Mass,comX,comY,comZ,comT"<<std::endl;
  //  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it )
  for (unsigned int mylabel=1; mylabel<maxlab+1; mylabel++)
    { 
      unsigned int roi=mylabel-1;
      /* first count which roi it is by iterating through the label sets
      it = myLabelSet.begin();
      while (  (*it) != mylabel && it != myLabelSet.end() ) 
	{
	  Rcpp::Rcout << " it " << *it << " roi " << roi << " mylabel " << mylabel << std::endl;
	  roi++;
	  ++it;
	  }*/

      typename ImageType::PointType myCenterOfMass;
      myCenterOfMass.Fill(0);
      unsigned long totalct=0;
      if ( clusters[mylabel] > 0 )
	{
        for( It.GoToBegin(); !It.IsAtEnd(); ++It )
	{
	  PixelType label = It.Get();
	  if(  label == mylabel )
	    {
	      totalct+=1;
	      // compute center of mass
	      typename ImageType::PointType point;
	      image->TransformIndexToPhysicalPoint(It.GetIndex(), point);
	      for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]+=point[i];
	    }
  	}
	}// if clusters big enough 
      if ( totalct == 0 ) totalct=1;
      for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]/=(float)totalct;
      double comx=0,comy=0,comz=0,comt=0;
      comx=myCenterOfMass[0];
      if (ImageDimension>=2) comy=myCenterOfMass[1];
      if (ImageDimension>=3) comz=myCenterOfMass[2];
      if (ImageDimension==4) comt=myCenterOfMass[3];
      if ( roi < roimap.size() )
	logfile << roimap[roi]<<","<<roi+1<<","<<clusters[roi+1] << ","<<masses[roi+1]<<","<<comx<<","<<comy<<","<<comz<<","<<comt<<std::endl;
    }
  logfile.close();
  
  return 0;

  float temp=sqrt((float)labelcount);
  unsigned int squareimagesize=(unsigned int)(temp+1);

  typedef itk::Image<float,2>  TwoDImageType;
  typename TwoDImageType::RegionType newregion;
  typename TwoDImageType::SizeType size;
  size[0]=size[1]=squareimagesize;
  newregion.SetSize(size);
  typename TwoDImageType::Pointer squareimage = TwoDImageType::New();
  typename TwoDImageType::SpacingType spacingb;
  spacingb.Fill(1);
  typename TwoDImageType::PointType origin;
  origin.Fill(0);
  squareimage->SetSpacing(spacingb);
  squareimage->SetOrigin(origin);
  squareimage->SetRegions(newregion );
  squareimage->Allocate();
  squareimage->FillBuffer( 0 );

  labelcount=0;
  typename ImageType::PointType myCenterOfMass;
  myCenterOfMass.Fill(0);
  //  for (unsigned int i=0; i<=maxlab; i++) mycomlist[i]=myCenterOfMass;
  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it )
    {
    float currentlabel= *it ;
    float totalvolume=0;
    float totalmass=0;
    float totalct=0;
    float maxoneminuspval=0.0;
    float maxoneminuspval3=0.0;
    float maxoneminuspval4=0.0;
    float maxoneminuspval5=0.0;
    myCenterOfMass.Fill(0);
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      PixelType label = It.Get();
      if(  label == currentlabel  )
	{
	totalvolume+=volumeelement;
	totalct+=1;
	float vv=valimage->GetPixel(It.GetIndex());
	if ( valimage) totalmass+=vv;
	if (  vv > maxoneminuspval) maxoneminuspval=vv;
	if ( valimage3 ){
	vv=valimage3->GetPixel(It.GetIndex());
	if (  fabs(vv) > fabs(maxoneminuspval3)) maxoneminuspval3=vv;
	}
	if ( valimage4 ){
	vv=valimage4->GetPixel(It.GetIndex());
	if (  vv > maxoneminuspval4) maxoneminuspval4=vv;
	}
	if ( valimage5 ){
	vv=valimage5->GetPixel(It.GetIndex());
	if (  vv > maxoneminuspval5) maxoneminuspval5=vv;
	}

	// compute center of mass
	typename ImageType::PointType point;
	image->TransformIndexToPhysicalPoint(It.GetIndex(), point);
	for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]+=point[i];
	}
      }
    if ( totalct == 0 ) totalct=1;
    for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]/=(float)totalct;

    clusters[(unsigned long)*it]=totalvolume;
    masses[(unsigned long)*it]=totalmass/totalct;
    pvals[(unsigned long)*it]=1.0-maxoneminuspval;
    pvals3[(unsigned long)*it]=maxoneminuspval3;
    pvals4[(unsigned long)*it]=1.0-maxoneminuspval4;
    pvals5[(unsigned long)*it]=1.0-maxoneminuspval5;
    // mycomlist[(unsigned long)*it]=myCenterOfMass;
// square image
    squareimage->GetBufferPointer()[labelcount]=totalmass/totalct;
    labelcount++;
    }

    for (unsigned int roi=1; roi <=maxlab ; roi++)
      {
	if (roi < maxlab )
	logfile << roimap.find(roi)->second << ",";
	else logfile << roimap.find(roi)->second << ","; //<< std::endl;
      }
    for (unsigned int roi=maxlab+1; roi <=maxlab*2 ; roi++)
      {
	if (roi < maxlab*2 )
	logfile << roimap.find(roi-maxlab)->second+std::string("mass") << ",";
	else logfile << roimap.find(roi-maxlab)->second+std::string("mass") << std::endl;
      }
    for (unsigned int roi=1; roi <=maxlab ; roi++)
      {
	if (roi < maxlab )
	logfile << clusters[roi] << ",";
	else logfile << clusters[roi] << ","; //<< std::endl;
      }
    for (unsigned int roi=maxlab+1; roi <=maxlab*2 ; roi++)
      {
	if (roi < maxlab*2 )
	logfile << masses[roi-maxlab] << ",";
	else logfile << masses[roi-maxlab] << std::endl;
      }
    logfile.close();
    return 0;
    for (unsigned int roi=1; roi <=maxlab ; roi++)
      {
	//unsigned int resol=5000;
	//unsigned int intpvalue=resol*totalmass/totalct;
	//	float pvalue=1.0-(float)intpvalue/(float)resol;// average pvalue
	//	myCenterOfMass=mycomlist[roi];
        if( roimap.find(roi) != roimap.end()  )
 	  Rcpp::Rcout << roimap.find(roi)->second << " & " << clusters[roi] << " , "  << pvals[roi] << "  & " <<  pvals3[roi]  << " &  " << pvals4[roi]   << " &  " << (float)((int)(myCenterOfMass[0]*10))/10. << " " << (float)((int)(myCenterOfMass[1]*10))/10.  << " " <<  (float)((int)(myCenterOfMass[2]*10))/10.  << "   \\ " << std::endl;

	mCSVMatrix(roi,0)=clusters[roi];
	for (unsigned int mydim=0;mydim<ImageDimension;mydim++)
	  mCSVMatrix(roi,mydim+1)=myCenterOfMass[mydim];
    }

  //  WriteImage<TwoDImageType>(squareimage,imagename.c_str());

    // write out the array2D object
    std::vector<std::string> ColumnHeaders;
    std::string colname=std::string("ClusterSize");
    ColumnHeaders.push_back( colname );
    colname=std::string("comX");
    ColumnHeaders.push_back( colname );
    colname=std::string("comY");
    if (ImageDimension >= 2)
      ColumnHeaders.push_back( colname );
    colname=std::string("comZ");
    if (ImageDimension >= 3)
      ColumnHeaders.push_back( colname );
    colname=std::string("comT");
    if (ImageDimension >= 4)
      ColumnHeaders.push_back( colname );

    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( csvname );
    writer->SetInput( &mCSVMatrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }


  return 0;

}


template <unsigned int ImageDimension>
int ByteImage(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ByteImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  float sig = 1;
  if (argc > argct) sig=atof(argv[argct]);
  if (sig <= 0) sig=0.5;

  typename ImageType::Pointer image = NULL;
  typename ByteImageType::Pointer image2 = NULL;
  ReadImage<ImageType>(image, fn1.c_str());

  typedef itk::RescaleIntensityImageFilter<ImageType,ByteImageType > RescaleFilterType;
  typename RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( image );

  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(outname.c_str());
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  return 0;

}



template <unsigned int ImageDimension>
int PValueImage(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  unsigned int dof = 1;
  if (argc > argct) dof=atoi(argv[argct]); argct++;

  typename ImageType::Pointer image = NULL;
  ReadImage<ImageType>(image, fn1.c_str());


  Rcpp::Rcout <<" read Image" << fn1 << " dof " << dof << std::endl;
  typedef itk::Statistics::TDistribution DistributionType;
  typename DistributionType::Pointer distributionFunction = DistributionType::New();
  distributionFunction->SetDegreesOfFreedom(  dof );
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator vfIter( image,  image->GetLargestPossibleRegion() );

  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      float val=image->GetPixel(vfIter.GetIndex());
      if ( ! vnl_math_isnan(val) && ! vnl_math_isinf(val) )
	{
	  if ( fabs( val ) > 0 )
	    {
	      float pval=0;
	      if (val >= 1) pval = 1.0-distributionFunction->EvaluateCDF( val , dof );
	      else if (val < 1) pval = distributionFunction->EvaluateCDF( val , dof );
	      image->SetPixel(vfIter.GetIndex() , pval );
	    }
	}
      else image->SetPixel(vfIter.GetIndex() , 0);
    }

  WriteImage<ImageType>(image,outname.c_str());

  return 0;

}


template <unsigned int ImageDimension>
int ConvertImageSetToMatrix(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string ext = itksys::SystemTools::GetFilenameExtension( outname );
  std::string operation = std::string(argv[argct]);  argct++;
  unsigned int  rowcoloption = atoi(argv[argct]);   argct++;
  std::string maskfn=std::string(argv[argct]); argct++;
  unsigned int numberofimages=0;
  typename ImageType::Pointer mask = NULL;
  ReadImage<ImageType>(mask,maskfn.c_str());
  unsigned long voxct=0;
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5) voxct++;

  typename ImageType::Pointer image2 = NULL;
  typename ImageType::SizeType size;
  size.Fill(0);
  unsigned int bigimage=0;
  for (unsigned int j=argct; j< argc; j++)
    {
    numberofimages++;
    // Get the image dimension
    std::string fn = std::string(argv[j]);
    typename itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(fn.c_str(), itk::ImageIOFactory::ReadMode);
    imageIO->SetFileName(fn.c_str());
    imageIO->ReadImageInformation();
    for (unsigned int i=0; i<imageIO->GetNumberOfDimensions(); i++)
      {
      if ( imageIO->GetDimensions(i) > size[i] )
	{
	size[i]=imageIO->GetDimensions(i);
	bigimage=j;
	Rcpp::Rcout << " bigimage " << j << " size " << size << std::endl;
	}
      }
    }

  std:: cout << " largest image " << size << " num images " << numberofimages << " voxct " << voxct << std::endl;
  unsigned long xx1=0,yy1=0;
  if (rowcoloption==0)
    {  Rcpp::Rcout << " row option " << std::endl;  xx1=voxct;  yy1=numberofimages; }
  if (rowcoloption==1)
    {  Rcpp::Rcout << " col option " << std::endl;  yy1=voxct;  xx1=numberofimages; }
  unsigned long xsize=xx1;
  unsigned long ysize=yy1;

  if  (strcmp(ext.c_str(),".csv") == 0 ) {
    typedef itk::Array2D<double> MatrixType;
    std::vector<std::string> ColumnHeaders;
    MatrixType matrix(xsize,ysize);
    matrix.Fill(0);
    unsigned int imagecount=0;
    for (unsigned int j=argct; j< argc; j++)
    {
      std::string fn = std::string(argv[j]);
      ReadImage<ImageType>(image2,fn.c_str());
      Rcpp::Rcout << " image " << j << " is "  << fn << std::endl;
      unsigned long xx=0,yy=0,tvoxct=0;
      if (rowcoloption==0)
	{    yy=imagecount; }
      if (rowcoloption==1)
	{  xx=imagecount; }
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	{
	  if (mIter.Get() >= 0.5)
	    {
	      if (rowcoloption==0)
		{   xx=tvoxct;  }
	      if (rowcoloption==1)
		{  yy=tvoxct; }
	      matrix[xx][yy]=image2->GetPixel(mIter.GetIndex());
              std::string colname=std::string("V")+ants_to_string<unsigned int>(tvoxct);
	      if ( imagecount == 0 ) ColumnHeaders.push_back( colname );
	      tvoxct++;
	    }
	}
      imagecount++;
    }

    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname );
    writer->SetInput( &matrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
  }
  else {
/** declare the tiled image */
  typename MatrixImageType::SizeType tilesize;
  tilesize[0]=xsize;
  tilesize[1]=ysize;
  Rcpp::Rcout << " allocate matrix " << tilesize << std::endl;
  typename MatrixImageType::RegionType region;
  region.SetSize( tilesize );

  typename MatrixImageType::Pointer matimage=MatrixImageType::New();
  matimage->SetLargestPossibleRegion( region );
  matimage->SetBufferedRegion( region );
  typename MatrixImageType::DirectionType mdir;  mdir.Fill(0); mdir[0][0]=1; mdir[1][1]=1;
  typename MatrixImageType::SpacingType mspc;  mspc.Fill(1);
  typename MatrixImageType::PointType morg;  morg.Fill(0);
  matimage->SetSpacing( mspc );
  matimage->SetDirection(mdir);
  matimage->SetOrigin( morg );
  matimage->Allocate();

  unsigned int imagecount=0;

  for (unsigned int j=argct; j< argc; j++)
    {
      std::string fn = std::string(argv[j]);
      ReadImage<ImageType>(image2,fn.c_str());
      Rcpp::Rcout << " image " << j << " is "  << fn << std::endl;
      unsigned long xx=0,yy=0,tvoxct=0;
      if (rowcoloption==0)
	{    yy=imagecount; }
      if (rowcoloption==1)
	{  xx=imagecount; }
      typename MatrixImageType::IndexType mind;
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	{
	  if (mIter.Get() >= 0.5)
	    {
	      if (rowcoloption==0)
		{   xx=tvoxct;  }
	      if (rowcoloption==1)
		{  yy=tvoxct; }
	      mind[0]=xx;
	      mind[1]=yy;
	      //	      Rcpp::Rcout << " Mind " << mind << std::endl;
	      matimage->SetPixel(mind,image2->GetPixel(mIter.GetIndex()));
	      tvoxct++;
	    }
	}
      imagecount++;
    }

  Rcpp::Rcout << " mat size " << matimage->GetLargestPossibleRegion().GetSize() << std::endl;
  WriteImage<MatrixImageType>(matimage,outname.c_str());
  }
  return 0;

}





template <unsigned int ImageDimension>
int RandomlySampleImageSetToCSV(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRandomConstIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string ext = itksys::SystemTools::GetFilenameExtension( outname );
  std::string operation = std::string(argv[argct]);  argct++;
  unsigned int  n_samples = atoi(argv[argct]);   argct++;
  /* std::string maskfn=std::string(argv[argct]); argct++;
  typename ImageType::Pointer mask = NULL;
  ReadImage<ImageType>(mask,maskfn.c_str());
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5) voxct++;
  */
  unsigned long voxct=0;
  unsigned int numberofimages=0;

  typename ImageType::Pointer image2 = NULL;
  for (unsigned int j=argct; j< argc; j++)
    {
    numberofimages++;
    }
  unsigned long xsize=numberofimages;
  unsigned long ysize=n_samples;

  if  (strcmp(ext.c_str(),".csv") == 0 ) {
    typedef itk::Array2D<double> MatrixType;
    std::vector<std::string> ColumnHeaders;
    MatrixType matrix(xsize,ysize);
    matrix.Fill(0);
    unsigned int imagecount=0;
    for (unsigned int j=argct; j< argc; j++)
    {
      std::string fn = std::string(argv[j]);
      ReadImage<ImageType>(image2,fn.c_str());
      Iterator mIter( image2,image2->GetLargestPossibleRegion() );
      mIter.SetNumberOfSamples(n_samples);
      Rcpp::Rcout << " image " << j << " is "  << fn << std::endl;
      voxct=0;
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	{
          matrix[imagecount][voxct]=image2->GetPixel(mIter.GetIndex());
	  if ( imagecount == 0 ) {
            std::string colname=std::string("V")+ants_to_string<unsigned int>(voxct);
            ColumnHeaders.push_back( colname );
	  }
	  voxct++;
	}
      imagecount++;
    }

    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname );
    writer->SetInput( &matrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
  }
  else {
    Rcpp::Rcout << " need a csv file as output type , you tried " << outname << std::endl;
  }
  return 0;

}


template <class T>
inline std::string to_string (const T& t)
{
std::stringstream ss;
ss << t;
return ss.str();
}

template <unsigned int ImageDimension>
int ConvertImageSetToEigenvectors(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  int argct=2;
  if (argc < 5 ) { Rcpp::Rcout <<" need more args -- see usage   " << std::endl;  throw std::exception() ; }
  std::string outname=std::string(argv[argct]); argct++;
  std::string ext = std::string(".csv"); // itksys::SystemTools::GetFilenameExtension( outname );
  std::string operation = std::string(argv[argct]);  argct++;
  unsigned int  n_evecs = atoi(argv[argct]);   argct++;
  unsigned int rowcoloption=1;
  std::string maskfn=std::string(argv[argct]); argct++;
  unsigned int numberofimages=0;
  typename ImageType::Pointer mask = NULL;
  ReadImage<ImageType>(mask,maskfn.c_str());
  /** 1. compute max value in mask */
  unsigned long maxval=0;
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() > maxval ) maxval=(unsigned long) mIter.Get();
  if ( maxval == 0 ) {
    Rcpp::Rcout <<" Max value in mask is <= 0, aborting. " << maxval << std::endl;
    throw std::exception() ;
  }

  typedef itk::Array2D<double> MatrixType;
  typename ImageType::Pointer image2 = NULL;
  typename ImageType::SizeType size;
  size.Fill(0);
  unsigned int bigimage=0;
  for (unsigned int j=argct; j< argc; j++)
    {
    numberofimages++;
    // Get the image dimension
    std::string fn = std::string(argv[j]);
    typename itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(fn.c_str(), itk::ImageIOFactory::ReadMode);
    imageIO->SetFileName(fn.c_str());
    imageIO->ReadImageInformation();
    for (unsigned int i=0; i<imageIO->GetNumberOfDimensions(); i++)
      {
      if ( imageIO->GetDimensions(i) > size[i] )
	{
	size[i]=imageIO->GetDimensions(i);
	bigimage=j;
	Rcpp::Rcout << " bigimage " << j << " size " << size << std::endl;
	}
      }
    }

  std:: cout << " largest image " << size << " num images " << numberofimages << std::endl;
  MatrixType avg_matrix(numberofimages,maxval);
  avg_matrix.Fill(0);

  for ( unsigned long mv=1; mv<=maxval; mv++ ) {
    /** 2. count the voxels in this label */
  unsigned long voxct=0;
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() == mv ) voxct++;

  unsigned long xx1=0,yy1=0;
  if (rowcoloption==0)
    {  Rcpp::Rcout << " row option " << std::endl;  xx1=voxct;  yy1=numberofimages; }
  if (rowcoloption==1)
    {  Rcpp::Rcout << " col option " << std::endl;  yy1=voxct;  xx1=numberofimages; }
  unsigned long xsize=xx1;
  unsigned long ysize=yy1;

  if  (strcmp(ext.c_str(),".csv") == 0 ) {
    MatrixType matrix(xsize,ysize);
    matrix.Fill(0);
    unsigned int imagecount=0;
    for (unsigned int j=argct; j< argc; j++)
    {
      std::string fn = std::string(argv[j]);
      ReadImage<ImageType>(image2,fn.c_str());
      Rcpp::Rcout << " image " << j << " is "  << fn << std::endl;
      unsigned long xx=0,yy=0,tvoxct=0;
      if (rowcoloption==0)
	{    yy=imagecount; }
      if (rowcoloption==1)
	{  xx=imagecount; }
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	{
	  if (mIter.Get() == mv )
	    {
	      if (rowcoloption==0)
		{   xx=tvoxct;  }
	      if (rowcoloption==1)
		{  yy=tvoxct; }
	      matrix[xx][yy]=image2->GetPixel(mIter.GetIndex());
	      tvoxct++;
	    }
	}
      imagecount++;
    }

    typedef double Scalar;
    typedef itk::ants::antsMatrixUtilities<ImageType,Scalar>  matrixOpType;
    typename matrixOpType::Pointer matrixOps=matrixOpType::New();
    typedef vnl_vector<Scalar> VectorType;
    MatrixType evec_matrix(xsize,n_evecs+1);
    evec_matrix.Fill(0);
    VectorType avg=matrixOps->AverageColumns(matrix);
    avg_matrix.set_column(mv,avg);
    evec_matrix.set_column(0,avg);
    MatrixType tempm=matrixOps->GetCovMatEigenvectors(matrix);
    for (unsigned int i=0; i<n_evecs;i++) {
      /** the GetCovMatEigenvector function normalizes the matrix & computes the covariance matrix internally */
      //      VectorType evec=matrixOps->GetCovMatEigenvector(matrix,i);
      evec_matrix.set_column(i+1,tempm.get_column(i));
    }

    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    std::string num=std::string("evecs_for_label")+to_string<unsigned long>(mv);
    std::string eoutname=outname+num+ext;
    writer->SetFileName( eoutname );
    writer->SetInput( &evec_matrix );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
  }
  else Rcpp::Rcout << " can only write out csv files " << std::endl;

  }// end loop over mv variable

  {
    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double,1,1> WriterType;
    WriterType::Pointer writer = WriterType::New();
    std::string num=std::string("avg_for_labels");
    std::string eoutname=outname+num+ext;
    writer->SetFileName( eoutname );
    writer->SetInput( &avg_matrix );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
  }

  return 0;

}



template <unsigned int ImageDimension>
int ConvertImageToFile(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2="";
  if (argc > argct) fn2=std::string(argv[argct]); argct++;

  typename ImageType::Pointer image = NULL;
  ReadImage<ImageType>(image, fn1.c_str());
  typename ImageType::Pointer mask = NULL;
  if (fn2.length() > 3) ReadImage<ImageType>(mask, fn2.c_str());

  Rcpp::Rcout <<" read Image" << fn1 << " mask? " << fn2 << std::endl;
  std::ofstream logfile;
  logfile.open(outname.c_str() );
  if (logfile.good())
    {
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator vfIter( image,  image->GetLargestPossibleRegion() );
  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      bool getval=true;
      if ( mask->GetPixel(vfIter.GetIndex()) < 0.5) getval=false;
      if (getval)
	{
	  float val=image->GetPixel(vfIter.GetIndex());
	  logfile << " " << val ;
	}
    }
  logfile << std::endl;
    }
  logfile.close();


  return 0;

}


template <unsigned int ImageDimension>
int CorrelationUpdate(      int argc, char *argv[])
{

  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<unsigned char,ImageDimension> ByteImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef  typename ImageType::IndexType IndexType;
  typedef  typename ImageType::SizeType SizeType;
  typedef  typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>  InterpolatorType2;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  int argct=2;
  std::string outname=std::string(argv[argct]); argct++;
  std::string operation = std::string(argv[argct]);  argct++;
  std::string fn1 = std::string(argv[argct]);   argct++;
  std::string fn2 ="";
  if (argc > argct) { fn2=std::string(argv[argct]); argct++;
  }else { Rcpp::Rcout <<" Not enough inputs "<< std::endl;  return 1; }
  unsigned int radius=2;
  if (argc > argct) radius=atoi(argv[argct]); argct++;

  typename ImageType::Pointer image1 = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());
  typename ImageType::Pointer imageout = NULL;
  ReadImage<ImageType>(imageout, fn1.c_str());
  imageout->FillBuffer(0);
  typename ImageType::Pointer image2 = NULL;
  ReadImage<ImageType>(image2, fn2.c_str());

  typedef itk::NeighborhoodIterator<ImageType>  iteratorType;
  typename iteratorType::RadiusType rad;
  for (unsigned int j=0; j<ImageDimension; j++) rad[j]=radius;
  iteratorType GHood(rad, image1,image1->GetLargestPossibleRegion());
  float Gsz=(float)GHood.Size();
  GHood.GoToBegin();
  while (!GHood.IsAtEnd())
    {
      typename ImageType::IndexType ind = GHood.GetIndex();
      bool isinside=true;
      for (unsigned int j=0; j<ImageDimension; j++)
	{
	  float shifted=ind[j];
	  if (shifted < (radius+1) || shifted >  image1->GetLargestPossibleRegion().GetSize()[j]-radius-1  ) isinside=false;
	}
      if (isinside) {
	if ( image1->GetPixel(ind) > 0 || image2->GetPixel(ind) > 0) {
      typename ImageType::IndexType ind2;
      // compute mean difference
      float diff=0.0;
      for (unsigned int i = 0; i < GHood.Size(); i++)
	{
	  ind2=GHood.GetIndex(i);
	  diff+=(image1->GetPixel(ind2)-image2->GetPixel(ind2));
	}
      diff/=Gsz;
      float upd=(image1->GetPixel(ind)-image2->GetPixel(ind))-diff;
      imageout->SetPixel(ind,upd);
      }
      }
      ++GHood;
    }

  WriteImage<ImageType>(imageout,outname.c_str());

  return 0;

}





RcppExport SEXP ImageMath( SEXP r_args )
try
{
  // put the arguments coming from R into standard (argc,argv) format;
  // arguments coming from R don't have the command name as first, argument, so add it manually;
  // arguments coming from R may have adjacent arguments concatenated into one argument, 
  // which the parser should handle
  std::deque<std::string> args = Rcpp::as< std::deque<std::string> >( r_args ) ;
  args.push_front( "antsRegistration" ) ;
  
  int argc = args.size() ;
  char** argv = new char*[args.size()+1] ;
  for( int i = 0 ; i < args.size() ; ++i )
    {
      // allocate space for the string plus a null character
      argv[i] = new char[args[i].length()+1] ;
      std::strncpy( argv[i] , args[i].c_str() , args[i].length() ) ;
      // place the null character in the end
      argv[i][args[i].length()] = '\0' ;
    }
  argv[argc] = 0 ;


  if ( argc < 5 )
  {
    Rcpp::Rcout << "\nUsage: " << argv[0] << " ImageDimension <OutputImage.ext> [operations and inputs] <Image1.ext> <Image2.ext>" << std::endl;

    Rcpp::Rcout << "\nUsage Information " << std::endl;
    Rcpp::Rcout << " ImageDimension: 2 or 3 (for 2 or 3 dimensional operations)." << std::endl;
    Rcpp::Rcout << " ImageDimension: 4 (for operations on 4D file, e.g. time-series data)." << std::endl;
    Rcpp::Rcout << " Operator: See list of valid operators below." << std::endl;
    Rcpp::Rcout << " The last two arguments can be an image or float value " << std::endl;
    Rcpp::Rcout << " NB: Some options output text files" << std::endl;

    Rcpp::Rcout << "\nMathematical Operations:" << std::endl;
    Rcpp::Rcout << "  m			: Multiply" << std::endl;
    Rcpp::Rcout << "  + 			: Add" << std::endl;
    Rcpp::Rcout << "  - 			: Subtract" << std::endl;
    Rcpp::Rcout << "  / 			: Divide" << std::endl;
    Rcpp::Rcout << "  ^			: Power" << std::endl;
    Rcpp::Rcout << "  exp			: Take exponent exp(imagevalue*value)" << std::endl;
    Rcpp::Rcout << "  addtozero		: add image-b to image-a only over points where image-a has zero values" << std::endl;
    Rcpp::Rcout << "  overadd		: replace image-a pixel with image-b pixel if image-b pixel is non-zero" << std::endl;
    Rcpp::Rcout << "  abs			: absolute value " << std::endl;
    Rcpp::Rcout << "  total			: Sums up values in an image or in image1*image2 (img2 is the probability mask)" << std::endl;
    Rcpp::Rcout << "  Decision		: Computes result=1./(1.+exp(-1.0*( pix1-0.25)/pix2))" << std::endl;
    Rcpp::Rcout << "  Neg			: Produce image negative" << std::endl;

    Rcpp::Rcout << "\nSpatial Filtering:" << std::endl;
    Rcpp::Rcout << "  G Image1.ext s	: Smooth with Gaussian of sigma = s" << std::endl;
    Rcpp::Rcout << "  MD Image1.ext s	: Morphological Dilation with radius s" << std::endl;
    Rcpp::Rcout << "  ME Image1.ext s	: Morphological Erosion with radius s" << std::endl;
    Rcpp::Rcout << "  MO Image1.ext s	: Morphological Opening with radius s" << std::endl;
    Rcpp::Rcout << "  MC Image1.ext s	: Morphological Closing with radius s" << std::endl;
    Rcpp::Rcout << "  GD Image1.ext s	: Grayscale Dilation with radius s" << std::endl;
    Rcpp::Rcout << "  GE Image1.ext s	: Grayscale Erosion with radius s" << std::endl;
    Rcpp::Rcout << "  GO Image1.ext s	: Grayscale Opening with radius s" << std::endl;
    Rcpp::Rcout << "  GC Image1.ext s	: Grayscale Closing with radius s" << std::endl;

    Rcpp::Rcout << "\nTime Series Operations:" << std::endl;
    Rcpp::Rcout << " CompCorrAuto : Outputs a csv file containing global signal vector and N comp-corr eigenvectors determined from PCA of the high-variance voxels.  Also outputs a comp-corr + global signal corrected 4D image as well as a 3D image measuring the time series variance.  Requires a label image with label 1 identifying voxels in the brain." << std::endl;
    Rcpp::Rcout << "    Usage		: CompCorr 4D_TimeSeries.nii.gz LabeLimage.nii.gz  N-comp-corr-eigenvectors " << std::endl;
    Rcpp::Rcout << " CompCorr : Outputs a comp-corr corrected 4D image as well as a 3D image measuring the correlation of a time series voxel/region with a reference voxel/region factored out.  Requires a label image with 1=overall region of interest,  2=reference voxel, 3=region to factor out.  If there is no 3rd label, then only the global signal is factored out." << std::endl;
    Rcpp::Rcout << "    Usage		: CompCorr 4D_TimeSeries.nii.gz LabeLimage.nii.gz  Sigma-for-temporal-smoothing " << std::endl;
    Rcpp::Rcout << " TimeSeriesSubset : Outputs n 3D image sub-volumes extracted uniformly from the input time-series 4D image." << std::endl;
    Rcpp::Rcout << "    Usage		: TimeSeriesSubset 4D_TimeSeries.nii.gz n " << std::endl;
    Rcpp::Rcout << " TimeSeriesToMatrix : Converts a 4D image + mask to matrix (stored as csv file) where rows are time and columns are space ." << std::endl;
    Rcpp::Rcout << "    Usage		: TimeSeriesToMatrix 4D_TimeSeries.nii.gz mask " << std::endl;

    Rcpp::Rcout << " ComputeTimeSeriesLeverage : Outputs a csv file that identifies the raw leverage and normalized leverage for each time point in the 4D image.  leverage, here, is the difference of the time-point image from the average of the n images.  the normalized leverage is =  average( sum_k abs(Leverage(t)-Leverage(k)) )/Leverage(t). " << std::endl;
    Rcpp::Rcout << "    Usage		: ComputeTimeSeriesLeverage 4D_TimeSeries.nii.gz k_neighbors " << std::endl;

    Rcpp::Rcout << "\nTensor Operations:" << std::endl;
    Rcpp::Rcout << "  4DTensorTo3DTensor	: Outputs a 3D_DT_Image with the same information. " << std::endl;
    Rcpp::Rcout << "    Usage		: 4DTensorTo3DTensor 4D_DTImage.ext" << std::endl;
    Rcpp::Rcout << "  ComponentTo3DTensor	: Outputs a 3D_DT_Image with the same information as component images. " << std::endl;
    Rcpp::Rcout << "    Usage		: ComponentTo3DTensor component_image_prefix[xx,xy,xz,yy,yz,zz] extension" << std::endl;
    Rcpp::Rcout << "  ExtractComponentFrom3DTensor	: Outputs a component images. " << std::endl;
    Rcpp::Rcout << "    Usage		: ExtractComponentFrom3DTensor dtImage.ext which={xx,xy,xz,yy,yz,zz}" << std::endl;
    Rcpp::Rcout << "  ExtractVectorComponent: Produces the WhichVec component of the vector " << std::endl;
    Rcpp::Rcout << "    Usage		: ExtractVectorComponent VecImage WhichVec" << std::endl;
    Rcpp::Rcout << "  TensorColor		: Produces RGB values identifying principal directions " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorColor DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorFA		: " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorFA DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorFADenominator	: " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorFADenominator DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorFANumerator	: " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorFANumerator DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorIOTest	: Will write the DT image back out ... tests I/O processes for consistency. " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorIOTest DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorMeanDiffusion	: " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorMeanDiffusion DTImage.ext" << std::endl;
    Rcpp::Rcout << "  TensorToVector	: Produces vector field identifying one of the principal directions, 2 = largest eigenvalue" << std::endl;
    Rcpp::Rcout << "    Usage		: TensorToVector DTImage.ext WhichVec" << std::endl;
    Rcpp::Rcout << "  TensorToVectorComponent: 0 => 2 produces component of the principal vector field (largest eigenvalue). 3 = 8 => gets values from the tensor " << std::endl;
    Rcpp::Rcout << "    Usage		: TensorToVectorComponent DTImage.ext WhichVec" << std::endl;

    Rcpp::Rcout << "\nUnclassified Operators:" << std::endl;

    Rcpp::Rcout << "  Byte			: Convert to Byte image in [0,255]" << std::endl;

    Rcpp::Rcout << "\n  CompareHeadersAndImages: Tries to find and fix header errors. Outputs a repaired image with new header. " << std::endl;
    Rcpp::Rcout << "  			  Never use this if you trust your header information. " << std::endl;
    Rcpp::Rcout << "      Usage		: CompareHeadersAndImages Image1 Image2" << std::endl;

    Rcpp::Rcout << "\n  ConvertImageSetToMatrix: Each row/column contains image content extracted from mask applied to images in *img.nii " << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertImageSetToMatrix rowcoloption Mask.nii *images.nii" << std::endl;
    Rcpp::Rcout << " ConvertImageSetToMatrix output can be an image type or csv file type." <<std::endl;

    Rcpp::Rcout << "\n  RandomlySampleImageSetToCSV: N random samples are selected from each image in a list " << std::endl;
    Rcpp::Rcout << "      Usage		: RandomlySampleImageSetToCSV N_samples *images.nii" << std::endl;
    Rcpp::Rcout << " RandomlySampleImageSetToCSV outputs a csv file type." <<std::endl;

    Rcpp::Rcout << "\n  ConvertImageSetToEigenvectors: Each row/column contains image content extracted from mask applied to images in *img.nii " << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertImageSetToEigenvectors N_Evecs Mask.nii *images.nii" << std::endl;
    Rcpp::Rcout << " ConvertImageSetToEigenvectors output will be a csv file for each label value > 0 in the mask." <<std::endl;


    Rcpp::Rcout << "\n  ConvertImageToFile	: Writes voxel values to a file  " << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertImageToFile imagevalues.nii {Optional-ImageMask.nii}" << std::endl;

    Rcpp::Rcout << "\n  ConvertLandmarkFile	: Converts landmark file between formats. See ANTS.pdf for description of formats."  << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertLandmarkFile InFile.txt" << std::endl;
    Rcpp::Rcout << "      Example 1		: ImageMath 3  outfile.vtk  ConvertLandmarkFile  infile.txt" << std::endl;

    Rcpp::Rcout << "\n  ConvertToGaussian	: " << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertToGaussian  TValueImage  sigma-float" << std::endl;

    Rcpp::Rcout << "\n  ConvertVectorToImage	: The vector contains image content extracted from a mask. Here the vector is returned to its spatial origins as image content " << std::endl;
    Rcpp::Rcout << "      Usage		: ConvertVectorToImage Mask.nii vector.nii" << std::endl;

    Rcpp::Rcout << "\n  CorrelationUpdate	: In voxels, compute update that makes Image2 more like Image1." << std::endl;
    Rcpp::Rcout << "      Usage		: CorrelationUpdate Image1.ext Image2.ext RegionRadius" << std::endl;

    Rcpp::Rcout << "\n  CountVoxelDifference	: The where function from IDL " << std::endl;
    Rcpp::Rcout << "      Usage		: CountVoxelDifference Image1 Image2 Mask" << std::endl;

    Rcpp::Rcout << "\n  CorruptImage		: " << std::endl;
    Rcpp::Rcout << "      Usage		: CorruptImage Image NoiseLevel Smoothing" << std::endl;

    Rcpp::Rcout << "\n  D			: DistanceTransform" << std::endl;

    Rcpp::Rcout << "\n  DiceAndMinDistSum	: Outputs DiceAndMinDistSum and Dice Overlap to text log file + optional distance image" << std::endl;
    Rcpp::Rcout << "      Usage		: DiceAndMinDistSum LabelImage1.ext LabelImage2.ext OptionalDistImage" << std::endl;

    Rcpp::Rcout << "\n  EnumerateLabelInterfaces: " << std::endl;
    Rcpp::Rcout << "      Usage		: EnumerateLabelInterfaces ImageIn ColoredImageOutname NeighborFractionToIgnore" << std::endl;

    Rcpp::Rcout << "\n  ExtractSlice		: Extracts slice number from last dimension of volume (2,3,4) dimensions "  << std::endl;
    Rcpp::Rcout << "      Usage		: ExtractSlice volume.nii.gz slicetoextract" << std::endl;

    Rcpp::Rcout << "\n  FastMarchingSegmentation: final output is the propagated label image. Optional stopping value: higher values allow more distant propagation "  << std::endl;
    Rcpp::Rcout << "      Usage		: FastMarchingSegmentation speed/binaryimagemask.ext initiallabelimage.ext Optional-Stopping-Value" << std::endl;

    Rcpp::Rcout << "\n  FillHoles		: Parameter = ratio of edge at object to edge at background;  --  " << std::endl;
    Rcpp::Rcout << "  			  Parameter = 1 is a definite hole bounded by object only, 0.99 is close" << std::endl;
    Rcpp::Rcout << "  			  Default of parameter > 1 will fill all holes" << std::endl;
    Rcpp::Rcout << "      Usage		: FillHoles Image.ext parameter" << std::endl;

    Rcpp::Rcout << "\n  FitSphere		: " << std::endl;
    Rcpp::Rcout << "      Usage		: FitSphere GM-ImageIn {WM-Image} {MaxRad-Default=5}" << std::endl;

    Rcpp::Rcout << "\n  FlattenImage		: Replaces values greater than %ofMax*Max to the value %ofMax*Max " << std::endl;
    Rcpp::Rcout << "      Usage		: FlattenImage Image %ofMax" << std::endl;

    Rcpp::Rcout << "\n  GetLargestComponent	: Get the largest object in an image" << std::endl;
    Rcpp::Rcout << "      Usage		: GetLargestComponent InputImage {MinObjectSize}" << std::endl;

    Rcpp::Rcout << "\n  Grad			: Gradient magnitude with sigma s (if normalize, then output in range [0, 1])" << std::endl;
    Rcpp::Rcout << "      Usage		: Grad Image.ext s normalize?" << std::endl;

    Rcpp::Rcout << "\n  HistogramMatch	: " << std::endl;
    Rcpp::Rcout << "      Usage		: HistogramMatch SourceImage ReferenceImage {NumberBins-Default=255} {NumberPoints-Default=64}" << std::endl;

    Rcpp::Rcout << "\n  InvId			: computes the inverse-consistency of two deformations and write the inverse consistency error image " << std::endl;
    Rcpp::Rcout << "      Usage		: InvId VectorFieldName VectorFieldName" << std::endl;

    Rcpp::Rcout << "\n  LabelStats		: Compute volumes / masses of objects in a label image. Writes to text file" << std::endl;
    Rcpp::Rcout << "      Usage		: LabelStats labelimage.ext valueimage.nii" << std::endl;

    Rcpp::Rcout << "\n  Laplacian		: Laplacian computed with sigma s (if normalize, then output in range [0, 1])" << std::endl;
    Rcpp::Rcout << "      Usage		: Laplacian Image.ext s normalize?" << std::endl;

    Rcpp::Rcout << "\n  Lipschitz		: Computes the Lipschitz norm of a vector field " << std::endl;
    Rcpp::Rcout << "      Usage		: Lipschitz VectorFieldName" << std::endl;

    Rcpp::Rcout << "\n  MakeImage		: " << std::endl;
    Rcpp::Rcout << "      Usage		: MakeImage SizeX  SizeY {SizeZ};" << std::endl;

    Rcpp::Rcout << "\n  Normalize		: Normalize to [0,1]. Option instead divides by average value" << std::endl;
    Rcpp::Rcout << "      Usage		: Normalize Image.ext opt" << std::endl;

    Rcpp::Rcout << "\n  PadImage		: If Pad-Number is negative, de-Padding occurs" << std::endl;
    Rcpp::Rcout << "      Usage		: PadImage ImageIn Pad-Number" << std::endl;

    Rcpp::Rcout << "\n  CenterImage2inImage1		: " << std::endl;
    Rcpp::Rcout << "      Usage		: ReferenceImageSpace ImageToCenter " << std::endl;

    Rcpp::Rcout << "\n  PH			: Print Header" << std::endl;

    Rcpp::Rcout << "\n  PoissonDiffusion		: Solves Poisson's equation in a designated region using non-zero sources" << std::endl;
    Rcpp::Rcout << "      Usage		: PoissonDiffusion inputImage labelImage [sigma=1.0] [regionLabel=1] [numberOfIterations=500] [convergenceThreshold=1e-10]" << std::endl;

    Rcpp::Rcout << "\n  PropagateLabelsThroughMask: Final output is the propagated label image. Optional stopping value: higher values allow more distant propagation" << std::endl;
    Rcpp::Rcout << "      Usage		: PropagateLabelsThroughMask speed/binaryimagemask.nii.gz initiallabelimage.nii.gz Optional-Stopping-Value" << std::endl;

    Rcpp::Rcout << "\n  PValueImage		: " << std::endl;
    Rcpp::Rcout << "      Usage		: PValueImage TValueImage dof" << std::endl;

    Rcpp::Rcout << "\n  RemoveLabelInterfaces: " << std::endl;
    Rcpp::Rcout << "      Usage		: RemoveLabelInterfaces ImageIn" << std::endl;

    Rcpp::Rcout << "\n  ROIStatistics		: computes anatomical locations, cluster size and mass of a stat image which should be in the same physical space (but not nec same resolution) as the label image." << std::endl;
    Rcpp::Rcout << "      Usage		: ROIStatistics LabelNames.txt labelimage.ext valueimage.nii" << std::endl;


    Rcpp::Rcout << "\n  SetOrGetPixel	: "  << std::endl;
    Rcpp::Rcout << "      Usage		: SetOrGetPixel ImageIn Get/Set-Value IndexX IndexY {IndexZ}" << std::endl;
    Rcpp::Rcout << "      Example 1		: ImageMath 2 outimage.nii SetOrGetPixel Image Get 24 34; Gets the value at 24, 34" << std::endl;
    Rcpp::Rcout << "      Example 2		: ImageMath 2 outimage.nii SetOrGetPixel Image 1.e9 24 34; This sets 1.e9 as the value at 23 34" << std::endl;
    Rcpp::Rcout << "  			  You can also pass a boolean at the end to force the physical space to be used" << std::endl;

    Rcpp::Rcout << "\n  Segment		: Segment an Image  with option of Priors, weight 1 => maximally local/prior-based )" << std::endl;
    Rcpp::Rcout << "      Usage		: Segment Image1.ext N-Classes LocalityVsGlobalityWeight-In-ZeroToOneRange OptionalPriorImages" << std::endl;

    Rcpp::Rcout << "\n  stack			: Will put 2 images in the same volume" << std::endl;
    Rcpp::Rcout << "      Usage		: Stack Image1.ext Image2.ext" << std::endl;

    Rcpp::Rcout << "\n  ThresholdAtMean	: See the code" << std::endl;
    Rcpp::Rcout << "      Usage		: ThresholdAtMean Image %ofMean" << std::endl;

    Rcpp::Rcout << "\n  TileImages	: " << std::endl;
    Rcpp::Rcout << "      Usage		: TileImages NumColumns ImageList*" << std::endl;

    Rcpp::Rcout << "\n  TriPlanarView	: " << std::endl;
    Rcpp::Rcout << "      Usage		: TriPlanarView  ImageIn.nii.gz PercentageToClampLowIntensity PercentageToClampHiIntensity x-slice y-slice z-slice" << std::endl;

    Rcpp::Rcout << "\n  TruncateImageIntensity: " << std::endl;
    Rcpp::Rcout << "      Usage		: TruncateImageIntensity InputImage.ext {lowerQuantile=0.05} {upperQuantile=0.95} {numberOfBins=65} {binary-maskImage}" << std::endl;

    Rcpp::Rcout << "\n  Where			: The where function from IDL" << std::endl;
    Rcpp::Rcout << "      Usage		: Where Image ValueToLookFor maskImage-option tolerance" << std::endl;

    return Rcpp::wrap( EXIT_FAILURE ) ;  
  }

  std::string operation=std::string(argv[3]);

  switch ( atoi(argv[1]) )
   {
   case 2:
     if (strcmp(operation.c_str(),"m") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"mresample") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"+") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"-") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"/") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"^") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"exp") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"abs") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"addtozero") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"overadd") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"total") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Decision") == 0)  ImageMath<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Neg") == 0)  NegativeImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"G") == 0)  SmoothImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"MD") == 0 || strcmp(operation.c_str(),"ME") == 0)  MorphImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"MO") == 0 || strcmp(operation.c_str(),"MC") == 0)  MorphImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"GD") == 0 || strcmp(operation.c_str(),"GE") == 0)  MorphImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"GO") == 0 || strcmp(operation.c_str(),"GC") == 0)  MorphImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"D") == 0 )  DistanceMap<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Normalize") == 0 )  NormalizeImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Grad") == 0 )  GradientImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Laplacian") == 0 )  LaplacianImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"PH") == 0 )  PrintHeader<2>(argc,argv);
     else if (strcmp(operation.c_str(),"CenterImage2inImage1") == 0 )  CenterImage2inImage1<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Byte") == 0 )  ByteImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"LabelStats") == 0 )  LabelStats<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ROIStatistics") == 0 )  ROIStatistics<2>(argc,argv);
     else if (strcmp(operation.c_str(),"DiceAndMinDistSum") == 0 )  DiceAndMinDistSum<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Lipschitz") == 0 )  Lipschitz<2>(argc,argv);
     else if (strcmp(operation.c_str(),"InvId") == 0 )  InvId<2>(argc,argv);
     else if (strcmp(operation.c_str(),"GetLargestComponent") == 0 )  GetLargestComponent<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractVectorComponent") == 0 )  ExtractVectorComponent<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ThresholdAtMean") == 0 )  ThresholdAtMean<2>(argc,argv);
     else if (strcmp(operation.c_str(),"FlattenImage") == 0 )  FlattenImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"CorruptImage") == 0 )  CorruptImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"TileImages") == 0 )  TileImages<2>(argc,argv);
     else if (strcmp(operation.c_str(),"Where") == 0 )  Where<2>(argc,argv);
     else if (strcmp(operation.c_str(),"FillHoles") == 0 )  FillHoles<2>(argc,argv);
     else if (strcmp(operation.c_str(),"HistogramMatch") == 0) HistogramMatching<2>(argc,argv);
     else if (strcmp(operation.c_str(),"PadImage") == 0 )  PadImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"SetOrGetPixel") == 0 )  SetOrGetPixel<2>(argc,argv);
     else if (strcmp(operation.c_str(),"MakeImage") == 0 )  MakeImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"stack") == 0 )  StackImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"CompareHeadersAndImages") == 0 )  CompareHeadersAndImages<2>(argc,argv);
     else if (strcmp(operation.c_str(),"CountVoxelDifference") == 0 )  CountVoxelDifference<2>(argc,argv);
     //     else if (strcmp(operation.c_str(),"AddToZero") == 0 )  AddToZero<2>(argc,argv);
     else if (strcmp(operation.c_str(),"RemoveLabelInterfaces") == 0 )  RemoveLabelInterfaces<2>(argc,argv);
     else if (strcmp(operation.c_str(),"PoissonDiffusion") == 0 )  PoissonDiffusion<2>(argc,argv);
     else if (strcmp(operation.c_str(),"EnumerateLabelInterfaces") == 0 )  EnumerateLabelInterfaces<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageToFile") == 0 )  ConvertImageToFile<2>(argc,argv);
     else if (strcmp(operation.c_str(),"PValueImage") == 0 )  PValueImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"CorrelationUpdate") == 0 )  CorrelationUpdate<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToMatrix") == 0 )  ConvertImageSetToMatrix<2>(argc,argv);
     else if (strcmp(operation.c_str(),"RandomlySampleImageSetToCSV") == 0 )  RandomlySampleImageSetToCSV<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToEigenvectors") == 0 )  ConvertImageSetToEigenvectors<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertVectorToImage") == 0 )  ConvertVectorToImage<2>(argc,argv);
     else if (strcmp(operation.c_str(),"PropagateLabelsThroughMask") == 0 )  PropagateLabelsThroughMask<2>(argc,argv);
     else if (strcmp(operation.c_str(),"FastMarchingSegmentation") == 0 )  FastMarchingSegmentation<2>(argc,argv);
     else if (strcmp(operation.c_str(),"TruncateImageIntensity") == 0 ) TruncateImageIntensity<2>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractSlice") == 0)  ExtractSlice<2>(argc,argv);
     //     else if (strcmp(operation.c_str(),"ConvertLandmarkFile") == 0)  ConvertLandmarkFile<2>(argc,argv);
     else Rcpp::Rcout << " cannot find operation : " << operation << std::endl;
     break;
   case 3:
     if (strcmp(operation.c_str(),"m") == 0)  ImageMath<3>(argc,argv);

     else if (strcmp(operation.c_str(),"mresample") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"+") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"-") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"/") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"^") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"exp") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"abs") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"addtozero") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"overadd") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"total") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Decision") == 0)  ImageMath<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Neg") == 0)  NegativeImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"G") == 0)  SmoothImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"MD") == 0 || strcmp(operation.c_str(),"ME") == 0)  MorphImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"MO") == 0 || strcmp(operation.c_str(),"MC") == 0)  MorphImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"GD") == 0 || strcmp(operation.c_str(),"GE") == 0)  MorphImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"GO") == 0 || strcmp(operation.c_str(),"GC") == 0)  MorphImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"D") == 0 )  DistanceMap<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Normalize") == 0 )  NormalizeImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Grad") == 0 )  GradientImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Laplacian") == 0 )  LaplacianImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"PH") == 0 )  PrintHeader<3>(argc,argv);
     else if (strcmp(operation.c_str(),"CenterImage2inImage1") == 0 )  CenterImage2inImage1<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Byte") == 0 )  ByteImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"LabelStats") == 0 )  LabelStats<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ROIStatistics") == 0 )  ROIStatistics<3>(argc,argv);
     else if (strcmp(operation.c_str(),"DiceAndMinDistSum") == 0 )  DiceAndMinDistSum<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Lipschitz") == 0 )  Lipschitz<3>(argc,argv);
     else if (strcmp(operation.c_str(),"InvId") == 0 )  InvId<3>(argc,argv);
     else if (strcmp(operation.c_str(),"GetLargestComponent") == 0 )  GetLargestComponent<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractVectorComponent") == 0 )  ExtractVectorComponent<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ThresholdAtMean") == 0 )  ThresholdAtMean<3>(argc,argv);
     else if (strcmp(operation.c_str(),"FlattenImage") == 0 )  FlattenImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"CorruptImage") == 0 )  CorruptImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"RemoveLabelInterfaces") == 0 )  RemoveLabelInterfaces<3>(argc,argv);
     else if (strcmp(operation.c_str(),"EnumerateLabelInterfaces") == 0 )  EnumerateLabelInterfaces<3>(argc,argv);
     else if (strcmp(operation.c_str(),"FitSphere") == 0 )  FitSphere<3>(argc,argv);
     else if (strcmp(operation.c_str(),"Where") == 0 )  Where<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorFA") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorFANumerator") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorFADenominator") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"4DTensorTo3DTensor") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ComponentTo3DTensor") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractComponentFrom3DTensor") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorIOTest") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorMeanDiffusion") == 0 )  TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorColor") == 0) TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorToVector") == 0) TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorToPhysicalSpace") == 0) TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorToLocalSpace") == 0) TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TensorToVectorComponent") == 0) TensorFunctions<3>(argc,argv);
     else if (strcmp(operation.c_str(),"FillHoles") == 0 )  FillHoles<3>(argc,argv);
     else if (strcmp(operation.c_str(),"HistogramMatch") == 0) HistogramMatching<3>(argc,argv);
     else if (strcmp(operation.c_str(),"PadImage") == 0 )  PadImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"SetOrGetPixel") == 0 )  SetOrGetPixel<3>(argc,argv);
     else if (strcmp(operation.c_str(),"MakeImage") == 0 )  MakeImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"stack") == 0 )  StackImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"CompareHeadersAndImages") == 0 )  CompareHeadersAndImages<3>(argc,argv);
     else if (strcmp(operation.c_str(),"CountVoxelDifference") == 0 )  CountVoxelDifference<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageToFile") == 0 )  ConvertImageToFile<3>(argc,argv);
     else if (strcmp(operation.c_str(),"PValueImage") == 0 )  PValueImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"CorrelationUpdate") == 0 )  CorrelationUpdate<3>(argc,argv);
     else if (strcmp(operation.c_str(),"PoissonDiffusion") == 0 )  PoissonDiffusion<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToMatrix") == 0 )  ConvertImageSetToMatrix<3>(argc,argv);
     else if (strcmp(operation.c_str(),"RandomlySampleImageSetToCSV") == 0 )  RandomlySampleImageSetToCSV<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToEigenvectors") == 0 )  ConvertImageSetToEigenvectors<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertVectorToImage") == 0 )  ConvertVectorToImage<3>(argc,argv);
     else if (strcmp(operation.c_str(),"PropagateLabelsThroughMask") == 0 )  PropagateLabelsThroughMask<3>(argc,argv);
     else if (strcmp(operation.c_str(),"FastMarchingSegmentation") == 0 )  FastMarchingSegmentation<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TriPlanarView") == 0 )  TriPlanarView<3>(argc,argv);
     else if (strcmp(operation.c_str(),"TruncateImageIntensity") == 0 ) TruncateImageIntensity<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractSlice") == 0)  ExtractSlice<3>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertLandmarkFile") == 0)  ConvertLandmarkFile<3>(argc,argv);
     else Rcpp::Rcout << " cannot find operation : " << operation << std::endl;
      break;
 case 4:
     if (strcmp(operation.c_str(),"m") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"mresample") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"+") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"-") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"/") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"^") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"exp") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"abs") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"addtozero") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"overadd") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"total") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Decision") == 0)  ImageMath<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Neg") == 0)  NegativeImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"G") == 0)  SmoothImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"MD") == 0 || strcmp(operation.c_str(),"ME") == 0)  MorphImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"MO") == 0 || strcmp(operation.c_str(),"MC") == 0)  MorphImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"GD") == 0 || strcmp(operation.c_str(),"GE") == 0)  MorphImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"GO") == 0 || strcmp(operation.c_str(),"GC") == 0)  MorphImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"D") == 0 )  DistanceMap<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Normalize") == 0 )  NormalizeImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Grad") == 0 )  GradientImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Laplacian") == 0 )  LaplacianImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"PH") == 0 )  PrintHeader<4>(argc,argv);
     else if (strcmp(operation.c_str(),"nvols") == 0 )  PrintHeader<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CenterImage2inImage1") == 0 )  CenterImage2inImage1<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Byte") == 0 )  ByteImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"LabelStats") == 0 )  LabelStats<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ROIStatistics") == 0 )  ROIStatistics<4>(argc,argv);
     else if (strcmp(operation.c_str(),"DiceAndMinDistSum") == 0 )  DiceAndMinDistSum<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Lipschitz") == 0 )  Lipschitz<4>(argc,argv);
     else if (strcmp(operation.c_str(),"InvId") == 0 )  InvId<4>(argc,argv);
     else if (strcmp(operation.c_str(),"GetLargestComponent") == 0 )  GetLargestComponent<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractVectorComponent") == 0 )  ExtractVectorComponent<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ThresholdAtMean") == 0 )  ThresholdAtMean<4>(argc,argv);
     else if (strcmp(operation.c_str(),"FlattenImage") == 0 )  FlattenImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CorruptImage") == 0 )  CorruptImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"RemoveLabelInterfaces") == 0 )  RemoveLabelInterfaces<4>(argc,argv);
     else if (strcmp(operation.c_str(),"EnumerateLabelInterfaces") == 0 )  EnumerateLabelInterfaces<4>(argc,argv);
     else if (strcmp(operation.c_str(),"FitSphere") == 0 )  FitSphere<4>(argc,argv);
     else if (strcmp(operation.c_str(),"Where") == 0 )  Where<4>(argc,argv);
     //    else if (strcmp(operation.c_str(),"TensorFA") == 0 )  TensorFunctions<4>(argc,argv);
     // else if (strcmp(operation.c_str(),"TensorIOTest") == 0 )  TensorFunctions<4>(argc,argv);
     // else if (strcmp(operation.c_str(),"TensorMeanDiffusion") == 0 )  TensorFunctions<4>(argc,argv);
     // else if (strcmp(operation.c_str(),"TensorColor") == 0) TensorFunctions<4>(argc,argv);
     // else if (strcmp(operation.c_str(),"TensorToVector") == 0) TensorFunctions<4>(argc,argv);
     // else if (strcmp(operation.c_str(),"TensorToVectorComponent") == 0) TensorFunctions<4>(argc,argv);
     else if (strcmp(operation.c_str(),"FillHoles") == 0 )  FillHoles<4>(argc,argv);
     else if (strcmp(operation.c_str(),"HistogramMatch") == 0) HistogramMatching<4>(argc,argv);
     else if (strcmp(operation.c_str(),"PadImage") == 0 )  PadImage<4>(argc,argv);
   //  else if (strcmp(operation.c_str(),"SetOrGetPixel") == 0 )  SetOrGetPixel<4>(argc,argv);
     else if (strcmp(operation.c_str(),"MakeImage") == 0 )  MakeImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"stack") == 0 )  StackImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CompareHeadersAndImages") == 0 )  CompareHeadersAndImages<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CountVoxelDifference") == 0 )  CountVoxelDifference<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageToFile") == 0 )  ConvertImageToFile<4>(argc,argv);
     else if (strcmp(operation.c_str(),"PValueImage") == 0 )  PValueImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"PoissonDiffusion") == 0 )  PoissonDiffusion<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CorrelationUpdate") == 0 )  CorrelationUpdate<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToMatrix") == 0 )  ConvertImageSetToMatrix<4>(argc,argv);
     else if (strcmp(operation.c_str(),"RandomlySampleImageSetToCSV") == 0 )  RandomlySampleImageSetToCSV<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertImageSetToEigenvectors") == 0 )  ConvertImageSetToEigenvectors<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertVectorToImage") == 0 )  ConvertVectorToImage<4>(argc,argv);
     else if (strcmp(operation.c_str(),"PropagateLabelsThroughMask") == 0 )  PropagateLabelsThroughMask<4>(argc,argv);
     else if (strcmp(operation.c_str(),"FastMarchingSegmentation") == 0 )  FastMarchingSegmentation<4>(argc,argv);
     else if (strcmp(operation.c_str(),"TriPlanarView") == 0 )  TriPlanarView<4>(argc,argv);
     else if (strcmp(operation.c_str(),"TruncateImageIntensity") == 0 ) TruncateImageIntensity<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ExtractSlice") == 0)  ExtractSlice<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ConvertLandmarkFile") == 0)  ConvertLandmarkFile<4>(argc,argv);
     else if (strcmp(operation.c_str(),"TimeSeriesSubset") == 0)  TimeSeriesSubset<4>(argc,argv);
     else if (strcmp(operation.c_str(),"TimeSeriesToMatrix") == 0)  TimeSeriesToMatrix<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CompCorr") == 0)  CompCorr<4>(argc,argv);
     else if (strcmp(operation.c_str(),"CompCorrAuto") == 0)  CompCorrAuto<4>(argc,argv);
     else if (strcmp(operation.c_str(),"ComputeTimeSeriesLeverage") == 0)  ComputeTimeSeriesLeverage<4>(argc,argv);
     else Rcpp::Rcout << " cannot find operation : " << operation << std::endl;
      break;

   default:
     Rcpp::Rcout << " Dimension Not supported " << atoi(argv[1]) << std::endl;
     return Rcpp::wrap( EXIT_FAILURE );
   }

  // cleanup of argv
  for( int i = 0 ; i < args.size() ; ++i )
    {
      delete[] argv[i] ;
    }
  delete[] argv ;

  return Rcpp::wrap( EXIT_SUCCESS ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( EXIT_FAILURE ) ;
   }
