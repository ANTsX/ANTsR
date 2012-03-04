/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ResampleImageBySpacing.cxx,v $
  Language:  C++
  Date:      $Date: 2009/03/31 21:22:00 $
  Version:   $Revision: 1.17 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or 
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include <Rcpp.h>

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"

#include "itkDiscreteGaussianImageFilter.h"

template <class TImage>
typename TImage::Pointer
SmoothImage(typename TImage::Pointer image, float sig)
{
  typedef itk::DiscreteGaussianImageFilter<TImage, TImage> dgf;
  typename dgf::Pointer filter = dgf::New();
  filter->SetVariance(sig);
  filter->SetUseImageSpacingOn();
  filter->SetMaximumError(.01f);
  filter->SetInput(image);
  filter->Update();
  return filter->GetOutput();
}



RcppExport SEXP ResampleImageBySpacing( SEXP r_args )
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


  if( argc < 5 )
    {
    Rcpp::Rcout << "Usage: " << std::endl;
    Rcpp::Rcout << argv[0] << "  ImageDimension inputImageFile  outputImageFile outxspc outyspc {outzspacing}  {dosmooth?}  {addvox} {nn-interp?}" << std::endl; 
    Rcpp::Rcout <<" addvox pads each dimension by addvox " << std::endl;
    Rcpp::Rcout << "  " << std::endl;
//    Rcpp::Rcout << " interp 0 = linear, 1 = nn " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  unsigned int    Dimension = atoi(argv[1]);

  if (Dimension == 2) 
    {

  typedef   float  InputPixelType;
  typedef   float           InternalPixelType;
  typedef   float   OutputPixelType;

  typedef itk::Image< InputPixelType,    2 >   InputImageType;
  typedef itk::Image< InternalPixelType, 2 >   InternalImageType;
  typedef itk::Image< OutputPixelType,   2 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[2] );
  writer->SetFileName( argv[3] );

  try 
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception caught!" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }


  InputImageType::ConstPointer inputImage = reader->GetOutput();

  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();

  OutputImageType::SpacingType spacing;
  for (int i=0; i<2; i++) spacing[i]=inputSpacing[i];

  Rcpp::Rcout <<  " spacing " << spacing << " dim " << 2 << std::endl;

  bool dosmooth=1;
  if (argc > 4) spacing[0] = atof(argv[4]);
  if (argc > 5) spacing[1] = atof(argv[5]);
  if (argc > 6) dosmooth = atoi(argv[6]);
  int addvox=0;
  if (argc > 7) addvox = atoi(argv[7]);
  bool nn=false;
  if (argc > 8) nn = atoi(argv[7]);

  Rcpp::Rcout <<  " spacing2 " << spacing << std::endl;

  InternalImageType::ConstPointer smoothedImage = reader->GetOutput();
  if( dosmooth )
  {
  for (int sm=0; sm<2; sm++)
  {
  typedef itk::RecursiveGaussianImageFilter< 
                                  OutputImageType,
                                  OutputImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  smootherX->SetInput( smoothedImage );
  float sig = 0; 
  sig = atof(argv[4+sm])/inputSpacing[sm]-1.0;
  Rcpp::Rcout << " smoothing by : " << sig << " dir " << sm << std::endl;
  smootherX->SetSigma( sig );
  smootherX->SetDirection( sm );
  smootherX->SetNormalizeAcrossScale( false );
  if (sig > 0 && dosmooth)
  {
  try 
    {
    smootherX->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception catched !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }
    smoothedImage = smootherX->GetOutput();
  }
  }
  }


 // InternalImageType::ConstPointer smoothedImage = smootherY->GetOutput();

 

 // InternalImageType::ConstPointer smoothedImage = reader->GetOutput();
  //smoothedImage =SmoothImage<ImageType>(reader->GetOutput() , );

  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  typedef itk::IdentityTransform< double, 2 >  TransformType;

  typedef itk::LinearInterpolateImageFunction< 
    InternalImageType, double >  InterpolatorType;
  typedef itk::NearestNeighborInterpolateImageFunction< 
    InternalImageType, double >  InterpolatorType2;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  InterpolatorType2::Pointer interpolator2 = InterpolatorType2::New();

  resampler->SetInterpolator( interpolator );
  if (nn == 1 ) resampler->SetInterpolator( interpolator2 );

  InternalImageType::IndexType ind;
  ind.Fill(1);
  resampler->SetDefaultPixelValue( inputImage->GetPixel(ind) ); // zero regions without source

  Rcpp::Rcout << " out space " << spacing << std::endl;
  resampler->SetOutputSpacing( spacing );
  // Use the same origin
  resampler->SetOutputOrigin( inputImage->GetOrigin() );
  // Use the same origin
  resampler->SetOutputDirection( inputImage->GetDirection() );


  InputImageType::SizeType   inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  typedef InputImageType::SizeType::SizeValueType SizeValueType;
  InputImageType::SizeType   size;

  for (int i=0; i<2; i++)
    size[i] = static_cast<SizeValueType>(inputSize[i] * inputSpacing[i] / spacing[i]+addvox);

  Rcpp::Rcout << " output size " << size << " spc " << spacing << std::endl;
  resampler->SetSize( size );

  resampler->SetInput( smoothedImage );

  writer->SetInput( resampler->GetOutput() );

  TransformType::Pointer transform = TransformType::New();

  transform->SetIdentity();

  resampler->SetTransform( transform );

  
  try 
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception catched !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }


    }


  if (Dimension == 3) 
    {

  typedef   float  InputPixelType;
  typedef   float           InternalPixelType;
  typedef   float   OutputPixelType;

  typedef itk::Image< InputPixelType,    3 >   InputImageType;
  typedef itk::Image< InternalPixelType, 3 >   InternalImageType;
  typedef itk::Image< OutputPixelType,   3 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[2] );
  writer->SetFileName( argv[3] );

  try 
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception caught!" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }


  InputImageType::ConstPointer inputImage = reader->GetOutput();

  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();

  OutputImageType::SpacingType spacing;
  for (int i=0; i<3; i++) spacing[i]=inputSpacing[i];

  Rcpp::Rcout <<  " spacing " << spacing << " dim " << 3 << std::endl;

  bool dosmooth=1;
  if (argc > 4) spacing[0] = atof(argv[4]);
  if (argc > 5) spacing[1] = atof(argv[5]);
  if (argc > 6) spacing[2] = atof(argv[6]);
  if (argc > 7) dosmooth = atoi(argv[7]);
  int addvox=0;
  if (argc > 8) addvox = atoi(argv[8]);
  bool nn=false;
  if (argc > 9) nn = atoi(argv[9]);

  Rcpp::Rcout <<  " spacing2 " << spacing << std::endl;

  InternalImageType::ConstPointer smoothedImage = reader->GetOutput();
  if( dosmooth )
  {
  for (int sm=0; sm<3; sm++)
  {
  typedef itk::RecursiveGaussianImageFilter< 
                                  OutputImageType,
                                  OutputImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  smootherX->SetInput( smoothedImage );
  float sig = 0; 
  sig = atof(argv[4+sm])/inputSpacing[sm]-1.0;
  Rcpp::Rcout << " smoothing by : " << sig << " dir " << sm << std::endl;
  smootherX->SetSigma( sig );
  smootherX->SetDirection( sm );
  smootherX->SetNormalizeAcrossScale( false );
  if (sig > 0 && dosmooth)
  {
  try 
    {
    smootherX->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception catched !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }
    smoothedImage = smootherX->GetOutput();
  }
  }
  }


 // InternalImageType::ConstPointer smoothedImage = smootherY->GetOutput();

 

 // InternalImageType::ConstPointer smoothedImage = reader->GetOutput();
  //smoothedImage =SmoothImage<ImageType>(reader->GetOutput() , );

  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  typedef itk::IdentityTransform< double, 3 >  TransformType;

  typedef itk::LinearInterpolateImageFunction< 
    InternalImageType, double >  InterpolatorType;
  typedef itk::NearestNeighborInterpolateImageFunction< 
    InternalImageType, double >  InterpolatorType2;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  InterpolatorType2::Pointer interpolator2 = InterpolatorType2::New();

  resampler->SetInterpolator( interpolator );
  if (nn  == 1 ) resampler->SetInterpolator( interpolator2 );

  InternalImageType::IndexType ind;
  ind.Fill(1);
  resampler->SetDefaultPixelValue( inputImage->GetPixel(ind) ); // zero regions without source

  Rcpp::Rcout << " out space " << spacing << std::endl;
  resampler->SetOutputSpacing( spacing );
  // Use the same origin
  resampler->SetOutputOrigin( inputImage->GetOrigin() );
  // Use the same origin
  resampler->SetOutputDirection( inputImage->GetDirection() );


  InputImageType::SizeType   inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  typedef InputImageType::SizeType::SizeValueType SizeValueType;
  InputImageType::SizeType   size;

  for (int i=0; i<3; i++)
    size[i] = static_cast<SizeValueType>(inputSize[i] * inputSpacing[i] / spacing[i]+addvox);

  Rcpp::Rcout << " output size " << size << " spc " << spacing << std::endl;
  resampler->SetSize( size );

  resampler->SetInput( smoothedImage );

  writer->SetInput( resampler->GetOutput() );

  TransformType::Pointer transform = TransformType::New();

  transform->SetIdentity();

  resampler->SetTransform( transform );

  
  try 
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    Rcpp::Rcout << "Exception catched !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
    }


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

