/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ThresholdImage.cxx,v $
  Language:  C++
  Date:      $Date: 2009/01/27 23:45:44 $
  Version:   $Revision: 1.20 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <Rcpp.h>

#include <cstdlib>
#include <ctime>
#include <iostream>



// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkExtractImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOtsuMultipleThresholdsImageFilter.h"


#include <fstream>

template <class TImage>
typename TImage::Pointer
MultiplyImage(typename TImage::Pointer image1, typename TImage::Pointer image2)
{
Rcpp::Rcout << " Multiply " << std::endl;
    // Begin Multiply Images
    typedef TImage tImageType;
    //  output will be the speed image for FMM
    typedef itk::MultiplyImageFilter<tImageType,
    tImageType,tImageType >  MultFilterType;
    typename MultFilterType::Pointer filter = MultFilterType::New();
    filter->SetInput1( image1 );
    filter->SetInput2( image2 );
    filter->Update();
    return filter->GetOutput(); // this is the speed image

    // write a function to threshold the speedimage so
    // if the dist is g.t. D then speed = 1

}



template <class TImage>
typename TImage::Pointer BinaryThreshold(
  typename TImage::PixelType low,
   typename TImage::PixelType high,
   typename TImage::PixelType insideval, typename TImage::PixelType outsideval,
   typename TImage::Pointer input )
{
Rcpp::Rcout << " Binary Thresh " << std::endl;

  typedef typename TImage::PixelType PixelType;
  // Begin Threshold Image
  typedef itk::BinaryThresholdImageFilter<TImage,TImage>  InputThresholderType;
  typename InputThresholderType::Pointer inputThresholder =
    InputThresholderType::New();

  inputThresholder->SetInput( input );
  inputThresholder->SetInsideValue(  insideval );
  inputThresholder->SetOutsideValue( outsideval );

  if (high < low) high=255;
  float eps=1.e-6*low;
  inputThresholder->SetLowerThreshold((PixelType) low-eps );
  inputThresholder->SetUpperThreshold((PixelType) high+eps);
  inputThresholder->Update();

  return inputThresholder->GetOutput();
}

template <class TImage>
typename TImage::Pointer
  LabelSurface(typename TImage::PixelType foreground,
  typename TImage::PixelType newval, typename TImage::Pointer input)
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

//  Rcpp::Rcout << " foreg " << (int) foreground;
  while (!GHood.IsAtEnd())
  {
    typename TImage::PixelType p = GHood.GetCenterPixel();
    typename TImage::IndexType ind = GHood.GetIndex();
    typename TImage::IndexType ind2;
    if ( p == foreground )
    {
      bool atedge=false;

      for (int i = 0; i < GHood.Size(); i++)
      {
        ind2=GHood.GetIndex(i);
        float dist=0.0;
        for (int j=0; j<ImageDimension; j++)
          dist+=(float)(ind[j]-ind2[j])*(float)(ind[j]-ind2[j]);
        dist=sqrt(dist);
  	    if (GHood.GetPixel(i) != foreground && dist < 1.1 )
        {
          atedge=true;
        }
      }
      if (atedge && p == foreground) Image->SetPixel(ind,newval);
      else if ( p == foreground) Image->SetPixel(ind,0);
    }
    ++GHood;
  }
  return Image;
}

template <class TImage>
typename TImage::Pointer
DanielssonDistanceMap(
typename TImage::PixelType pixlo,
typename TImage::PixelType pixhi,
typename TImage::Pointer input)
{
Rcpp::Rcout << " DDMap " << std::endl;

  typedef TImage ImageType;

  typedef itk::DanielssonDistanceMapImageFilter<
               ImageType, ImageType >  FilterType;

  typename  FilterType::Pointer filter = FilterType::New();
  filter->InputIsBinaryOn();
  filter->SetUseImageSpacing(true);
  filter->SetInput(BinaryThreshold<TImage>(pixlo,pixhi,pixhi,input));
  filter->Update();

//  std::string fn="C:\\Data\\temp.img";
//  WriteImage(filter->GetOutput(),fn.c_str());
//  fn="C:\\Data\\temp2.img";
//  WriteImage(filter->GetVoronoiMap(),fn.c_str());

  return filter->GetOutput();

}

template <class TImage>
typename TImage::Pointer OtsuThreshold(
   int NumberOfThresholds, typename TImage::Pointer input)
{
Rcpp::Rcout << " Otsu Thresh with " << NumberOfThresholds << " thresholds" << std::endl;

  typedef typename TImage::PixelType PixelType;
  // Begin Threshold Image
  typedef itk::OtsuMultipleThresholdsImageFilter<TImage,TImage>  InputThresholderType;
  typename InputThresholderType::Pointer inputThresholder =
    InputThresholderType::New();

  inputThresholder->SetInput( input );
  /*
  inputThresholder->SetInsideValue(  replaceval );
  int outval=0;
  if ((float) replaceval == (float) -1) outval=1;
  inputThresholder->SetOutsideValue( outval );
  */
  inputThresholder->SetNumberOfThresholds ( NumberOfThresholds );

  inputThresholder->Update();

  return inputThresholder->GetOutput();
}



template <unsigned int InImageDimension>
int ThresholdImage( int argc, char * argv[] )
{

  //  const     unsigned int   InImageDimension = AvantsImageDimension;
  typedef   float  PixelType;
  typedef   itk::Image< PixelType, InImageDimension >  FixedImageType;
  typedef   itk::ImageFileReader< FixedImageType  >  FixedReaderType;
  typename FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

  typedef   itk::ImageFileWriter< FixedImageType >  MovingWriterType;
  typename MovingWriterType::Pointer movingWriter = MovingWriterType::New();
  typename MovingWriterType::Pointer movingWriter2 = MovingWriterType::New();
  movingWriter->SetFileName( argv[3] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    Rcpp::Rcout << "Exception thrown " << std::endl;
    Rcpp::Rcout << excp << std::endl;
    return EXIT_FAILURE;
    }
  // Label the surface of the image
  typename FixedImageType::Pointer thresh;
  std::string threshtype=std::string(argv[4]);
  if (strcmp(threshtype.c_str(),"Otsu") == 0 )
  {
  	thresh = OtsuThreshold<FixedImageType>(atoi(argv[5]),fixedReader->GetOutput());
  }
  else
    {
    PixelType insideValue = 1;
    PixelType outsideValue = 0;
    if( argc > 6 )
      {
      insideValue = static_cast<PixelType>( atof( argv[6] ) );
      }
    if( argc > 7 )
      {
      outsideValue = static_cast<PixelType>( atof( argv[7] ) );
      }
    thresh = BinaryThreshold<FixedImageType>(atof(argv[4]),atof(argv[5]),
      insideValue,outsideValue,fixedReader->GetOutput());
    }

  movingWriter->SetInput(thresh);
  movingWriter->Write( );
  return EXIT_SUCCESS;
}

RcppExport SEXP ThresholdImage( SEXP r_args )
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

  if( argc < 3 )
    {
    Rcpp::Rcout << "Usage: " << argv[0];
    Rcpp::Rcout << "   ImageDimension ImageIn.ext outImage.ext  threshlo threshhi <insideValue> <outsideValue>" << std::endl;
    Rcpp::Rcout << "   ImageDimension ImageIn.ext outImage.ext  Otsu NumberofThresholds " << std::endl;

    Rcpp::Rcout << " Inclusive thresholds " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

   // Get the image dimension

  switch ( atoi(argv[1]))
   {
   case 2:
     ThresholdImage<2>(argc,argv);
      break;
   case 3:
     ThresholdImage<3>(argc,argv);
      break;
   case 4:
     ThresholdImage<4>(argc,argv);
      break;
   default:
      Rcpp::Rcout << "Unsupported dimension" << std::endl;
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
