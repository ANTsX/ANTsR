/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: MemoryTest.cxx,v $
  Language:  C++
  Date:      $Date: 2008/11/15 23:46:06 $
  Version:   $Revision: 1.18 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <Rcpp.h>

#include "ReadWriteImage.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkAvantsMutualInformationRegistrationFunction.h"
#include "itkProbabilisticRegistrationFunction.h"
#include "itkCrossCorrelationRegistrationFunction.h"
// #include "itkLandmarkCrossCorrelationRegistrationFunction.h"


template <unsigned int ImageDimension>
int MemoryTest(unsigned int argc, char *argv[])
{
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::AffineTransform<double,ImageDimension>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType1;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;


  typedef itk::Image<float,2> JointHistType;
  typedef itk::ImageFileWriter<JointHistType> jhwritertype;

// get command line params
  unsigned int argct=2;
  unsigned int whichmetric = atoi(argv[argct]); argct++;
  std::string fn1 = std::string(argv[argct]); argct++;
  std::string fn2 = std::string(argv[argct]); argct++;
  unsigned int numberoffields=11;
  if (argc > argct) numberoffields=atoi(argv[argct]); argct++;

  typename ImageType::Pointer image1 = NULL;
  ReadImage<ImageType>(image1, fn1.c_str());
  typename ImageType::Pointer image2 = NULL;
  ReadImage<ImageType>(image2, fn2.c_str());

  typedef itk::ImageRegionIteratorWithIndex<FieldType> VIterator;
  std::vector<typename FieldType::Pointer>  fieldvec;
  for (unsigned int i=0; i < numberoffields; i++)
    {
    Rcpp::Rcout <<" NFields " << i << " of " << numberoffields << std::endl;
    typename FieldType::Pointer field=FieldType::New();
    field->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
    field->SetBufferedRegion( image1->GetLargestPossibleRegion() );
    field->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
    field->Allocate();
    field->SetSpacing(image1->GetSpacing());
    field->SetOrigin(image1->GetOrigin());
    VectorType zero;
    zero.Fill(0);
    VIterator vfIter2( field,  field->GetLargestPossibleRegion() );
    for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
      {
      IndexType index=vfIter2.GetIndex();
      vfIter2.Set(zero);
      }
    fieldvec.push_back(field);
    }

  typename ImageType::Pointer metricimg=ImageType::New();
  metricimg->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
  metricimg->SetBufferedRegion( image1->GetLargestPossibleRegion() );
  metricimg->SetLargestPossibleRegion( image1->GetLargestPossibleRegion() );
  metricimg->Allocate();
  metricimg->SetSpacing(image1->GetSpacing());
  metricimg->SetOrigin(image1->GetOrigin());
  Iterator iter( metricimg,  metricimg->GetLargestPossibleRegion() );
  for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter ) iter.Set(0);

  typedef ImageType FixedImageType;
  typedef ImageType MovingImageType;
  typedef FieldType DisplacementFieldType;

  // Choose the similarity metric
  typedef itk::AvantsMutualInformationRegistrationFunction<FixedImageType,MovingImageType,DisplacementFieldType> MIMetricType;
  typedef itk::CrossCorrelationRegistrationFunction<FixedImageType,MovingImageType,DisplacementFieldType> CCMetricType;
  //typedef itk::LandmarkCrossCorrelationRegistrationFunction<FixedImageType,MovingImageType,DisplacementFieldType> MetricType;
  //typename
  typename MIMetricType::Pointer mimet=MIMetricType::New();
  typename CCMetricType::Pointer ccmet=CCMetricType::New();

//  int nbins=32;

  typename CCMetricType::RadiusType hradius;
  typename CCMetricType::RadiusType ccradius;
  ccradius.Fill(4);
  typename MIMetricType::RadiusType miradius;
  miradius.Fill(0);

//  mimet->SetDisplacementField(field);
  mimet->SetFixedImage(image1);
  mimet->SetMovingImage(image2);
  mimet->SetRadius(miradius);
  mimet->SetGradientStep(1.e2);
  mimet->SetNormalizeGradient(false);

//  ccmet->SetDisplacementField(field);
  ccmet->SetFixedImage(image1);
  ccmet->SetMovingImage(image2);
  ccmet->SetRadius(ccradius);
  ccmet->SetGradientStep(1.e2);
  ccmet->SetNormalizeGradient(false);

  double metricvalue=0;
  std::string metricname="";
  if (whichmetric  == 0)
    {
    hradius=miradius;
    unsigned long ct = 0;
    for(  iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
      {
      IndexType index=iter.GetIndex();
      double fval=image1->GetPixel(index);
      double mval=image2->GetPixel(index);
      metricvalue+=fabs(fval-mval);
      ct++;
      }
    metricvalue/=(float)ct;
    metricname="MSQ ";
    }
  else if (whichmetric == 1 ) // imagedifference
    {
    hradius=ccradius;
    ccmet->InitializeIteration();
    metricvalue=ccmet->ComputeCrossCorrelation();
    metricname="CC ";
    }
  else
    {
    hradius=miradius;
    mimet->InitializeIteration();
    metricvalue=mimet->ComputeMutualInformation();
   metricname="MI ";
     }


  return 0;

 }



RcppExport SEXP MemoryTest( SEXP r_args )
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


  if ( argc < 3 )
    { Rcpp::Rcout << "Basic useage ex: " << std::endl;
    Rcpp::Rcout << argv[0] << " ImageDimension whichmetric image1.ext image2.ext NumberOfFieldsToAllocate " << std::endl;
    Rcpp::Rcout << "  outimage and logfile are optional  " << std::endl;
    Rcpp::Rcout << "  Metric 0 - MeanSquareDifference, 1 - Cross-Correlation, 2-Mutual Information  " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }

   // Get the image dimension
  switch( atoi(argv[1]))
   {
   case 2:
     MemoryTest<2>(argc,argv);
      break;
   case 3:
     MemoryTest<3>(argc,argv);
      break;
   default:
      Rcpp::Rcout << "Unsupported dimension" << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
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


