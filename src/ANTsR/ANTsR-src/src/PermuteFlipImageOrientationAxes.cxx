/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: PermuteFlipImageOrientationAxes.cxx,v $
  Language:  C++
  Date:      $Date: 2009/01/30 16:40:33 $
  Version:   $Revision: 1.19 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or 
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/



#include <Rcpp.h>

#include "itkImage.h"
#include "itkConstantPadImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"

#include "ReadWriteImage.h"

template <unsigned int Dimension>
int PermuteFlipImageOrientationAxes( int argc, char * argv[] )
{

  typedef   float  InputPixelType;
  typedef   float           InternalPixelType;
  typedef   float   OutputPixelType;

  typedef itk::Image< InputPixelType,    Dimension >   InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >   InternalImageType;
  typedef itk::Image< OutputPixelType,   Dimension >   OutputImageType;


  typename InputImageType::Pointer inputImage = NULL;
  ReadImage<InputImageType>(inputImage,argv[1]);

  typename InputImageType::SpacingType inputSpacing = inputImage->GetSpacing();
  
  // Create a filter
  typedef OutputImageType ShortImage;
  typename itk::PermuteAxesImageFilter< ShortImage >::Pointer permute;
  permute = itk::PermuteAxesImageFilter< ShortImage >::New();
  permute->SetInput( inputImage );
  
  unsigned int upperFactors[Dimension];
  unsigned int lowerFactors[Dimension];

  bool flipaboutorigin=false;
  if (Dimension == 2)
    {
    if (argc > 3) upperFactors[0] = atoi(argv[3]);
    if (argc > 4) upperFactors[1] = atoi(argv[4]);
    if (argc > 5) lowerFactors[0] = atoi(argv[5]);
    if (argc > 6) lowerFactors[1] = atoi(argv[6]);
    if (argc > 7) flipaboutorigin=atoi(argv[7]);
    }
  else if (Dimension == 3)
    {
    if (argc > 3) upperFactors[0] = atoi(argv[3]);
    if (argc >  4) upperFactors[1] = atoi(argv[4]);
    if (argc > 5 ) upperFactors[2] = atoi(argv[5]);
    if (argc > 6) lowerFactors[0] = atoi(argv[6]);
    if (argc > 7) lowerFactors[1] = atoi(argv[7]);
    if (argc > 8  ) lowerFactors[2] = atoi(argv[8]);
    if (argc > 9) flipaboutorigin=atoi(argv[9]);
    }

  permute->SetOrder( upperFactors );
  permute->Update();

  typedef itk::FlipImageFilter<ShortImage> FlipType;
  typename FlipType::FlipAxesArrayType flip;
  for (unsigned int i=0; i<Dimension; i++) flip[i]=lowerFactors[i];
  typename FlipType::Pointer flipper = FlipType::New();
  flipper->SetFlipAboutOrigin(flipaboutorigin);
  flipper->SetFlipAxes(flip);
  flipper->SetInput( permute->GetOutput());
  flipper->Update();


  typename InputImageType::Pointer image = flipper->GetOutput();
  WriteImage<OutputImageType>(image,argv[2]);


  return 0;
}


RcppExport SEXP PermuteFlipImageOrientationAxes( SEXP r_args )
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
    Rcpp::Rcout << "Usage: " << std::endl;
    Rcpp::Rcout << argv[0] << " ImageDimension  inputImageFile  outputImageFile xperm yperm {zperm}  xflip yflip {zflip}  {FlipAboutOrigin}" << std::endl; 
    Rcpp::Rcout << " for 3D:  " << argv[0] << " 3  in.nii out.nii   2 0 1  1 1 1  \n would map z=>x, x=>y, y=>z and flip each " << std::endl;
    Rcpp::Rcout << " for 2D:  " << argv[0] << " 2  in.nii out.nii   1 0  1 0  \n would map x=>y, y=>x and flip x  " << std::endl;
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << " 0 1 2 for permute factors gives no axis permutation " << std::endl;
    Rcpp::Rcout << " 1 2 0 maps y to x,  z to y and x to z " << std::endl;
    Rcpp::Rcout <<" the flip values are boolean -  0 1 0 would flip the y-axis only " << std::endl;
    Rcpp::Rcout <<  std::endl << " The FlipAboutOrigin boolean lets you flip about the coordinate set in the origin " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }
   

   // Get the image dimension
  switch( atoi(argv[1]))
   {
   case 2:
     PermuteFlipImageOrientationAxes<2>(argc-1,argv+1);
      break;
   case 3:
     PermuteFlipImageOrientationAxes<3>(argc-1,argv+1);
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

