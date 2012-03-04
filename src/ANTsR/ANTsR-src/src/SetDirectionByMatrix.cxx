/*
 * ResetDirection2.cxx
 *
 *  Created on: Nov 14, 2008
 *      Author: songgang
 */

/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ResetDirection2.cxx,v $
  Language:  C++
  Date:      $Date: 2008/11/14 20:47:46 $
  Version:   $Revision: 1.1 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or 
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <Rcpp.h>

#include <iostream>
#include <fstream>
#include <stdio.h>
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

// #include "../itkAvants.DefineDimension"

template <unsigned int ImageDimension>
int ResetDirection(int argc, char *argv[])
{

  if ( argc < 3 )
    { Rcpp::Rcout << "Usage:   " << argv[0] << "  infile.nii outfile.nii direction matrix in a row " << std::endl;
    return 1;
  }

  typedef  float  outPixelType;
  typedef  float floatPixelType;
  typedef  float inPixelType;
  typedef itk::Image<inPixelType,ImageDimension> ImageType;
  typedef itk::Image<floatPixelType,ImageDimension> IntermediateType;
  typedef itk::Image<outPixelType,ImageDimension> OutImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<OutImageType> writertype;

  typename readertype::Pointer reader = readertype::New();
  reader->SetFileName(argv[1]);
  reader->Update();


   typename OutImageType::Pointer outim = reader->GetOutput();
   typename OutImageType::DirectionType direction=outim->GetDirection();
   //direction->SetIdentity();
   // direction.Fill(0);
   // for (unsigned int i=0;i<ImageDimension;i++) direction[i][i]=1;

   char** dtext = argv+3;
   for(unsigned int i=0; i<ImageDimension; i++) {
       for(unsigned int j=0; j<ImageDimension; j++) {
           direction[i][j] = atof(dtext[i*ImageDimension+j]);
           Rcpp::Rcout << "direction["<<i<<"]["<<j<<"]="<<direction[i][j]<<std::endl;
       }
   }


  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  typename ImageType::Pointer varimage=ImageType::New();
  varimage->SetLargestPossibleRegion( outim->GetLargestPossibleRegion() );
  varimage->SetBufferedRegion( outim->GetLargestPossibleRegion() );
  varimage->SetLargestPossibleRegion( outim->GetLargestPossibleRegion() );
  varimage->Allocate();
  varimage->SetSpacing(outim->GetSpacing());
  varimage->SetOrigin(outim->GetOrigin());
  varimage->SetDirection( direction );
  Iterator vfIter2( varimage,  varimage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 ) vfIter2.Set(outim->GetPixel(vfIter2.GetIndex()));


  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(  varimage );
  writer->Update();
  writer->Write();

  return 0;

}





RcppExport SEXP SetDirectionByMatrix( SEXP r_args )
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
    { Rcpp::Rcout << "Usage:   " << argv[0] << "  infile.nii outfile.nii  d01 d02 d03 d10 .... " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }

   // Get the image dimension
  std::string fn = std::string(argv[1]);
   itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(
         fn.c_str(), itk::ImageIOFactory::ReadMode);
   imageIO->SetFileName(fn.c_str());
   imageIO->ReadImageInformation();

   switch ( imageIO->GetNumberOfDimensions() )
   {
   case 2:
     ResetDirection<2>(argc,argv);
      break;
   case 3:
     ResetDirection<3>(argc,argv);
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

