/*=========================================================================
  
  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ConvertToJpg.cxx,v $
  Language:  C++      
  Date:      $Date: 2008/11/15 23:46:06 $
  Version:   $Revision: 1.19 $

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


template <unsigned int ImageDimension>
int ConvertType(int argc, char *argv[])        
{
   
  typedef  unsigned char outPixelType;
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
  Rcpp::Rcout << " Updated reader " << std::endl;

  typedef itk::CastImageFilter<ImageType,IntermediateType> castertype;
  typename   castertype::Pointer caster=castertype::New();
  caster->SetInput(reader->GetOutput());
  caster->Update();
  
  // Rescale the image intensities so that they fall between 0 and 255
  typedef itk::RescaleIntensityImageFilter<IntermediateType,IntermediateType> FilterType;
  typename   FilterType::Pointer fixedrescalefilter = FilterType::New();
  fixedrescalefilter->SetInput(caster->GetOutput());
  const double desiredMinimum =  0.0;
  const double desiredMaximum =  255.0;
  fixedrescalefilter->SetOutputMinimum( desiredMinimum );
  fixedrescalefilter->SetOutputMaximum( desiredMaximum );
  fixedrescalefilter->UpdateLargestPossibleRegion();
  
  typedef itk::CastImageFilter<IntermediateType,OutImageType> castertype2;
  typename castertype2::Pointer caster2=castertype2::New();
  caster2->SetInput(fixedrescalefilter->GetOutput());
  caster2->Update();

  typename   OutImageType::Pointer outim = caster2->GetOutput();
  typename   OutImageType::SpacingType spc = outim->GetSpacing();
  outim->SetSpacing(spc);
  Rcpp::Rcout << " Dire in " << reader->GetOutput()->GetDirection() << std::endl;
  Rcpp::Rcout << " Dire out " << outim->GetDirection() << std::endl;
  typename   writertype::Pointer writer = writertype::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(outim); 
  writer->Update();
  writer->Write();   
  
  return 0;
 
}     



RcppExport SEXP ConvertToJpg( SEXP r_args )
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
  { Rcpp::Rcout << "Usage:   ConvertToJpg infile.nii out.jpg " << std::endl;
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
     ConvertType<2>(argc,argv);
      break;
   case 3:
     ConvertType<3>(argc,argv);
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


             

       
 
