/*=========================================================================
  
  Program:   Advanced Normalization Tools
  Module:    $RCSfile: SetOrigin.cxx,v $
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
int SetOrigin(int argc, char *argv[])        
{
   
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
  typename OutImageType::PointType orig = outim->GetOrigin();

  Rcpp::Rcout << " Old Orig " <<  outim->GetOrigin();
  if (argc > 3) orig[0]=atof(argv[3]);
  if (argc > 4) orig[1]=atof(argv[4]);
  if (argc > 5) orig[2]=atof(argv[5]);
  Rcpp::Rcout <<"  New Orig " << orig << std::endl;

  outim->SetOrigin(orig);


  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  typename ImageType::Pointer varimage=ImageType::New();
  varimage->SetLargestPossibleRegion( outim->GetLargestPossibleRegion() );
  varimage->SetBufferedRegion( outim->GetLargestPossibleRegion() );
  varimage->SetLargestPossibleRegion( outim->GetLargestPossibleRegion() );
  varimage->Allocate();
  varimage->SetSpacing(outim->GetSpacing());
  varimage->SetOrigin(orig);
  varimage->SetDirection( outim->GetDirection());
  Iterator vfIter2( varimage,  varimage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 ) vfIter2.Set(outim->GetPixel(vfIter2.GetIndex()));
  

  typename writertype::Pointer writer = writertype::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(  varimage ); 
  writer->Update();
  writer->Write();   
  
  return 0;
 
}     

             

       
                 
 
RcppExport SEXP SetOrigin( SEXP r_args )
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
    { Rcpp::Rcout << "Usage:   " << argv[0] << "  Dimension infile.hdr outfile.nii  OriginX OriginY {OriginZ} " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }           
   
   // Get the image dimension
  switch( atoi(argv[1]))
   {
   case 2:
     SetOrigin<2>(argc-1,argv+1);
      break;
   case 3:
     SetOrigin<3>(argc-1,argv+1);
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

