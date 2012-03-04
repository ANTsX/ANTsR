/*=========================================================================
  
  Program:   Advanced Normalization Tools
  Module:    $RCSfile: CopyImageHeaderInformation.cxx,v $
  Language:  C++      
  Date:      $Date: 2009/04/30 18:32:36 $
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
#include "ReadWriteImage.h"
#include "TensorFunctions.h"

template <unsigned int ImageDimension>
int CopyImageHeaderInformation(int argc, char *argv[])        
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
  //  Rcpp::Rcout << " Spacing " << reader->GetOutput()->GetSpacing() << std::endl;
  //Rcpp::Rcout << " Origin " << reader->GetOutput()->GetOrigin() << std::endl;
  //Rcpp::Rcout << " Direction " << std::endl << reader->GetOutput()->GetDirection() << std::endl;
  //Rcpp::Rcout << " Size " << std::endl << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;

  bool istensor=false;
  if (argc >7) if (atoi(argv[7])) istensor=true;
  if (istensor) 
    {
      typedef itk::Vector<float, 6> TensorType;
      typedef itk::Image<TensorType,ImageDimension>     TensorFieldType;
      typename TensorFieldType::Pointer timage;
      ReadTensorImage<TensorFieldType>(timage,argv[2],false);
      //      Rcpp::Rcout<< " tim dir " << timage->GetDirection() << std::endl;
      if (argc >6) if (atoi(argv[6])) timage->SetSpacing(  reader->GetOutput()->GetSpacing()  );
      if (argc > 5) if (atoi(argv[5])) timage->SetOrigin(  reader->GetOutput()->GetOrigin()  );
      if (argc > 4) if (atoi(argv[4])) timage->SetDirection(  reader->GetOutput()->GetDirection()  );

      //      Rcpp::Rcout<< " tim dir " << timage->GetDirection() << std::endl;
      WriteTensorImage<TensorFieldType>( timage ,argv[3],false);
      
      return 0;
    }


  typename readertype::Pointer reader2 = readertype::New();
  reader2->SetFileName(argv[2]); 
  reader2->Update();   

  
  //MakeNewImage(typename TImage::Pointer image1, typename TImage::PixelType initval)
  typename ImageType::Pointer newimage = MakeNewImage<ImageType>(reader2->GetOutput(), -1);
  
  if (argc >6) if (atoi(argv[6])) newimage->SetSpacing(  reader->GetOutput()->GetSpacing()  );
  if (argc > 5) if (atoi(argv[5])) newimage->SetOrigin(  reader->GetOutput()->GetOrigin()  );
  if (argc > 4) if (atoi(argv[4])) newimage->SetDirection(  reader->GetOutput()->GetDirection()  );



  WriteImage<ImageType>(newimage,argv[3]);

  return 1;
 
}     



RcppExport SEXP CopyImageHeaderInformation( SEXP r_args )
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

   
  if ( argc < 4  )     
    { Rcpp::Rcout << "Usage:  " << argv[0] << " refimage.ext imagetocopyrefimageinfoto.ext imageout.ext   boolcopydirection  boolcopyorigin boolcopyspacing  {bool-Image2-IsTensor}" << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }           
                 
   // Get the image dimension
  std::string fn = std::string(argv[1]);
   itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(
         fn.c_str(), itk::ImageIOFactory::ReadMode);
   imageIO->SetFileName(fn.c_str());
   imageIO->ReadImageInformation();
   unsigned int dim=imageIO->GetNumberOfDimensions();
   switch ( dim  )
   {
   case 2:
     CopyImageHeaderInformation<2>(argc,argv);
      break;
   case 3:
     CopyImageHeaderInformation<3>(argc,argv);
      break;
   default:
     Rcpp::Rcout << "Unsupported dimension : " << dim<< std::endl;
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


             

       
 
