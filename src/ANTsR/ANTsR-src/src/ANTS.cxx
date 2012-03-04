/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ANTS.cxx,v $
  Language:  C++
  Date:      $Date: 2008/11/15 23:46:05 $
  Version:   $Revision: 1.19 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or 
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <Rcpp.h>

#include "itkPICSLAdvancedNormalizationToolKit.h"
#include "itkANTSImageTransformation.h"
#include "itkANTSImageRegistrationOptimizer.h"

#include <string>

template <unsigned int ImageDimension>
int ANTSex(int argc, char *argv[])
{
    typedef itk::PICSLAdvancedNormalizationToolKit<ImageDimension> RegistrationType;
    typename RegistrationType::Pointer registration = RegistrationType::New();
    registration->ParseCommandLine( argc, argv );
    Rcpp::Rcout << " Run Reg " << std::endl;
    try
      {
      registration->RunRegistration();
      }
    catch(...)
      {
      Rcpp::Rcout << "Exception thrown: ANTS" << std::endl;
      return EXIT_FAILURE;
      }
    registration->GetTransformationModel()->SetWriteComponentImages(true);
    registration->GetTransformationModel()->Write();

    return EXIT_SUCCESS;

}

RcppExport SEXP ANTS( SEXP r_args )
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

  int  dim = 2;
//  while(argc--) printf("%s\n", *argv++);
  if ( argc < 2 || ((argc == 2) && strcmp(argv[1], "--help") == 0))
    {
    Rcpp::Rcout <<  " \n " << std::endl;
    Rcpp::Rcout <<  "Example usage: \n " << std::endl;
    Rcpp::Rcout << argv[0] <<  " ImageDimension -m MI[fixedimage.nii.gz,movingimage.nii.gz,1,32] -o Outputfname.nii.gz -i 30x20x0 -r Gauss[3,1] -t Elast[3] \n \n " << std::endl;
    Rcpp::Rcout << " Compulsory arguments:\n " << std::endl;
    Rcpp::Rcout << " ImageDimension: 2 or 3 (for 2 or 3 Dimensional registration)\n " << std::endl;
    Rcpp::Rcout << " -m:	Type of similarity model used for registration. \n " << std::endl;
    Rcpp::Rcout << "	For intramodal image registration, use: " << std::endl;
    Rcpp::Rcout << "		CC = cross-correlation " << std::endl;
    Rcpp::Rcout << "		MI = mutual information " << std::endl;
    Rcpp::Rcout << "		PR = probability mapping " << std::endl;
    Rcpp::Rcout << "		MSQ = mean square difference " << std::endl;
    Rcpp::Rcout << " \n " << std::endl;
    Rcpp::Rcout << "	For intermodal image registration, use: " << std::endl;
    Rcpp::Rcout << "		MI = mutual information " << std::endl;
    Rcpp::Rcout << "		PR = probability mapping " << std::endl;
    Rcpp::Rcout << " \n " << std::endl;
    Rcpp::Rcout << " -o 	Outputfname.nii.gz: the name of the resulting image.\n " << std::endl;
    Rcpp::Rcout << " -i 	Max-iterations in format: JxKxL, where: " << std::endl;
    Rcpp::Rcout << "		J = max iterations at coarsest resolution (here, reduce by power of 2^2) " << std::endl;
    Rcpp::Rcout << "		K = middle resolution iterations (here,reduce by power of 2) " << std::endl;
    Rcpp::Rcout << "		L = fine resolution iterations (here, full resolution). This level takes much more time per iteration!\n " << std::endl;
    Rcpp::Rcout << "    	Adding an extra value before JxKxL (i.e. resulting in IxJxKxL) would add another iteration level.\n " << std::endl;
    Rcpp::Rcout << " -r 	Regularization \n" << std::endl;
    Rcpp::Rcout << " -t 	Type of transformation model used for registration \n" << std::endl;
    Rcpp::Rcout << "	For elastic image registration, use: " << std::endl;
    Rcpp::Rcout << "		Elast = elastic transformation model (less deformation possible)\n " << std::endl;
    Rcpp::Rcout << "	For diffeomorphic image registration, use: " << std::endl;
    Rcpp::Rcout << "		Syn[GradStep,TimePoints,IntegrationStep] --geodesic 2 = SyN with time with arbitrary number of time points in time discretization  " << std::endl;
    Rcpp::Rcout << "		SyN[GradStep,2,IntegrationStep] = SyN with time optimized specifically for 2 time points in the time discretization " << std::endl;
    Rcpp::Rcout << "		SyN[GradStep] = Greedy SyN, typicall GradStep=0.25  " << std::endl;
    Rcpp::Rcout << "		Exp[GradStep,TimePoints] = Exponential " << std::endl;
    Rcpp::Rcout << "		GreedyExp = Diffeomorphic Demons style exponential mapping " << std::endl;
    Rcpp::Rcout << " \n " << std::endl;
    Rcpp::Rcout << " Please use the `ANTS -h ` call or refer to the ANTS.pdf manual or antsIntroduction.sh script for additional information and typical values for transformation models\n " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE );
    }
    else  dim = atoi( argv[1] );

      
    if ( dim <= 1 || dim > 3 ) 
      {
      Rcpp::Rcout <<" You passed ImageDimension: " << dim << " . Please use only 2 or 3 (for 2 or 3 Dimensional registration)  " << std::endl;
      argv[1]=(char*)("--help");
      ANTSex<2>( argc,argv );
      return Rcpp::wrap( EXIT_FAILURE );
      }
    /**
     * Try the simple case of the call "ANTS fixedImage movingImage"
     */
    if( argc == 3 && ( atoi( argv[1] ) != 2 || atoi( argv[1] ) != 3 ) )
      {
      itk::ImageIOBase::Pointer fixedImageIO
        = itk::ImageIOFactory::CreateImageIO(
          argv[1], itk::ImageIOFactory::ReadMode );
      if( fixedImageIO.IsNull() )
        {
        Rcpp::Rcout << "Invalid fixed image: " << argv[1] << std::endl;
        return Rcpp::wrap( EXIT_FAILURE );
        }
      itk::ImageIOBase::Pointer movingImageIO
        = itk::ImageIOFactory::CreateImageIO(
          argv[2], itk::ImageIOFactory::ReadMode );
      if( movingImageIO.IsNull() )
        {
        Rcpp::Rcout << "Invalid moving image: " << argv[2] << std::endl;
        return Rcpp::wrap( EXIT_FAILURE );
        }
      fixedImageIO->SetFileName( argv[1] );
      fixedImageIO->ReadImageInformation();
      movingImageIO->SetFileName( argv[2] );
      movingImageIO->ReadImageInformation();

      unsigned int fdim = fixedImageIO->GetNumberOfDimensions();
      unsigned int mdim = movingImageIO->GetNumberOfDimensions();

      if( fdim != mdim )
        {
        Rcpp::Rcout << "Fixed image dimension does not equal "
          << "the moving image dimension (" << fdim << " != " << mdim << ")"
          << std::endl;
        return Rcpp::wrap( EXIT_FAILURE );
        }
      if( fdim != 2 && fdim != 3 )
        {
        Rcpp::Rcout << "Unsupported image dimension" << std::endl;
        return Rcpp::wrap( EXIT_FAILURE );
        }

      /**
       * After the checking, we can add the default parameters;
       */
      std::string arguments; 
       
      std::string transformation( " -t SyN[1.0] ");
      std::string regularization( " -r Gauss[3,0.5] ");
      std::string metric = std::string( " -m PR[" ) + std::string( argv[1] )
        + std::string( "," ) + std::string( argv[2] ) + std::string( ",1,3] " );
      std::string outputNaming( " -o ANTS.nii.gz " );

      long maxSize = vnl_math_min( fixedImageIO->GetDimensions( 0 ),
        movingImageIO->GetDimensions( 0 ) );
      for( unsigned int d = 1; d < fdim; d++ )
        {
        long tmpMax = vnl_math_max( fixedImageIO->GetDimensions( d ),
          movingImageIO->GetDimensions( d ) );
        if( maxSize < tmpMax )
          {
          maxSize = tmpMax;
          }
        }

      unsigned int numberOfLevels = static_cast<unsigned int>(
        vcl_log( (double)maxSize / 32 ) / vcl_log( (double) 2 ) ) + 1;
      std::string iterations( " -i " );  
      for( int n = numberOfLevels; n > 0; n-- )
        {
        std::ostringstream buf;
        unsigned long numberOfIterations = ( 5 << (n-1) );
        buf << numberOfIterations << "x";
        iterations += buf.str();
        }
      iterations = iterations.substr( 0, iterations.length()-1 );  
      
      std::ostringstream dimBuf;
      dimBuf << fdim;
      arguments.clear();
      arguments = std::string( " ANTS " ) + dimBuf.str() + iterations + 
        transformation + regularization + metric + outputNaming;  
 
      dim = fdim;

      Rcpp::Rcout << arguments << std::endl;
      
      unsigned int my_argc = 0;
      std::string::size_type delimPos = 0;
      std::string::size_type tokenPos = 0;
      std::string::size_type pos = 0;
      while( true )
        {
        delimPos = arguments.find_first_of( " ", pos );
        tokenPos = arguments.find_first_not_of( " ", pos );
        if( std::string::npos != delimPos &&
          std::string::npos != tokenPos && tokenPos < delimPos )
          {
          my_argc++; 
          } 
        else if( std::string::npos == delimPos )
          { 
          if( std::string::npos != tokenPos )
            {
            my_argc++; 
            }
          else
            {  
            break;
            }
          } 
        pos = delimPos + 1;
        }

      unsigned int arg_count = 0;  
      char **my_argv = new char*[my_argc];
      delimPos = 0;
      tokenPos = 0;
      pos = 0;
      while( true )
        {
        delimPos = arguments.find_first_of( " ", pos );
        tokenPos = arguments.find_first_not_of( " ", pos );
        if( std::string::npos != delimPos &&
          std::string::npos != tokenPos && tokenPos < delimPos )
          {
          std::string arg = arguments.substr( pos, delimPos-pos );
          my_argv[arg_count] = new char[arg.size() + 1];
          strcpy( my_argv[arg_count], arg.c_str() );
          arg_count++;
          } 
        else if( std::string::npos == delimPos )
          { 
          if( std::string::npos != tokenPos )
            {
            std::string arg = arguments.substr( pos );
            my_argv[arg_count] = new char[arg.size() + 1];
            strcpy( my_argv[arg_count], arg.c_str() );
            arg_count++;
            }
          else
            {  
            break;
            }
          } 
        pos = delimPos + 1;
        }

      switch ( dim )
        {
        case 3:
          ANTSex<3>( my_argc, my_argv );
          break;
        default:
          ANTSex<2>( my_argc, my_argv );
        }
      }
    else
      {  
      switch ( dim )
        {
        case 3:
          ANTSex<3>( argc, argv );
          break;
        default:
          ANTSex<2>( argc, argv );
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
