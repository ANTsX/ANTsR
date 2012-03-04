
#include <Rcpp.h>

#include "stdio.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "ReadWriteImage.h"
#include "TensorFunctions.h"


template <unsigned int ImageDimension>
int AverageTensorImages(unsigned int argc, char *argv[])        
{

  // typedef itk::Vector<float,6> TensorType;
  typedef itk::SymmetricSecondRankTensor< float, 3 >  TensorType; 

  typedef itk::Image<TensorType, ImageDimension> ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> IteratorType;
    
  char * outputName = argv[2];
  int mathtype = atoi(argv[3]);
  float numberofimages = (float)argc - 4.0;

  Rcpp::Rcout << "Averaging " << numberofimages << " images " << std::endl;

  typename ImageType::Pointer averageimage = NULL; 
  typename ImageType::Pointer image2 = NULL; 

  typename ImageType::SizeType size; 
  size.Fill(0);
  unsigned int bigimage=0;

  for (unsigned int j=4; j< argc; j++)
  {
    // Get the image dimension
    std::string fn = std::string(argv[j]);
    Rcpp::Rcout <<" fn " << fn << std::endl;
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
    
  std:: cout << " largest image " << size << std::endl;

  bool logeuc = true;
  if (mathtype == 1)
    logeuc = false;

  TensorType nullTensor;
  nullTensor[0] = nullTensor[1] = nullTensor[2] = nullTensor[3] 
    = nullTensor[4] = nullTensor[5] = 0;

  ReadTensorImage<ImageType>(averageimage,argv[bigimage],logeuc);
  averageimage->FillBuffer(nullTensor);

  for (unsigned int j=4; j< argc; j++)
  {
    std::string fn = std::string(argv[j]);
    ReadTensorImage<ImageType>(image2,fn.c_str(),logeuc);

    IteratorType vfIter( image2,  image2->GetLargestPossibleRegion() );  
    for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter )
    {
      TensorType val =  vfIter.Get() / numberofimages;
      averageimage->SetPixel(vfIter.GetIndex(), val + averageimage->GetPixel(vfIter.GetIndex()));
    }
  }

  WriteTensorImage<ImageType>(averageimage,outputName,logeuc);
  
  return EXIT_SUCCESS;

}


RcppExport SEXP AverageTensorImages( SEXP r_args )
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


  try 
  {

    int dim = atoi(argv[1]);
    //char * outputName = argv[2];
    //int mathtype = atoi(argv[3]);
    int numberofimages = argc - 4;
    
    if (numberofimages < 1)
      {
      Rcpp::Rcout << "Basic useage ex: " << std::endl;
      Rcpp::Rcout << argv[0] << " ImageDimension  average.nii mathtype list-of-files-via-wildcard " << std::endl;
      Rcpp::Rcout << " e.g. \n   AverageTensorImages 3  average.nii  1  *registered.nii " << std::endl;
      Rcpp::Rcout << " mathtype=[0=log-euclidean, 1=euclidean] " << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
      }

     // Get the image dimension
   switch(dim)
   {
   case 2:
     AverageTensorImages<2>(argc,argv);
      break;
   case 3:
     AverageTensorImages<3>(argc,argv);
      break;
   default:
      Rcpp::Rcout << "Unsupported dimension" << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
   }
	
  return Rcpp::wrap( EXIT_SUCCESS ) ;
  }
  catch( itk::ExceptionObject & err ) 
    { 
    Rcpp::Rcout << "ExceptionObject caught !" << std::endl; 
    Rcpp::Rcout << err << std::endl; 
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


   

