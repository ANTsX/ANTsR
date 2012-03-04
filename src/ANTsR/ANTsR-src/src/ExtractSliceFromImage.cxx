
#include <Rcpp.h>

#include <stdio.h>

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkExtractImageFilter.h"

template <unsigned int ImageDimension>
int ExtractSliceFromImage( int itkNotUsed( argc ), char *argv[] )
{
  typedef float PixelType;

  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::Image<PixelType, ImageDimension-1> SliceType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );
  reader->Update();

  typename ImageType::RegionType region;
  typename ImageType::RegionType::SizeType size = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  size[atoi( argv[4] )] = 0;
  typename ImageType::IndexType index;
  index.Fill( 0 );
  index[atoi( argv[4] )] = atoi( argv[5] );
  region.SetIndex( index );
  region.SetSize( size );

  typedef itk::ExtractImageFilter<ImageType, SliceType> ExtracterType;
  typename ExtracterType::Pointer extracter = ExtracterType::New();
  extracter->SetInput( reader->GetOutput() );
  extracter->SetExtractionRegion( region );
  extracter->SetDirectionCollapseToIdentity();
  extracter->Update();

  typedef itk::ImageFileWriter<SliceType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( extracter->GetOutput() );
  writer->Update();


  return 0;
}

RcppExport SEXP ExtractSliceFromImage( SEXP r_args )
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


  if ( argc != 6 )
    {
    Rcpp::Rcout << "Usage: " << argv[0] << " imageDimension inputImage outputSlice direction(e.g. 0, 1, 2) slice_number" << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     ExtractSliceFromImage<2>( argc, argv );
     break;
   case 3:
     ExtractSliceFromImage<3>( argc, argv );
     break;
   case 4:
     ExtractSliceFromImage<4>( argc, argv );
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


