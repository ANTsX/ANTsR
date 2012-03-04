
#include <Rcpp.h>

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkLabelGeometryImageFilter.h"

#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>

template <unsigned int ImageDimension>
int LabelGeometryMeasures( int argc, char * argv[] )
{
  typedef int LabelType;
  typedef itk::Image<LabelType, ImageDimension> LabelImageType;
  typedef itk::ImageFileReader<LabelImageType> LabelReaderType;

  typedef float RealType;
  typedef itk::Image<RealType, ImageDimension> RealImageType;
  typedef itk::ImageFileReader<RealImageType> ReaderType;

  typename LabelReaderType::Pointer labelReader = LabelReaderType::New();
  labelReader->SetFileName( argv[2] );
  labelReader->Update();

  typename ReaderType::Pointer reader = ReaderType::New();
  if( argc > 3 )
    {
    reader->SetFileName( argv[3] );
    reader->Update();
    }

  typedef itk::LabelGeometryImageFilter<LabelImageType, RealImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( labelReader->GetOutput() );
  if( argc > 3 )
    {
    filter->SetIntensityInput( reader->GetOutput() );
    }
  filter->CalculatePixelIndicesOff();
  filter->CalculateOrientedBoundingBoxOff();
  filter->CalculateOrientedLabelRegionsOff();
  // These generate optional outputs.
//   filter->CalculatePixelIndicesOn();
//   filter->CalculateOrientedBoundingBoxOn();;
//   filter->CalculateOrientedLabelRegionsOn();
  filter->Update();


  typename FilterType::LabelsType allLabels = filter->GetLabels();
  typename FilterType::LabelsType::iterator allLabelsIt;
//   Rcpp::Rcout << "Number of labels: " << labelGeometryFilter->GetNumberOfLabels() << std::endl;
//   Rcpp::Rcout << "Label geometry measures." << std::endl;
  Rcpp::Rcout << std::left << std::setw( 7 )  << "Label"
            << std::left << std::setw( 10 ) << "Volume"
            << std::left << std::setw( 15 ) << "Eccentricity"
            << std::left << std::setw( 15 ) << "Elongation"
            << std::left << std::setw( 15 ) << "Orientation"
            << std::left << std::setw( 30 ) << "Centroid"
            << std::left << std::setw( 30 ) << "Axes Length"
            << std::left << std::setw( 30 ) << "Bounding Box";
  if( filter->GetIntensityInput() )
    {
    Rcpp::Rcout << std::left << std::setw( 20 )  << "Integrated Int."
              << std::left << std::setw( 30 ) << "Weighted Centroid";
    }
  Rcpp::Rcout << std::endl;
  for( allLabelsIt = allLabels.begin(); allLabelsIt != allLabels.end(); allLabelsIt++ )
    {
    if( *allLabelsIt == 0 )
      {
      continue;
      }
    Rcpp::Rcout << std::setw( 7 ) << *allLabelsIt;
    Rcpp::Rcout << std::setw( 10 ) << filter->GetVolume( *allLabelsIt );
    Rcpp::Rcout << std::setw( 15 ) << filter->GetEccentricity( *allLabelsIt );
    Rcpp::Rcout << std::setw( 15 ) << filter->GetElongation( *allLabelsIt );
    Rcpp::Rcout << std::setw( 15 ) << filter->GetOrientation( *allLabelsIt );

    std::stringstream oss;
    oss << filter->GetCentroid( *allLabelsIt );
    Rcpp::Rcout << std::setw( 30 ) << ( oss.str() ).c_str();
    oss.str( "" );

    oss << filter->GetAxesLength( *allLabelsIt );
    Rcpp::Rcout << std::setw( 30 ) << ( oss.str() ).c_str();
    oss.str( "" );

    oss << filter->GetBoundingBox( *allLabelsIt );
    Rcpp::Rcout << std::setw( 30 ) << ( oss.str() ).c_str();
    oss.str( "" );

//     Rcpp::Rcout << filter->GetMajorAxisLength( *allLabelsIt ) << "\t";
//     Rcpp::Rcout << filter->GetMinorAxisLength( *allLabelsIt ) << "\t";
    if( filter->GetIntensityInput() )
      {
      oss << filter->GetIntegratedIntensity( *allLabelsIt );
      Rcpp::Rcout << std::setw( 20 ) << ( oss.str() ).c_str();
      oss.str( "" );

      oss << filter->GetWeightedCentroid( *allLabelsIt );
      Rcpp::Rcout << std::setw( 30 ) << ( oss.str() ).c_str();
      oss.str( "" );
      }
    Rcpp::Rcout << std::endl;
    }

  return EXIT_SUCCESS;
}

RcppExport SEXP LabelGeometryMeasures( SEXP r_args )
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
    Rcpp::Rcout << "Usage: " << argv[0] << " imageDimension labelImage [intensityImage]"
      << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     LabelGeometryMeasures<2>( argc, argv );
     break;
   case 3:
     LabelGeometryMeasures<3>( argc, argv );
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

