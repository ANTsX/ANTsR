
#include <Rcpp.h>

#include <string>
#include <fstream>
#include <iostream>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"

#include "itkRedColormapFunction.h"
#include "itkGreenColormapFunction.h"
#include "itkBlueColormapFunction.h"
#include "itkGreyColormapFunction.h"
#include "itkHotColormapFunction.h"
#include "itkCoolColormapFunction.h"
#include "itkSpringColormapFunction.h"
#include "itkSummerColormapFunction.h"
#include "itkAutumnColormapFunction.h"
#include "itkWinterColormapFunction.h"
#include "itkCopperColormapFunction.h"
#include "itkHSVColormapFunction.h"
#include "itkJetColormapFunction.h"
#include "itkCustomColormapFunction.h"
#include "itkOverUnderColormapFunction.h"

#include "itkScalarToRGBColormapImageFilter.h"


template <unsigned int ImageDimension>
int ConvertScalarImageToRGB( int argc, char *argv[] )
{
  typedef unsigned int PixelType;
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
//  typedef itk::RGBAPixel<unsigned char> RGBPixelType;

  typedef float RealType;

  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::Image<float, ImageDimension> RealImageType;
  typedef itk::Image<RGBPixelType, ImageDimension> RGBImageType;

  typedef itk::ImageFileReader<RealImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );
  reader->Update();

  typedef itk::Image<unsigned char, ImageDimension> MaskImageType;
  typename MaskImageType::Pointer maskImage = MaskImageType::New();
  typedef itk::ImageFileReader<MaskImageType> MaskReaderType;
  typename MaskReaderType::Pointer maskreader = MaskReaderType::New();
  maskreader->SetFileName( argv[4] );
  try
    {
    maskreader->Update();
    maskImage = maskreader->GetOutput();
    }
  catch(...)
    {
    maskImage = NULL;
    };


  std::string colormapString( argv[5] );

  typedef itk::ScalarToRGBColormapImageFilter<RealImageType,
    RGBImageType> RGBFilterType;
  typename RGBFilterType::Pointer rgbfilter = RGBFilterType::New();
  rgbfilter->SetInput( reader->GetOutput() );

  if ( colormapString == "red" )
    {
    rgbfilter->SetColormap( RGBFilterType::Red );
    }
  else if ( colormapString == "green"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Green );
    }
  else if ( colormapString == "blue"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Blue );
    }
  else if ( colormapString == "grey"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Grey );
    }
  else if ( colormapString == "cool"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Cool );
    }
  else if ( colormapString == "hot"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Hot );
    }
  else if ( colormapString == "spring"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Spring );
    }
  else if ( colormapString == "autumn"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Autumn );
    }
  else if ( colormapString == "winter"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Winter );
    }
  else if ( colormapString == "copper"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Copper );
    }
  else if ( colormapString == "summer"  )
    {
    rgbfilter->SetColormap( RGBFilterType::Summer );
    }
  else if ( colormapString == "jet"  )
    {
//    rgbfilter->SetColormap( RGBFilterType::Jet );
    typedef itk::Function::JetColormapFunction<typename RealImageType::PixelType,
      typename RGBImageType::PixelType> ColormapType;
    typename ColormapType::Pointer colormap = ColormapType::New();
//    rgbfilter->SetColormap( colormap );
    }
  else if ( colormapString == "hsv"  )
    {
//    rgbfilter->SetColormap( RGBFilterType::HSV );
    typedef itk::Function::HSVColormapFunction<typename RealImageType::PixelType,
      typename RGBImageType::PixelType> ColormapType;
    typename ColormapType::Pointer colormap = ColormapType::New();
//    rgbfilter->SetColormap( colormap );
    }
  else if ( colormapString == "overunder"  )
    {
    rgbfilter->SetColormap( RGBFilterType::OverUnder );
    }
  else if ( colormapString == "custom"  )
    {
    typedef itk::Function::CustomColormapFunction<typename RealImageType::PixelType,
      typename RGBImageType::PixelType> ColormapType;
    typename ColormapType::Pointer colormap = ColormapType::New();

    std::ifstream str( argv[6] );
    std::string line;

    // Get red values
    {
    std::getline( str, line );
    std::istringstream iss( line );
    float value;
    typename ColormapType::ChannelType channel;
    while ( iss >> value )
      {
      channel.push_back( value );
      }
    colormap->SetRedChannel( channel );
    }

    // Get green values
    {
    std::getline( str, line );
    std::istringstream iss( line );
    float value;
    typename ColormapType::ChannelType channel;
    while ( iss >> value )
      {
      channel.push_back( value );
      }
    colormap->SetGreenChannel( channel );
    }
    // Get blue values
    {
    std::getline( str, line );
    std::istringstream iss( line );
    float value;
    typename ColormapType::ChannelType channel;
    while ( iss >> value )
      {
      channel.push_back( value );
      }
    colormap->SetBlueChannel( channel );
    }
//    rgbfilter->SetColormap( colormap );
    }

  if( maskImage )
    {

    RealType maskMinimumValue = itk::NumericTraits<RealType>::max();
    RealType maskMaximumValue = itk::NumericTraits<RealType>::NonpositiveMin();

    itk::ImageRegionIterator<MaskImageType> ItM( maskImage,
      maskImage->GetLargestPossibleRegion() );
    itk::ImageRegionIterator<RealImageType> ItS( reader->GetOutput(),
      reader->GetOutput()->GetLargestPossibleRegion() );
    for( ItM.GoToBegin(), ItS.GoToBegin(); !ItM.IsAtEnd(); ++ItM, ++ItS )
      {
      if( ItM.Get() != 0 )
        {
        if( maskMinimumValue >= ItS.Get() )
          {
          maskMinimumValue = ItS.Get();
          }
        if( maskMaximumValue <= ItS.Get() )
          {
          maskMaximumValue = ItS.Get();
          }
        }
      }

    rgbfilter->SetUseInputImageExtremaForScaling( false );
    rgbfilter->GetColormap()->SetMinimumInputValue( maskMinimumValue );
    rgbfilter->GetColormap()->SetMaximumInputValue( maskMaximumValue );
    }

  rgbfilter->GetColormap()->SetMinimumRGBComponentValue(
    ( argc > 9 ) ? static_cast<
    typename RGBPixelType::ComponentType>( atof( argv[9] ) ) : 0 );
  rgbfilter->GetColormap()->SetMaximumRGBComponentValue(
    ( argc > 10 ) ? static_cast<
    typename RGBPixelType::ComponentType>( atof( argv[10] ) ) : 255 );

  if( argc > 8 )
    {
    rgbfilter->SetUseInputImageExtremaForScaling( false );
    rgbfilter->GetColormap()->SetMinimumInputValue(
      static_cast<RealType>( atof( argv[7] ) ) );
    rgbfilter->GetColormap()->SetMaximumInputValue(
      static_cast<RealType>( atof( argv[8] ) ) );
    }

  try
    {
    rgbfilter->Update();
    }
  catch (...)
    {
    return EXIT_FAILURE;
    }

  if( maskImage )
    {
    itk::ImageRegionIterator<MaskImageType> ItM( maskImage,
      maskImage->GetLargestPossibleRegion() );
    itk::ImageRegionIterator<RGBImageType> ItC( rgbfilter->GetOutput(),
      rgbfilter->GetOutput()->GetLargestPossibleRegion() );
    itk::ImageRegionIterator<RealImageType> ItS( reader->GetOutput(),
      reader->GetOutput()->GetLargestPossibleRegion() );

    ItM.GoToBegin();
    ItC.GoToBegin();
    ItS.GoToBegin();

    while( !ItM.IsAtEnd() )
      {
      if( ItM.Get() == 0 )
        {
        RGBPixelType rgbpixel;

//        RealType minimumValue = rgbfilter->GetColormap()->GetMinimumInputValue();
//        RealType maximumValue = rgbfilter->GetColormap()->GetMaximumInputValue();
//
//        RealType minimumRGBValue
//          = rgbfilter->GetColormap()->GetMinimumRGBComponentValue();
//        RealType maximumRGBValue
//          = rgbfilter->GetColormap()->GetMaximumRGBComponentValue();
//
//        RealType ratio = ( ItS.Get() - minimumValue ) / ( maximumValue - minimumValue );
//
//        rgbpixel.Fill( ratio * ( maximumRGBValue - minimumRGBValue )
//          + minimumRGBValue );
        rgbpixel.Fill( itk::NumericTraits<typename RGBPixelType::ComponentType>::Zero );

        ItC.Set( rgbpixel );
        }
      ++ItM;
      ++ItC;
      ++ItS;
      }
    }

  typedef itk::ImageFileWriter<RGBImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rgbfilter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();

  return 0;
}

RcppExport SEXP ConvertScalarImageToRGB( SEXP r_args )
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


  if ( argc < 6 )
    {
    Rcpp::Rcout << "Usage: " << argv[0] << " imageDimension inputImage outputImage "
      << "mask colormap [customColormapFile] [minimumInput] [maximumInput] "
      << "[minimumRGBOutput] [maximumRGBOutput]" << std::endl;
    Rcpp::Rcout << "  Possible colormaps: grey, red, green, blue, copper, jet, hsv, ";
    Rcpp::Rcout << "spring, summer, autumn, winter, hot, cool, overunder, custom" << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     ConvertScalarImageToRGB<2>( argc, argv );
     break;
   case 3:
     ConvertScalarImageToRGB<3>( argc, argv );
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



