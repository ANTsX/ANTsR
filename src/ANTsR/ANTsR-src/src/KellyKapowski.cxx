
#include <Rcpp.h>

#include "antsCommandLineParser.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkDiReCTImageFilter.h"
#include "itkDiReCTImageFilter926.h"
#include "itkDiReCTImageFilter949.h"
#include "itkDiReCTImageFilter953.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTimeProbe.h"

#include <string>
#include <algorithm>
#include <vector>

template<class TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate   Self;
  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *) caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    const TFilter * filter =
      dynamic_cast< const TFilter * >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      { return; }
    Rcpp::Rcout << "  Iteration " << filter->GetElapsedIterations()
      << " (of "
      << filter->GetMaximumNumberOfIterations()
      << ").  ";
    Rcpp::Rcout << "Current energy = " << filter->GetCurrentEnergy() << ".  ";
    if( filter->GetElapsedIterations() >= filter->GetConvergenceWindowSize() )
      {
      Rcpp::Rcout << "(convergence value = "
        << filter->GetCurrentConvergenceMeasurement()
        << ", threshold = " << filter->GetConvergenceThreshold()
        << ")";
      }
    Rcpp::Rcout << std::endl;
    }
};

static void ConvertToLowerCase( std::string& str )
{
  std::transform( str.begin(), str.end(), str.begin(), tolower );
// You may need to cast the above line to (int(*)(int))
// tolower - this works as is on VC 7.1 but may not work on
// other compilers
}

template<unsigned int ImageDimension>
int DiReCT0( itk::ants::CommandLineParser *parser )
{
  typedef float RealType;
  typedef short LabelType;

  typedef itk::Image<LabelType, ImageDimension> LabelImageType;
  typename LabelImageType::Pointer segmentationImage = NULL;

  typedef itk::Image<RealType, ImageDimension> ImageType;
  typename ImageType::Pointer grayMatterProbabilityImage = NULL;
  typename ImageType::Pointer whiteMatterProbabilityImage = NULL;

  typedef itk::DiReCTImageFilter<LabelImageType, ImageType> DiReCTFilterType;
  typename DiReCTFilterType::Pointer direct = DiReCTFilterType::New();

  //
  // debugging information
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    debugOption = parser->GetOption( "print-debug-information" );
  if( debugOption && debugOption->GetNumberOfValues() > 0 )
    {
    std::string value = debugOption->GetValue();
    ConvertToLowerCase( value );
    if( std::strcmp( value.c_str(), "true" ) ||
      parser->Convert<int>( debugOption->GetValue() ) != 0 )
      {
      direct->DebugOn();
      }
    }

  //
  // segmentation image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    segmentationImageOption = parser->GetOption( "segmentation-image" );
  if( segmentationImageOption )
    {
    if( segmentationImageOption->GetNumberOfValues() > 0 )
      {
      if( segmentationImageOption->GetNumberOfParameters() == 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetValue();
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        }
      else if( segmentationImageOption->GetNumberOfParameters() > 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetParameter( 0 );
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        if( segmentationImageOption->GetNumberOfParameters() > 1 )
          {
          direct->SetGrayMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 1 ) ) );
          }
        if( segmentationImageOption->GetNumberOfParameters() > 2 )
          {
          direct->SetWhiteMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 2 ) ) );
          }
        }
      }
    }
  else
    {
    Rcpp::Rcout << "Segmentation image not specified." << std::endl;
    return EXIT_FAILURE;
    }
  direct->SetSegmentationImage( segmentationImage );

  //
  // gray matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    grayMatterOption = parser->GetOption( "gray-matter-probability-image" );
  if( grayMatterOption && grayMatterOption->GetNumberOfValues() > 0)
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer gmReader = ReaderType::New();

    std::string gmFile = grayMatterOption->GetValue();
    gmReader->SetFileName( gmFile.c_str() );

    grayMatterProbabilityImage = gmReader->GetOutput();
    grayMatterProbabilityImage->Update();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  Grey matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, LabelImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<LabelImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    grayMatterProbabilityImage = smoother->GetOutput();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetGrayMatterProbabilityImage( grayMatterProbabilityImage );

  //
  // white matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    whiteMatterOption = parser->GetOption( "white-matter-probability-image" );
  if( whiteMatterOption && whiteMatterOption->GetNumberOfValues() > 0 )
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer wmReader = ReaderType::New();

    std::string wmFile = whiteMatterOption->GetValue();
    wmReader->SetFileName( wmFile.c_str() );

    whiteMatterProbabilityImage = wmReader->GetOutput();
    whiteMatterProbabilityImage->Update();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  White matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, ImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    whiteMatterProbabilityImage = smoother->GetOutput();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetWhiteMatterProbabilityImage( whiteMatterProbabilityImage );

  //
  // convergence options
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer convergenceOption =
    parser->GetOption( "convergence" );
  if( convergenceOption )
    {
    if( convergenceOption->GetNumberOfParameters() > 0 )
      {
      direct->SetMaximumNumberOfIterations( parser->Convert<unsigned int>(
        convergenceOption->GetParameter( 0 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 1 )
      {
      direct->SetConvergenceThreshold( parser->Convert<float>(
					    convergenceOption->GetParameter( 1 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 2 )
      {
      direct->SetConvergenceWindowSize( parser->Convert<unsigned int>(
					    convergenceOption->GetParameter( 2 ) ) );
      }
    }

  //
  // thickness prior estimate
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    thicknessPriorOption = parser->GetOption( "thickness-prior-estimate" );
  if( thicknessPriorOption && thicknessPriorOption->GetNumberOfValues() > 0 )
    {
    direct->SetThicknessPriorEstimate( parser->Convert<RealType>(
      thicknessPriorOption->GetValue() ) );
    }

  //
  // gradient step
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    gradientStepOption = parser->GetOption( "gradient-step" );
  if( gradientStepOption && gradientStepOption->GetNumberOfValues() > 0 )
    {
    direct->SetInitialGradientStep( parser->Convert<RealType>(
      gradientStepOption->GetValue() ) );
    }

  //
  // smoothing sigma
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    smoothingSigmaOption = parser->GetOption( "smoothing-sigma" );
  if( smoothingSigmaOption && smoothingSigmaOption->GetNumberOfValues() > 0 )
    {
    direct->SetSmoothingSigma( parser->Convert<RealType>(
      smoothingSigmaOption->GetValue() ) );
    }

  //
  // set maximum number of threads
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    threadOption = parser->GetOption( "maximum-number-of-threads" );
  if( threadOption && threadOption->GetNumberOfValues() > 0 )
    {
    unsigned int numThreads = parser->Convert<unsigned int>(
      threadOption->GetValue() );
    direct->SetNumberOfThreads( numThreads );
    }

  typedef CommandIterationUpdate<DiReCTFilterType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  direct->AddObserver( itk::IterationEvent(), observer );

  itk::TimeProbe timer;
  try
    {
    //direct->DebugOn();
    timer.Start();
    direct->Update();
    timer.Stop();
    }
  catch( itk::ExceptionObject &e )
    {
    Rcpp::Rcout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  direct->Print( Rcpp::Rcout, 3 );

  Rcpp::Rcout << "DiReCT elapsed time: " << timer.GetMeanTime() << std::endl;

  /**
   * output
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( outputOption )
    {
    typedef  itk::ImageFileWriter<ImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput( direct->GetOutput() );
    writer->SetFileName( ( outputOption->GetValue() ).c_str() );
    writer->Update();
    }

  return EXIT_SUCCESS;
}

template<unsigned int ImageDimension>
int DiReCT1( itk::ants::CommandLineParser *parser )
{
  typedef float RealType;
  typedef short LabelType;

  typedef itk::Image<LabelType, ImageDimension> LabelImageType;
  typename LabelImageType::Pointer segmentationImage = NULL;

  typedef itk::Image<RealType, ImageDimension> ImageType;
  typename ImageType::Pointer grayMatterProbabilityImage = NULL;
  typename ImageType::Pointer whiteMatterProbabilityImage = NULL;

  typedef itk::DiReCTImageFilter926<LabelImageType, ImageType> DiReCTFilterType;
  typename DiReCTFilterType::Pointer direct = DiReCTFilterType::New();

  //
  // debugging information
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    debugOption = parser->GetOption( "print-debug-information" );
  if( debugOption && debugOption->GetNumberOfValues() > 0 )
    {
    std::string value = debugOption->GetValue();
    ConvertToLowerCase( value );
    if( std::strcmp( value.c_str(), "true" ) ||
      parser->Convert<int>( debugOption->GetValue() ) != 0 )
      {
      direct->DebugOn();
      }
    }

  //
  // segmentation image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    segmentationImageOption = parser->GetOption( "segmentation-image" );
  if( segmentationImageOption )
    {
    if( segmentationImageOption->GetNumberOfValues() > 0 )
      {
      if( segmentationImageOption->GetNumberOfParameters() == 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetValue();
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        }
      else if( segmentationImageOption->GetNumberOfParameters() > 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetParameter( 0 );
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        if( segmentationImageOption->GetNumberOfParameters() > 1 )
          {
          direct->SetGrayMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 1 ) ) );
          }
        if( segmentationImageOption->GetNumberOfParameters() > 2 )
          {
          direct->SetWhiteMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 2 ) ) );
          }
        }
      }
    }
  else
    {
    Rcpp::Rcout << "Segmentation image not specified." << std::endl;
    return EXIT_FAILURE;
    }
  direct->SetSegmentationImage( segmentationImage );

  //
  // gray matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    grayMatterOption = parser->GetOption( "gray-matter-probability-image" );
  if( grayMatterOption && grayMatterOption->GetNumberOfValues() > 0)
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer gmReader = ReaderType::New();

    std::string gmFile = grayMatterOption->GetValue();
    gmReader->SetFileName( gmFile.c_str() );

    grayMatterProbabilityImage = gmReader->GetOutput();
    grayMatterProbabilityImage->Update();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  Grey matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, LabelImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<LabelImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    grayMatterProbabilityImage = smoother->GetOutput();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetGrayMatterProbabilityImage( grayMatterProbabilityImage );

  //
  // white matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    whiteMatterOption = parser->GetOption( "white-matter-probability-image" );
  if( whiteMatterOption && whiteMatterOption->GetNumberOfValues() > 0 )
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer wmReader = ReaderType::New();

    std::string wmFile = whiteMatterOption->GetValue();
    wmReader->SetFileName( wmFile.c_str() );

    whiteMatterProbabilityImage = wmReader->GetOutput();
    whiteMatterProbabilityImage->Update();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  White matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, ImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    whiteMatterProbabilityImage = smoother->GetOutput();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetWhiteMatterProbabilityImage( whiteMatterProbabilityImage );

  //
  // convergence options
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer convergenceOption =
    parser->GetOption( "convergence" );
  if( convergenceOption )
    {
    if( convergenceOption->GetNumberOfParameters() > 0 )
      {
      direct->SetMaximumNumberOfIterations( parser->Convert<unsigned int>(
        convergenceOption->GetParameter( 0 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 1 )
      {
      direct->SetConvergenceThreshold( parser->Convert<float>(
					    convergenceOption->GetParameter( 1 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 2 )
      {
      direct->SetConvergenceWindowSize( parser->Convert<unsigned int>(
					    convergenceOption->GetParameter( 2 ) ) );
      }
    }

  //
  // thickness prior estimate
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    thicknessPriorOption = parser->GetOption( "thickness-prior-estimate" );
  if( thicknessPriorOption && thicknessPriorOption->GetNumberOfValues() > 0 )
    {
    direct->SetThicknessPriorEstimate( parser->Convert<RealType>(
      thicknessPriorOption->GetValue() ) );
    }

  //
  // gradient step
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    gradientStepOption = parser->GetOption( "gradient-step" );
  if( gradientStepOption && gradientStepOption->GetNumberOfValues() > 0 )
    {
    direct->SetGradientStep( parser->Convert<RealType>(
      gradientStepOption->GetValue() ) );
    }

  //
  // smoothing sigma
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    smoothingSigmaOption = parser->GetOption( "smoothing-sigma" );
  if( smoothingSigmaOption && smoothingSigmaOption->GetNumberOfValues() > 0 )
    {
    direct->SetSmoothingSigma( parser->Convert<RealType>(
      smoothingSigmaOption->GetValue() ) );
    }

  //
  // set maximum number of threads
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    threadOption = parser->GetOption( "maximum-number-of-threads" );
  if( threadOption && threadOption->GetNumberOfValues() > 0 )
    {
    unsigned int numThreads = parser->Convert<unsigned int>(
      threadOption->GetValue() );
    direct->SetNumberOfThreads( numThreads );
    }

  typedef CommandIterationUpdate<DiReCTFilterType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  direct->AddObserver( itk::IterationEvent(), observer );

  itk::TimeProbe timer;
  try
    {
    //direct->DebugOn();
    timer.Start();
    direct->Update();
    timer.Stop();
    }
  catch( itk::ExceptionObject &e )
    {
    Rcpp::Rcout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }
  direct->Print( Rcpp::Rcout, 3 );

  Rcpp::Rcout << "DiReCT elapsed time: " << timer.GetMeanTime() << std::endl;

  /**
   * output
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( outputOption )
    {
    typedef  itk::ImageFileWriter<ImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput( direct->GetOutput() );
    writer->SetFileName( ( outputOption->GetValue() ).c_str() );
    writer->Update();
    }

  return EXIT_SUCCESS;
}

template<unsigned int ImageDimension>
int DiReCT2( itk::ants::CommandLineParser *parser )
{
  typedef float RealType;
  typedef short LabelType;

  typedef itk::Image<LabelType, ImageDimension> LabelImageType;
  typename LabelImageType::Pointer segmentationImage = NULL;

  typedef itk::Image<RealType, ImageDimension> ImageType;
  typename ImageType::Pointer grayMatterProbabilityImage = NULL;
  typename ImageType::Pointer whiteMatterProbabilityImage = NULL;

  typedef itk::DiReCTImageFilter949<LabelImageType, ImageType> DiReCTFilterType;
  typename DiReCTFilterType::Pointer direct = DiReCTFilterType::New();

  //
  // debugging information
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    debugOption = parser->GetOption( "print-debug-information" );
  if( debugOption && debugOption->GetNumberOfValues() > 0 )
    {
    std::string value = debugOption->GetValue();
    ConvertToLowerCase( value );
    if( std::strcmp( value.c_str(), "true" ) ||
      parser->Convert<int>( debugOption->GetValue() ) != 0 )
      {
      direct->DebugOn();
      }
    }

  //
  // segmentation image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    segmentationImageOption = parser->GetOption( "segmentation-image" );
  if( segmentationImageOption )
    {
    if( segmentationImageOption->GetNumberOfValues() > 0 )
      {
      if( segmentationImageOption->GetNumberOfParameters() == 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetValue();
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        }
      else if( segmentationImageOption->GetNumberOfParameters() > 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetParameter( 0 );
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        if( segmentationImageOption->GetNumberOfParameters() > 1 )
          {
          direct->SetGrayMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 1 ) ) );
          }
        if( segmentationImageOption->GetNumberOfParameters() > 2 )
          {
          direct->SetWhiteMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 2 ) ) );
          }
        }
      }
    }
  else
    {
    Rcpp::Rcout << "Segmentation image not specified." << std::endl;
    return EXIT_FAILURE;
    }
  direct->SetSegmentationImage( segmentationImage );

  //
  // gray matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    grayMatterOption = parser->GetOption( "gray-matter-probability-image" );
  if( grayMatterOption && grayMatterOption->GetNumberOfValues() > 0)
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer gmReader = ReaderType::New();

    std::string gmFile = grayMatterOption->GetValue();
    gmReader->SetFileName( gmFile.c_str() );

    grayMatterProbabilityImage = gmReader->GetOutput();
    grayMatterProbabilityImage->Update();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  Grey matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, LabelImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<LabelImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    grayMatterProbabilityImage = smoother->GetOutput();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetGrayMatterProbabilityImage( grayMatterProbabilityImage );

  //
  // white matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    whiteMatterOption = parser->GetOption( "white-matter-probability-image" );
  if( whiteMatterOption && whiteMatterOption->GetNumberOfValues() > 0 )
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer wmReader = ReaderType::New();

    std::string wmFile = whiteMatterOption->GetValue();
    wmReader->SetFileName( wmFile.c_str() );

    whiteMatterProbabilityImage = wmReader->GetOutput();
    whiteMatterProbabilityImage->Update();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  White matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, ImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    whiteMatterProbabilityImage = smoother->GetOutput();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetWhiteMatterProbabilityImage( whiteMatterProbabilityImage );

  //
  // convergence options
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer convergenceOption =
    parser->GetOption( "convergence" );
  if( convergenceOption )
    {
    if( convergenceOption->GetNumberOfParameters() > 0 )
      {
      direct->SetMaximumNumberOfIterations( parser->Convert<unsigned int>(
        convergenceOption->GetParameter( 0 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 1 )
      {
      direct->SetConvergenceThreshold( parser->Convert<float>(
					    convergenceOption->GetParameter( 1 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 2 )
      {
      direct->SetConvergenceWindowSize( parser->Convert<unsigned int>(
					    convergenceOption->GetParameter( 2 ) ) );
      }
    }
  //
  // thickness prior estimate
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    thicknessPriorOption = parser->GetOption( "thickness-prior-estimate" );
  if( thicknessPriorOption && thicknessPriorOption->GetNumberOfValues() > 0 )
    {
    direct->SetThicknessPriorEstimate( parser->Convert<RealType>(
      thicknessPriorOption->GetValue() ) );
    }

  //
  // gradient step
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    gradientStepOption = parser->GetOption( "gradient-step" );
  if( gradientStepOption && gradientStepOption->GetNumberOfValues() > 0 )
    {
    direct->SetGradientStep( parser->Convert<RealType>(
      gradientStepOption->GetValue() ) );
    }

  //
  // smoothing sigma
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    smoothingSigmaOption = parser->GetOption( "smoothing-sigma" );
  if( smoothingSigmaOption && smoothingSigmaOption->GetNumberOfValues() > 0 )
    {
    direct->SetSmoothingSigma( parser->Convert<RealType>(
      smoothingSigmaOption->GetValue() ) );
    }

  //
  // set maximum number of threads
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    threadOption = parser->GetOption( "maximum-number-of-threads" );
  if( threadOption && threadOption->GetNumberOfValues() > 0 )
    {
    unsigned int numThreads = parser->Convert<unsigned int>(
      threadOption->GetValue() );
    direct->SetNumberOfThreads( numThreads );
    }

  typedef CommandIterationUpdate<DiReCTFilterType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  direct->AddObserver( itk::IterationEvent(), observer );

  itk::TimeProbe timer;
  try
    {
    //direct->DebugOn();
    timer.Start();
    direct->Update();
    timer.Stop();
    }
  catch( itk::ExceptionObject &e )
    {
    Rcpp::Rcout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }
  direct->Print( Rcpp::Rcout, 3 );

  Rcpp::Rcout << "DiReCT elapsed time: " << timer.GetMeanTime() << std::endl;

  /**
   * output
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( outputOption )
    {
    typedef  itk::ImageFileWriter<ImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput( direct->GetOutput() );
    writer->SetFileName( ( outputOption->GetValue() ).c_str() );
    writer->Update();
    }

  return EXIT_SUCCESS;
}

template<unsigned int ImageDimension>
int DiReCT3( itk::ants::CommandLineParser *parser )
{
  typedef float RealType;
  typedef short LabelType;

  typedef itk::Image<LabelType, ImageDimension> LabelImageType;
  typename LabelImageType::Pointer segmentationImage = NULL;

  typedef itk::Image<RealType, ImageDimension> ImageType;
  typename ImageType::Pointer grayMatterProbabilityImage = NULL;
  typename ImageType::Pointer whiteMatterProbabilityImage = NULL;

  typedef itk::DiReCTImageFilter953<LabelImageType, ImageType> DiReCTFilterType;
  typename DiReCTFilterType::Pointer direct = DiReCTFilterType::New();

  //
  // debugging information
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    debugOption = parser->GetOption( "print-debug-information" );
  if( debugOption && debugOption->GetNumberOfValues() > 0 )
    {
    std::string value = debugOption->GetValue();
    ConvertToLowerCase( value );
    if( std::strcmp( value.c_str(), "true" ) ||
      parser->Convert<int>( debugOption->GetValue() ) != 0 )
      {
      direct->DebugOn();
      }
    }

  //
  // segmentation image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    segmentationImageOption = parser->GetOption( "segmentation-image" );
  if( segmentationImageOption )
    {
    if( segmentationImageOption->GetNumberOfValues() > 0 )
      {
      if( segmentationImageOption->GetNumberOfParameters() == 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetValue();
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        }
      else if( segmentationImageOption->GetNumberOfParameters() > 0 )
        {
        typedef itk::ImageFileReader<LabelImageType> LabelReaderType;
        typename LabelReaderType::Pointer labelReader = LabelReaderType::New();

        std::string inputFile = segmentationImageOption->GetParameter( 0 );
        labelReader->SetFileName( inputFile.c_str() );

        segmentationImage = labelReader->GetOutput();
        segmentationImage->Update();
        segmentationImage->DisconnectPipeline();
        if( segmentationImageOption->GetNumberOfParameters() > 1 )
          {
          direct->SetGrayMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 1 ) ) );
          }
        if( segmentationImageOption->GetNumberOfParameters() > 2 )
          {
          direct->SetWhiteMatterLabel( parser->Convert<LabelType>(
            segmentationImageOption->GetParameter( 2 ) ) );
          }
        }
      }
    }
  else
    {
    Rcpp::Rcout << "Segmentation image not specified." << std::endl;
    return EXIT_FAILURE;
    }
  direct->SetSegmentationImage( segmentationImage );

  //
  // gray matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    grayMatterOption = parser->GetOption( "gray-matter-probability-image" );
  if( grayMatterOption && grayMatterOption->GetNumberOfValues() > 0)
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer gmReader = ReaderType::New();

    std::string gmFile = grayMatterOption->GetValue();
    gmReader->SetFileName( gmFile.c_str() );

    grayMatterProbabilityImage = gmReader->GetOutput();
    grayMatterProbabilityImage->Update();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  Grey matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, LabelImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetGrayMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<LabelImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    grayMatterProbabilityImage = smoother->GetOutput();
    grayMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetGrayMatterProbabilityImage( grayMatterProbabilityImage );

  //
  // white matter probability image
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    whiteMatterOption = parser->GetOption( "white-matter-probability-image" );
  if( whiteMatterOption && whiteMatterOption->GetNumberOfValues() > 0 )
    {
    typedef itk::ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer wmReader = ReaderType::New();

    std::string wmFile = whiteMatterOption->GetValue();
    wmReader->SetFileName( wmFile.c_str() );

    whiteMatterProbabilityImage = wmReader->GetOutput();
    whiteMatterProbabilityImage->Update();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  else
    {
    Rcpp::Rcout << "  White matter probability image not specified. "
      << "Creating one from the segmentation image." << std::endl << std::endl;

    typedef itk::BinaryThresholdImageFilter<LabelImageType, ImageType>
      ThresholderType;
    typename ThresholderType::Pointer thresholder = ThresholderType::New();
    thresholder->SetInput( segmentationImage );
    thresholder->SetLowerThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetUpperThreshold( direct->GetWhiteMatterLabel() );
    thresholder->SetInsideValue( 1 );
    thresholder->SetOutsideValue( 0 );

    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> SmootherType;
    typename SmootherType::Pointer smoother = SmootherType::New();
    smoother->SetVariance( 1.0 );
    smoother->SetUseImageSpacingOn();
    smoother->SetMaximumError( 0.01 );
    smoother->SetInput( thresholder->GetOutput() );
    smoother->Update();

    whiteMatterProbabilityImage = smoother->GetOutput();
    whiteMatterProbabilityImage->DisconnectPipeline();
    }
  direct->SetWhiteMatterProbabilityImage( whiteMatterProbabilityImage );

  //
  // convergence options
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer convergenceOption =
    parser->GetOption( "convergence" );
  if( convergenceOption )
    {
    if( convergenceOption->GetNumberOfParameters() > 0 )
      {
      direct->SetMaximumNumberOfIterations( parser->Convert<unsigned int>(
        convergenceOption->GetParameter( 0 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 1 )
      {
      direct->SetConvergenceThreshold( parser->Convert<float>(
					    convergenceOption->GetParameter( 1 ) ) );
      }
    if( convergenceOption->GetNumberOfParameters() > 2 )
      {
      direct->SetConvergenceWindowSize( parser->Convert<unsigned int>(
					    convergenceOption->GetParameter( 2 ) ) );
      }
    }

  //
  // thickness prior estimate
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    thicknessPriorOption = parser->GetOption( "thickness-prior-estimate" );
  if( thicknessPriorOption && thicknessPriorOption->GetNumberOfValues() > 0 )
    {
    direct->SetThicknessPriorEstimate( parser->Convert<RealType>(
      thicknessPriorOption->GetValue() ) );
    }

  //
  // gradient step
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    gradientStepOption = parser->GetOption( "gradient-step" );
  if( gradientStepOption && gradientStepOption->GetNumberOfValues() > 0 )
    {
    direct->SetGradientStep( parser->Convert<RealType>(
      gradientStepOption->GetValue() ) );
    }

  //
  // smoothing sigma
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    smoothingSigmaOption = parser->GetOption( "smoothing-sigma" );
  if( smoothingSigmaOption && smoothingSigmaOption->GetNumberOfValues() > 0 )
    {
    direct->SetSmoothingSigma( parser->Convert<RealType>(
      smoothingSigmaOption->GetValue() ) );
    }

  //
  // set maximum number of threads
  //
  typename itk::ants::CommandLineParser::OptionType::Pointer
    threadOption = parser->GetOption( "maximum-number-of-threads" );
  if( threadOption && threadOption->GetNumberOfValues() > 0 )
    {
    unsigned int numThreads = parser->Convert<unsigned int>(
      threadOption->GetValue() );
    direct->SetNumberOfThreads( numThreads );
    }

  typedef CommandIterationUpdate<DiReCTFilterType> CommandType;
  typename CommandType::Pointer observer = CommandType::New();
  direct->AddObserver( itk::IterationEvent(), observer );

  itk::TimeProbe timer;
  try
    {
    //direct->DebugOn();
    timer.Start();
    direct->Update();
    timer.Stop();
    }
  catch( itk::ExceptionObject &e )
    {
    Rcpp::Rcout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }
  direct->Print( Rcpp::Rcout, 3 );

  Rcpp::Rcout << "DiReCT elapsed time: " << timer.GetMeanTime() << std::endl;

  /**
   * output
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( outputOption )
    {
    typedef  itk::ImageFileWriter<ImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput( direct->GetOutput() );
    writer->SetFileName( ( outputOption->GetValue() ).c_str() );
    writer->Update();
    }

  return EXIT_SUCCESS;
}

static void InitializeCommandLineOptions( itk::ants::CommandLineParser *parser )
{
  typedef itk::ants::CommandLineParser::OptionType OptionType;

  {
  std::string description =
    std::string( "This option forces the image to be treated as a specified-" ) +
    std::string( "dimensional image.  If not specified, DiReCT tries to " ) +
    std::string( "infer the dimensionality from the input image." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "image-dimensionality" );
  option->SetShortName( 'd' );
  option->SetUsageOption( 0, "2/3" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "A segmentation image must be supplied labeling the gray" ) +
    std::string( "and white matters.  Ddefault values = 2 and 3, respectively." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "segmentation-image" );
  option->SetShortName( 's' );
  option->SetUsageOption( 0, "imageFilename" );
  option->SetUsageOption( 1, "[imageFilename,<grayMatterLabel=2>,<whiteMatterLabel=3>]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "In addition to the segmentation image, a gray matter " ) +
    std::string( "probability image can be used. If no such image is " ) +
    std::string( "supplied, one is created using the segmentation image " ) +
    std::string( "and a variance of 1.0 mm." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "gray-matter-probability-image" );
  option->SetShortName( 'g' );
  option->SetUsageOption( 0, "imageFilename" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "In addition to the segmentation image, a white matter " ) +
    std::string( "probability image can be used. If no such image is " ) +
    std::string( "supplied, one is created using the segmentation image " ) +
    std::string( "and a variance of 1.0 mm." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "white-matter-probability-image" );
  option->SetShortName( 'w' );
  option->SetUsageOption( 0, "imageFilename" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Convergence is determined by fitting a line to the normalized energy " ) +
    std::string( "profile of the last N iterations (where N is specified by " ) +
    std::string( "the window size) and determining the slope which is then " ) +
    std::string( "compared with the convergence threshold." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "convergence" );
  option->SetShortName( 'c' );
  option->SetUsageOption( 0, "[<numberOfIterations=50>,<convergenceThreshold=0.001>,<convergenceWindowSize=10>]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Provides a prior constraint on the final thickness measurement. Default = 10 mm." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "thickness-prior-estimate" );
  option->SetShortName( 't' );
  option->SetUsageOption( 0, "thicknessPriorEstimate" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Gradient step size for the optimization.  Default = 0.5." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "gradient-step" );
  option->SetShortName( 'r' );
  option->SetUsageOption( 0, "stepSize" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "smoothing-sigma" );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "smoothing-sigma" );
  option->SetShortName( 'm' );
  option->SetUsageOption( 0, "sigma" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "The output consists of a thickness map defined in the " ) +
    std::string( "segmented gray matter. " );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "output" );
  option->SetShortName( 'o' );
  option->SetUsageOption( 0, "imageFilename" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Boolean option to print out the debug information.  " ) +
    std::string( "ITK and ANTs must be compiled in debug or RelWithDebInfo." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "print-debug-information" );
  option->SetShortName( 'p' );
  option->SetUsageOption( 0, "1" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Option to set the maximum number of threads.  If this " ) +
    std::string( "option is not set, it defaults to the ITK filter behavior." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "maximum-number-of-threads" );
  option->SetShortName( 'e' );
  option->SetUsageOption( 0, "numberOfThreads" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Option to set the DiReCT Image Filter svn revision " ) +
    std::string( "(0, 926, 949, 953).  Version 926 is most likely the " ) +
    std::string( "best option but it comes at the cost of a larger memory footprint. " ) +
    std::string( "Memory-saving strategies were employed for version " ) +
    std::string( "949 but at a computational speed cost.  Additional " ) +
    std::string( "memory-saving strategies were used in version 953." ) +
    std::string( "If this option is not set, it defaults to the latest " ) +
    std::string( "DiReCT revision (0) which is currently the same as 926." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "DiReCT-revision" );
  option->SetShortName( 'z' );
  option->SetUsageOption( 0, "0, 926, 949, or 953" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description = std::string( "Print the help menu (short version)." );

  OptionType::Pointer option = OptionType::New();
  option->SetShortName( 'h' );
  option->SetDescription( description );
  option->AddValue( std::string( "0" ) );
  parser->AddOption( option );
  }

  {
  std::string description = std::string( "Print the help menu." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "help" );
  option->SetDescription( description );
  option->AddValue( std::string( "0" ) );
  parser->AddOption( option );
  }

}

RcppExport SEXP KellyKapowski( SEXP r_args )
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


  itk::ants::CommandLineParser::Pointer parser =
    itk::ants::CommandLineParser::New();
  parser->SetCommand( argv[0] );

  std::string commandDescription =
    std::string( "DiReCT is a registration based estimate of cortical " ) +
    std::string( "thickness.  It was published in S. R. Das, B. B. " ) +
    std::string( "Avants, M. Grossman, and J. C. Gee, Registration based " ) +
    std::string( "cortical thickness measurement, Neuroimage 2009, " ) +
    std::string( "45:867--879." );

  parser->SetCommandDescription( commandDescription );
  InitializeCommandLineOptions( parser );

  parser->Parse( argc, argv );

  if( argc < 2 || parser->Convert<bool>(
    parser->GetOption( "help" )->GetValue() ) )
    {
    parser->PrintMenu( Rcpp::Rcout, 5, false );
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }
  else if( parser->Convert<bool>(
    parser->GetOption( 'h' )->GetValue() ) )
    {
    parser->PrintMenu( Rcpp::Rcout, 5, true );
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  // Get dimensionality
  unsigned int dimension = 3;

  itk::ants::CommandLineParser::OptionType::Pointer dimOption =
    parser->GetOption( "image-dimensionality" );
  if( dimOption && dimOption->GetNumberOfValues() > 0 )
    {
      dimension = parser->Convert<unsigned int>( dimOption->GetValue() );
    }
  else
    {
    // Read in the first intensity image to get the image dimension.
    std::string filename;

    itk::ants::CommandLineParser::OptionType::Pointer imageOption =
      parser->GetOption( "input-image" );
    if( imageOption && imageOption->GetNumberOfValues() > 0 )
      {
      if( imageOption->GetNumberOfParameters( 0 ) > 0 )
        {
        filename = imageOption->GetParameter( 0, 0 );
        }
      else
        {
        filename = imageOption->GetValue( 0 );
        }
      }
    else
      {
      Rcpp::Rcout << "No input images were specified.  Specify an input "
        << " segmentation image with the -s option" << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
      }
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
        filename.c_str(), itk::ImageIOFactory::ReadMode );
    dimension = imageIO->GetNumberOfDimensions();
    }

  unsigned int whichDiReCTVersion = 0;
  itk::ants::CommandLineParser::OptionType::Pointer directOption =
    parser->GetOption( "DiReCT-revision" );
  if( directOption && directOption->GetNumberOfValues() > 0 )
    {
    whichDiReCTVersion = parser->Convert<unsigned int>( directOption->GetValue() );
    if( whichDiReCTVersion != 953 && whichDiReCTVersion != 949 &&
      whichDiReCTVersion != 926 && whichDiReCTVersion != 0 )
      {
      Rcpp::Rcout << "Unknown requested DiReCT version. See long help menu "
        << "for the description of the different versions (i.e. --help )." << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
      }
    }

  Rcpp::Rcout << std::endl << "Running DiReCT (revision " << whichDiReCTVersion
    << ") for " << dimension << "-dimensional images." << std::endl << std::endl;

  switch( dimension )
   {
   case 2:
     {
     switch( whichDiReCTVersion )
       {
       case 0:
         {
         DiReCT0<2>( parser );
         break;
         }
       case 926: default:
         {
         DiReCT1<2>( parser );
         break;
         }
       case 949:
         {
         DiReCT2<2>( parser );
         break;
         }
       case 953:
         {
         DiReCT3<2>( parser );
         break;
         }
       }
     break;
     }
   case 3:
     {
     switch( whichDiReCTVersion )
       {
       case 0:
         {
         DiReCT0<3>( parser );
         break;
         }
       case 926: default:
         {
         DiReCT1<3>( parser );
         break;
         }
       case 949:
         {
         DiReCT2<3>( parser );
         break;
         }
       case 953:
         {
         DiReCT3<3>( parser );
         break;
         }
       }
     break;
     }
   default:
     {
     Rcpp::Rcout << "Unsupported dimension" << std::endl;
     return Rcpp::wrap( EXIT_FAILURE ) ;
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


