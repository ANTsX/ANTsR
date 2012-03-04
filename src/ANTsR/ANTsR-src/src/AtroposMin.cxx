
#include <Rcpp.h>

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "antsAtroposSegmentationImageFilter.h"
#include "antsCommandLineOption.h"
#include "antsCommandLineParser.h"
#include "antsJointHistogramParzenShapeAndOrientationListSampleFunction.h"

#include <string>
#include <algorithm>
#include <vector>

static void ConvertToLowerCase( std::string& str )
{
  std::transform( str.begin(), str.end(), str.begin(), tolower );
// You may need to cast the above line to (int(*)(int))
// tolower - this works as is on VC 7.1 but may not work on
// other compilers
}

template <unsigned int ImageDimension>
int AtroposSegmentation( itk::ants::CommandLineParser *parser )
{
  typedef float PixelType;
  typedef float RealType;
  typedef itk::Image<PixelType, ImageDimension>  InputImageType;

  typedef unsigned int LabelType;
  typedef itk::Image<LabelType, ImageDimension> LabelImageType;

  typedef  itk::ants::AtroposSegmentationImageFilter
    <InputImageType, LabelImageType> SegmentationFilterType;
  typename SegmentationFilterType::Pointer segmenter
    = SegmentationFilterType::New();

  /**
   * mask image
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer maskOption =
    parser->GetOption( "mask-image" );
  if( maskOption && maskOption->GetNumberOfValues() > 0 )
    {
    try
      {
      typedef  itk::ImageFileReader<LabelImageType> ReaderType;
      typename ReaderType::Pointer reader = ReaderType::New();
      reader->SetFileName( ( maskOption->GetValue() ).c_str() );
      reader->Update();

      segmenter->SetMaskImage( reader->GetOutput() );
      }
    catch(...) {}
    }
  else
    {
    Rcpp::Rcout << "An image mask is required.  Specify a mask image"
      << " with the -x option." << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * intensity images
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer imageOption =
    parser->GetOption( "intensity-image" );
  if( imageOption && imageOption->GetNumberOfValues() > 0 )
    {
    unsigned int count = 0;
    for( int n = imageOption->GetNumberOfValues() - 1; n >= 0; n-- )
      {
      typedef itk::ImageFileReader<InputImageType> ReaderType;
      typename ReaderType::Pointer reader = ReaderType::New();
      if( imageOption->GetNumberOfParameters( n ) > 0 )
        {
        reader->SetFileName( imageOption->GetParameter( n, 0 ) );
        }
      else
        {
        reader->SetFileName( imageOption->GetValue( n ) );
        }
      reader->Update();

      segmenter->SetIntensityImage( count, reader->GetOutput() );
      if( imageOption->GetNumberOfParameters( count ) > 1 )
        {
        segmenter->SetAdaptiveSmoothingWeight( count, parser->Convert<float>(
          imageOption->GetParameter( count, 1 ) ) );
        }
      else
        {
        segmenter->SetAdaptiveSmoothingWeight( count, 0.0 );
        }
      count++;
      }
    }
  else
    {
    Rcpp::Rcout << "No input images were specified.  Specify an input image"
      << " with the -a option." << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * likelihood
   */
  typename itk::ants::CommandLineParser::OptionType::Pointer likelihoodOption =
    parser->GetOption( "likelihood-model" );
  if( likelihoodOption && likelihoodOption->GetNumberOfValues() > 0 )
    {
    std::string likelihoodModel = likelihoodOption->GetValue();
    ConvertToLowerCase( likelihoodModel );
    if( !likelihoodModel.compare( std::string( "jointshapeandorientationprobability" ) ) )
      {
      if( segmenter->GetNumberOfIntensityImages() !=
        static_cast<unsigned int>( ImageDimension * ( ImageDimension + 1 ) / 2 ) )
        {
     	  Rcpp::Rcout << " Expect images in upper triangular order " << std::endl;
     	  Rcpp::Rcout << " xx xy xz yy yz zz " <<std::endl;
        Rcpp::Rcout << "Incorrect number of intensity images specified." << std::endl;
        return EXIT_FAILURE;
        }
      typedef typename SegmentationFilterType::SampleType SampleType;
      typedef itk::ants::Statistics::
        JointHistogramParzenShapeAndOrientationListSampleFunction
        <SampleType, float, float> LikelihoodType;

      float sigma = 1.0;
      if( likelihoodOption->GetNumberOfParameters() > 0 )
        {
        sigma = parser->Convert<float>(
          likelihoodOption->GetParameter( 0 ) );
        }
      unsigned int numberOfBins = 32;
      if( likelihoodOption->GetNumberOfParameters() > 1 )
        {
        numberOfBins = parser->Convert<unsigned int>(
          likelihoodOption->GetParameter( 1 ) );
        }

      for( unsigned int n = 0; n < segmenter->GetNumberOfTissueClasses(); n++ )
        {
        typename LikelihoodType::Pointer hpwLikelihood =
          LikelihoodType::New();
        hpwLikelihood->SetSigma( sigma );
        hpwLikelihood->SetNumberOfJointHistogramBins( numberOfBins );
        segmenter->SetLikelihoodFunction( n, hpwLikelihood );
        }
      }
    else
      {
      Rcpp::Rcout << "Unrecognized likelihood model request." << std::endl;
      return EXIT_FAILURE;
      }
    }

//  Rcpp::Rcout << std::endl << "Writing output:" << std::endl;
//  typename itk::ants::CommandLineParser::OptionType::Pointer outputOption =
//    parser->GetOption( "output" );
//  if( outputOption && outputOption->GetNumberOfValues() > 0 )
//    {
//    typedef  itk::ImageFileWriter<ImageType> WriterType;
//    typename WriterType::Pointer writer = WriterType::New();
//    writer->SetInput( segmenter->GetOutput() );
//    writer->SetFileName( ( outputOption->GetValue() ).c_str() );
//    writer->Update();
//    }

  Rcpp::Rcout << std::endl;
  segmenter->Print( Rcpp::Rcout, 2 );

  return EXIT_SUCCESS;
}

static void InitializeCommandLineOptions( itk::ants::CommandLineParser *parser )
{
  typedef itk::ants::CommandLineParser::OptionType OptionType;

  {
  std::string description =
    std::string( "This option forces the image to be treated as a specified-" ) +
    std::string( "dimensional image.  If not specified, Atropos tries to " ) +
    std::string( "infer the dimensionality from the first input image." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "image-dimensionality" );
  option->SetShortName( 'd' );
  option->SetUsageOption( 0, "2/3/4" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "One or more scalar images is specified for segmentation " ) +
    std::string( "using the -a/--intensity-image option.  For segmentation " ) +
    std::string( "scenarios with no prior information, the first scalar " ) +
    std::string( "image encountered on the command line is used to order " ) +
    std::string( "labelings such that the class with the smallest intensity " ) +
    std::string( "signature is class \'1\' through class \'N\' which represents " ) +
    std::string( "the voxels with the largest intensity values.  The " ) +
    std::string( "optional adaptive smoothing weight parameter is applicable " ) +
    std::string( "only when using prior label or probability images.  This " ) +
    std::string( "scalar parameter is to be specified between [0,1] which " ) +
    std::string( "smooths each labeled region separately and modulates the " ) +
    std::string( "intensity measurement at each voxel in each intensity image " ) +
    std::string( "between the original intensity and its smoothed " ) +
    std::string( "counterpart.  The smoothness parameters are governed by the " ) +
    std::string( "-b/--bspline option." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "intensity-image" );
  option->SetShortName( 'a' );
  option->SetUsageOption( 0, "[intensityImage,<adaptiveSmoothingWeight>]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "The image mask (which is required) defines the region which " ) +
    std::string( "is to be labeled by the Atropos algorithm." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "mask-image" );
  option->SetShortName( 'x' );
  option->SetUsageOption( 0, "maskImageFilename" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Both parametric and non-parametric options exist in Atropos. " ) +
    std::string( "The Gaussian parametric option is commonly used " ) +
    std::string( "(e.g. SPM & FAST) where the mean and standard deviation " ) +
    std::string( "for the Gaussian of each class is calculated at each " ) +
    std::string( "iteration.  Other groups use non-parametric approaches " ) +
    std::string( "exemplified by option 2.  We recommend using options 1 " ) +
    std::string( "or 2 as they are fairly standard and the " ) +
    std::string( "default parameters work adequately." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "likelihood-model" );
  option->SetShortName( 'k' );
  option->SetUsageOption( 0, "JointShapeAndOrientationProbability[<sigma=1.0>,<numberOfBins=32>]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "The output consists of a labeled image where each voxel " ) +
    std::string( "in the masked region is assigned a label from 1, 2, " ) +
    std::string( "..., N.  Optionally, one can also output the posterior " ) +
    std::string( "probability images specified in the same format as the " ) +
    std::string( "prior probability images, e.g. posterior%02d.nii.gz " ) +
    std::string( "(C-style file name formatting)." );

  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "output" );
  option->SetShortName( 'o' );
  option->SetUsageOption( 0, "[classifiedImage,<posteriorProbabilityImageFileNameFormat>]" );
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

RcppExport SEXP AtroposMin( SEXP r_args )
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
    std::string( "A finite mixture modeling (FMM) segmentation approach " ) +
    std::string( "with possibilities for specifying prior constraints. " ) +
    std::string( "These prior constraints include the specification " ) +
    std::string( "of a prior label image, prior probability images " ) +
    std::string( "(one for each class), and/or an MRF prior to " ) +
    std::string( "enforce spatial smoothing of the labels.  Similar algorithms " ) +
    std::string( "include FAST and SPM.  " );

  parser->SetCommandDescription( commandDescription );
  InitializeCommandLineOptions( parser );

  parser->Parse( argc, argv );

  if( argc < 2 || parser->Convert<bool>(
    parser->GetOption( "help" )->GetValue() ) )
    {
    parser->PrintMenu( Rcpp::Rcout, 5, false );
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }
  else if( parser->GetOption( 'h' ) &&
    parser->Convert<bool>( parser->GetOption( 'h' )->GetValue() ) )
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
      parser->GetOption( "intensity-image" );
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
      Rcpp::Rcout << "No input images were specified.  Specify an input image"
        << " with the -a option" << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
      }
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
        filename.c_str(), itk::ImageIOFactory::ReadMode );
    dimension = imageIO->GetNumberOfDimensions();
    }

  Rcpp::Rcout << std::endl << "Running Atropos for "
    << dimension << "-dimensional images." << std::endl;

  switch( dimension )
   {
   case 2:
     AtroposSegmentation<2>( parser );
     break;
   case 3:
     AtroposSegmentation<3>( parser );
     break;
   case 4:
     AtroposSegmentation<4>( parser );
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



