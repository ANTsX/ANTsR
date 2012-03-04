
#include <Rcpp.h>

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
//#include "itkVectorImageFileReader.h"
#include "itkVector.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkWarpImageMultiTransformFilter.h"
#include "itkGridImageSource.h"

template<class TValue>
TValue Convert( std::string optionString )
			{
			TValue value;
			std::istringstream iss( optionString );
			iss >> value;
			return value;
			}

template<class TValue>
std::vector<TValue> ConvertVector( std::string optionString )
			{
			std::vector<TValue> values;
			std::string::size_type crosspos = optionString.find( 'x', 0 );

			if ( crosspos == std::string::npos )
					{
					values.push_back( Convert<TValue>( optionString ) );
					}
			else
					{
					std::string element = optionString.substr( 0, crosspos ) ;
					TValue value;
					std::istringstream iss( element );
					iss >> value;
					values.push_back( value );
					while ( crosspos != std::string::npos )
							{
							std::string::size_type crossposfrom = crosspos;
							crosspos = optionString.find( 'x', crossposfrom + 1 );
							if ( crosspos == std::string::npos )
									{
									element = optionString.substr( crossposfrom + 1, optionString.length() );
									}
							else
									{
									element = optionString.substr( crossposfrom + 1, crosspos ) ;
									}
							std::istringstream iss( element );
							iss >> value;
							values.push_back( value );
							}
					}
			return values;
			}


template <unsigned int ImageDimension>
int CreateWarpedGridImage( int argc, char *argv[] )
{

  typedef float RealType;
  typedef itk::Image<RealType, ImageDimension> RealImageType;
  typedef itk::Vector<RealType, ImageDimension> VectorType;
  typedef itk::Image<VectorType, ImageDimension> VectorImageType;

  /**
   * Read in vector field
   */
  typedef itk::ImageFileReader<VectorImageType> ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );
  reader->Update();

  typedef itk::GridImageSource<RealImageType> GridSourceType;
  typename GridSourceType::Pointer gridder = GridSourceType::New();
  gridder->SetSpacing( reader->GetOutput()->GetSpacing() );
  gridder->SetOrigin( reader->GetOutput()->GetOrigin() );
  gridder->SetSize( reader->GetOutput()->GetLargestPossibleRegion().GetSize() );

  typename GridSourceType::ArrayType gridSpacing;
  typename GridSourceType::ArrayType gridSigma;
  typename GridSourceType::BoolArrayType which;
  which.Fill( false );
  for( unsigned int i = 0; i < 2; i++ )
    {
    which[i] = true;
    }

  if( argc > 4 )
    {
    std::vector<unsigned int> directions
      = ConvertVector<unsigned int>( std::string( argv[4] ) );
    if( directions.size() != ImageDimension )
      {
      Rcpp::Rcout << "Incorrect direction size." << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        which[i] = static_cast<bool>( directions[i] );
        }
      }
    }

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    gridSpacing[i] = reader->GetOutput()->GetLargestPossibleRegion().GetSize()[i]
      *reader->GetOutput()->GetSpacing()[i]/25.0;
    gridSigma[i] = gridSpacing[i]/10.0;
    }
  if( argc > 5 )
    {
    std::vector<RealType> spacing
      = ConvertVector<RealType>( std::string( argv[5] ) );
    if( spacing.size() != ImageDimension )
      {
      Rcpp::Rcout << "Incorrect spacing size." << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        gridSpacing[i] = spacing[i];
        gridSigma[i] = gridSpacing[i]/10.0;
        }
      }
    }
  if( argc > 6 )
    {
    std::vector<RealType> sigma
      = ConvertVector<RealType>( std::string( argv[6] ) );
    if( sigma.size() != ImageDimension )
      {
      Rcpp::Rcout << "Incorrect sigma size." << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        gridSigma[i] = sigma[i]/10.0;
        }
      }
    }

  gridder->SetGridSpacing( gridSpacing );
  gridder->SetSigma( gridSigma );
  gridder->SetWhichDimensions( which );
  gridder->Update();
  typename RealImageType::Pointer grid=gridder->GetOutput();
  grid->SetDirection(reader->GetOutput()->GetDirection());
  grid->SetOrigin(reader->GetOutput()->GetOrigin());
  grid->SetSpacing(reader->GetOutput()->GetSpacing());

    typedef itk::MatrixOffsetTransformBase< double, ImageDimension, ImageDimension > TransformType;
    typedef itk::WarpImageMultiTransformFilter<RealImageType,RealImageType, VectorImageType, TransformType> WarperType;
    typename WarperType::Pointer  warper = WarperType::New();
    warper->SetInput(grid);
    warper->SetEdgePaddingValue( 0);
    warper->SetSmoothScale(1);
    warper->PushBackDisplacementFieldTransform(reader->GetOutput());
    warper->SetOutputOrigin(reader->GetOutput()->GetOrigin());
    warper->SetOutputSize(reader->GetOutput()->GetLargestPossibleRegion().GetSize());
    warper->SetOutputSpacing(reader->GetOutput()->GetSpacing());
    warper->SetOutputDirection(reader->GetOutput()->GetDirection());
    warper->Update();

  std::string file = std::string( argv[3] );
  typedef itk::ImageFileWriter<RealImageType> ImageWriterType;
  typename ImageWriterType::Pointer gridWriter = ImageWriterType::New();
  gridWriter->SetFileName( file.c_str() );
  gridWriter->SetInput( warper->GetOutput() );
  gridWriter->Update();

  return 0;
}

RcppExport SEXP CreateWarpedGridImage( SEXP r_args )
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


  if ( argc < 4 )
    {
    Rcpp::Rcout << "Usage: " << argv[0] << " ImageDimension deformationField "
      << "outputImage [directions,e.g. 1x0x0] [gridSpacing] [gridSigma]"
      << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     CreateWarpedGridImage<2>( argc, argv );
     break;
   case 3:
     CreateWarpedGridImage<3>( argc, argv );
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



