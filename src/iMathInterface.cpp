#include <RcppANTsR.h>
#include "iMathFunctions.h"

template <class ImageType>
SEXP iMathCanny( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double sigma = Rcpp::as<double>( args[2] );
  double lower = Rcpp::as<double>( args[3] );
  double upper = Rcpp::as<double>( args[4] );

  ImagePointerType output = ants::iMathCanny<ImageType>( input, sigma, lower, upper );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathDistanceMap( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  bool useSpacing = Rcpp::as<bool>( args[2] );

  ImagePointerType output = ants::iMathDistanceMap<ImageType>( input, useSpacing );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathFillHoles( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double holeParam = Rcpp::as<double>( args[2] );

  ImagePointerType output = ants::iMathFillHoles<ImageType>( input, holeParam );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathGC( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );

  ImagePointerType output = ants::iMathGC<ImageType>( input, radius );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathGD( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );

  ImagePointerType output = ants::iMathGD<ImageType>( input, radius );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathGE( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );

  ImagePointerType output = ants::iMathGE<ImageType>( input, radius );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathGO( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );

  ImagePointerType output = ants::iMathGO<ImageType>( input, radius );

  return Rcpp::wrap(output);
}


template <class ImageType>
SEXP iMathGetLargestComponent( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned int minSize = Rcpp::as<unsigned int>( args[2] );

  ImagePointerType output = ants::iMathGetLargestComponent<ImageType>( input, minSize );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathGrad( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double sigma = Rcpp::as<double>( args[2] );
  bool normalize = Rcpp::as<bool>( args[3] );

  ImagePointerType output = ants::iMathGrad<ImageType>( input, sigma, normalize );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathLaplacian( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double sigma = Rcpp::as<double>( args[2] );
  bool normalize = Rcpp::as<bool>( args[3] );

  ImagePointerType output = ants::iMathLaplacian<ImageType>( input, sigma, normalize );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathMC( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );
  PixelType value = (PixelType) Rcpp::as<double>( args[3] );

  ImagePointerType output = ants::iMathMC<ImageType>( input, radius, value );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathMD( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );
  PixelType value = (PixelType) Rcpp::as<double>( args[3] );

  ImagePointerType output = ants::iMathMD<ImageType>( input, radius, value );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathME( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );
  PixelType value = (PixelType) Rcpp::as<double>( args[3] );

  ImagePointerType output = ants::iMathME<ImageType>( input, radius, value );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathMO( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long radius = Rcpp::as<unsigned long>( args[2] );
  PixelType value = (PixelType) Rcpp::as<double>( args[3] );

  ImagePointerType output = ants::iMathMO<ImageType>( input, radius, value );

  return Rcpp::wrap(output);
 }

template <class ImageType>
SEXP iMathMaurerDistance( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  PixelType foreground = (PixelType) Rcpp::as<double>( args[2] );

  ImagePointerType output = ants::iMathMaurerDistance<ImageType>( input, foreground );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathNormalize( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );

  ImagePointerType output = ants::iMathNormalize<ImageType>( input );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathPad( Rcpp::List args )
{
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  int padding = Rcpp::as<int>( args[2] );

  ImagePointerType output = ants::iMathMaurerDistance<ImageType>( input, padding );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathPeronaMalik( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  unsigned long nIterations = Rcpp::as<unsigned long>( args[2] );
  double conductance = Rcpp::as<double>( args[3] );


  ImagePointerType output = ants::iMathPeronaMalik<ImageType>( input, nIterations, conductance );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathSharpen( Rcpp::List args )
{
  typedef typename ImageType::Pointer ImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );

  ImagePointerType output = ants::iMathSharpen<ImageType>( input );

  return Rcpp::wrap(output);
}

template <class ImageType>
SEXP iMathTruncateIntensity( Rcpp::List args )
{
  const unsigned int ImageDimension = ImageType::ImageDimension;
  typedef typename ImageType::Pointer   ImagePointerType;
  typedef typename ImageType::PixelType PixelType;

  typedef itk::Image<unsigned int, ImageDimension>   MaskImageType;
  typedef typename MaskImageType::Pointer   MaskImagePointerType;

  ImagePointerType input = Rcpp::as<ImagePointerType>( args[0] );
  double lowerQ = (PixelType) Rcpp::as<double>( args[2] );
  double upperQ = (PixelType) Rcpp::as<double>( args[3] );
  int nBins = (PixelType) Rcpp::as<int>( args[4] );

  MaskImagePointerType mask=NULL;

  if ( args.size() > 5 )
    {
    mask = Rcpp::as<MaskImagePointerType>( args[5] );
    }
  else
    {
    mask = MaskImageType::New();
    mask->SetSpacing( input->GetSpacing() );
    mask->SetOrigin( input->GetOrigin() );
    mask->SetDirection( input->GetDirection() );
    mask->SetRegions( input->GetLargestPossibleRegion() );
    mask->Allocate();
    mask->FillBuffer(1);
    }

  ImagePointerType output = ants::iMathTruncateIntensity<ImageType>( input, lowerQ, upperQ, nBins, mask );

  return Rcpp::wrap(output);
}

RcppExport SEXP iMathInterface( SEXP r_args )
{
try
{
  Rcpp::List args(r_args);
  std::string operation = Rcpp::as< std::string >( args[1] );

  unsigned int dim = 0;
  unsigned int components = 0;
  std::string pixeltype = "";

  if ( operation == "Canny" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( args.size() < 5 )
      {
      Rcpp::stop("To few input parameters");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathCanny<ImageType>( args );
        }
      }

    }
  //else if ( operation == "DistanceMap" )
  else if ( operation == "D" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      bool useSpacing = iMathDistanceMapUseSpacing;
      args.push_back( Rcpp::wrap(useSpacing) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathDistanceMap<ImageType>( args );
        }
      }
    }
  else if ( operation == "FillHoles" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      double holeParam = iMathFillHolesHoleParam;
      args.push_back( Rcpp::wrap(holeParam) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathFillHoles<ImageType>( args );
        }
      }
    }
  else if ( operation == "GC" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GC only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathGCRadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGC<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGC<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGC<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGC<ImageType>( args );
        }
      }
    }
  else if ( operation == "GD" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GD only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathGDRadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGD<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGD<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGD<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGD<ImageType>( args );
        }
      }
    }
  else if ( operation == "GE" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GE only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathMERadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGE<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGE<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGE<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGE<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGE<ImageType>( args );
        }
      }
    }
  else if ( operation == "GO" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GO only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathGORadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGO<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGO<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGO<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGO<ImageType>( args );
        }
      }
    }
  else if ( operation == "GetLargestComponent" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("GetLargestComponent only supports scalar images");
      }

    // Optional parameters with defaul values
    if ( args.size() < 3 )
      {
      unsigned long minSize = iMathGetLargestComponentMinSize;
      args.push_back( Rcpp::wrap(minSize) );  // minSize
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGetLargestComponent<ImageType>( args );
        }
      }
    }
  else if ( operation == "Grad" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      double sigma = iMathGradSigma;
      args.push_back( Rcpp::wrap(sigma) );  // radius
      }
    if ( args.size() < 4 )
      {
      bool normalize = iMathGradNormalize;
      args.push_back( Rcpp::wrap(normalize) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathGrad<ImageType>( args );
        }
      }
    }
  else if ( operation == "Laplacian" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      double sigma = iMathLaplacianSigma;
      args.push_back( Rcpp::wrap(sigma) );  // radius
      }
    if ( args.size() < 4 )
      {
      bool normalize = iMathLaplacianNormalize;
      args.push_back( Rcpp::wrap(normalize) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathLaplacian<ImageType>( args );
        }
      }
    }
  else if ( operation == "MC" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("MC only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathMCRadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }
    if ( args.size() < 4)
      {
      double value = iMathMCValue;
      args.push_back( Rcpp::wrap(value));
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMC<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMC<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMC<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMC<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMC<ImageType>( args );
        }
      }
    }
  else if ( operation == "MD" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("MD only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathMDRadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }
    if ( args.size() < 4)
      {
      double value = iMathMDValue;
      args.push_back( Rcpp::wrap(value));
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMD<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMD<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMD<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMD<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMD<ImageType>( args );
        }
      }
    }
  else if ( operation == "ME" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("ME only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathMERadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }
    if ( args.size() < 4)
      {
      double value = iMathMDValue;
      args.push_back( Rcpp::wrap(value));
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathME<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathME<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathME<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathME<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathME<ImageType>( args );
        }
      }
    }
  else if ( operation == "MO" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("MO only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      unsigned long radius = iMathMORadius;
      args.push_back( Rcpp::wrap(radius) );  // radius
      }
    if ( args.size() < 4)
      {
      double value = iMathMDValue;
      args.push_back( Rcpp::wrap(value));
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMO<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMO<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMO<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMO<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMO<ImageType>( args );
        }
      }
    }
  else if ( operation == "MaurerDistance" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("MaurerDistance only supports scalar images");
      }

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      double foreground = iMathMaurerDistanceForeground;
      args.push_back( Rcpp::wrap(foreground) );  // radius
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathMaurerDistance<ImageType>( args );
        }
      }
    }
  else if ( operation == "Normalize" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("Normalize only supports scalar images");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathNormalize<ImageType>( args );
        }
      }
    }
  else if ( operation == "PadImage" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 3 )
      {
      Rcpp::stop( "Pad: requires a padding value");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPad<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPad<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPad<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPad<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPad<ImageType>( args );
        }
      }
    }
  else if ( operation == "PeronaMalik" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("PeronaMalik only supports scalar images");
      }

    // Optional parameters with default values
     if ( args.size() < 3 )
       {
       unsigned long nIterations = iMathPeronaMalikNIterations;
       args.push_back( Rcpp::wrap(nIterations) );
       }
    if ( args.size() < 4 )
      {
      double conductance = iMathPeronaMalikConductance;
      args.push_back( Rcpp::wrap(conductance) );
     }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathPeronaMalik<ImageType>( args );
        }
      }
    }
  else if ( operation == "Sharpen" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    if ( components > 1 )
      {
      Rcpp::stop("Sharpen only supports scalar images");
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 3)
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathSharpen<ImageType>( args );
        }
      }
    }
  else if ( operation == "TruncateIntensity" )
    {
    Rcpp::S4 image( args[0] );
    dim = Rcpp::as< int >( image.slot( "dimension" ) );
    components = Rcpp::as< int >( image.slot( "components" ) );
    pixeltype = Rcpp::as<std::string>( image.slot( "pixeltype") );

    // Optional parameters with default values
    if ( args.size() < 4 )
      {
      Rcpp::stop("TruncateIntensity: requires lowerQ and upperQ values");
      }
    if ( args.size() < 5 )
      {
      int nBins = iMathTruncateIntensityNBins;
      args.push_back( Rcpp::wrap(nBins) );
      }

    if ( pixeltype == "double" )
      {
      typedef double ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      }
    else if ( pixeltype == "float" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned int" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      }
    else if ( pixeltype == "unsigned char" )
      {
      typedef float ValueType;

      if ( dim == 2 )
        {
        typedef itk::Image<ValueType,2>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 3 )
        {
        typedef itk::Image<ValueType,3>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      else if ( dim == 4 )
        {
        typedef itk::Image<ValueType,4>       ImageType;
        return iMathTruncateIntensity<ImageType>( args );
        }
      }
    }

  else
    {
    Rcpp::Rcout << "Operation = " << operation << std::endl;
    Rcpp::stop( "Unknown iMath operation");
    }

  return Rcpp::wrap(NA_REAL); // FIXME - for debugging only
}
catch( const itk::ExceptionObject& err )
  {
  Rcpp::Rcout << "ITK Exception" << std::endl;
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  Rcpp::Rcout << "STD Exception" << std::endl;
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }

return Rcpp::wrap(NA_REAL); //not reached
}
