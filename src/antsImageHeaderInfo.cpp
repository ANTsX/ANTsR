#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>
#include "itkImage.h"
#include "itkImageFileReader.h"

RcppExport SEXP antsImageHeaderInfo( SEXP r_filename )
try
{
  std::string fname = Rcpp::as<std::string>(r_filename);

  typedef itk::ImageIOBase::IOComponentType ScalarPixelType;

  itk::ImageIOBase::Pointer imageIO =
      itk::ImageIOFactory::CreateImageIO(
          fname.c_str(), itk::ImageIOFactory::ReadMode);

  if ( !imageIO->CanReadFile(fname.c_str()) )
  {
    Rcpp::stop("Can't read this file format");
  }

  imageIO->SetFileName(fname);
  imageIO->ReadImageInformation();

  const size_t numDimensions =  imageIO->GetNumberOfDimensions();
  const size_t numComponents = imageIO->GetNumberOfComponents();
  const std::string pixelClass( imageIO->GetPixelTypeAsString(imageIO->GetPixelType()) );
  const unsigned int pixelCode = imageIO->GetComponentType();

  Rcpp::NumericVector dimensions( numDimensions );
  Rcpp::NumericVector spacing( numDimensions );
  Rcpp::NumericVector origin( numDimensions );
  Rcpp::NumericMatrix direction( numDimensions, numDimensions );

  for (unsigned int i=0; i<numDimensions; i++)
    {

    dimensions[i] = imageIO->GetDimensions(i);
    spacing[i] = imageIO->GetSpacing(i);
    origin[i] = imageIO->GetOrigin(i);
    for (unsigned int j=0; j<numDimensions; j++)
      {
      direction(i,j) = imageIO->GetDirection(i)[j];
      }
    }

  std::string pixeltype = "unknown";

  switch( pixelCode )
    {
    case 0: // UNKNOWNCOMPONENTTYPE - exception here?
      pixeltype = "unknown";
      break;
    case 1: // UCHAR
      pixeltype = "unsigned char";
      break;
    case 2: // CHAR
      pixeltype = "char";
      break;
    case 3: // USHORT
      pixeltype = "unsigned short";
      break;
    case 4: // SHORT
      pixeltype = "short";
      break;
    case 5: // UINT
      pixeltype = "unsigned int";
      break;
    case 6: // INT
      pixeltype = "int";
      break;
    case 7: // ULONG
      pixeltype = "unsigned long";
      break;
    case 8: // LONG
      pixeltype = "long";
      break;
    case 9: // FLOAT
      pixeltype = "float";
      break;
    case 10: // DOUBLE
      pixeltype = "double";
      break;
    default:
      Rcpp::stop("Invalid pixel type in image header");
    }

  return Rcpp::List::create( Rcpp::Named("pixelclass", Rcpp::wrap(pixelClass)),
                             Rcpp::Named("pixeltype", Rcpp::wrap(pixeltype)),
                             Rcpp::Named("nDimensions", Rcpp::wrap(numDimensions)),
                             Rcpp::Named("nComponents", Rcpp::wrap(numComponents)),
                             Rcpp::Named("dimensions", dimensions),
                             Rcpp::Named("spacing", spacing),
                             Rcpp::Named("origin", origin),
                             Rcpp::Named("direction", direction) );
}
catch( itk::ExceptionObject & err )
{
  Rcpp::Rcout << "ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;

  // Since the goal of the example is to catch the exception,
  // we declare this a success.
  Rcpp::stop("ITK exception caught");
}
