#include <exception>
#include <algorithm>
#include <vector>
#include "RcppANTsR.h"
#include <ants.h>
#include <iostream>
#include <fstream>
#include <iterator>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sstream>
#include <unistd.h>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "vnl/vnl_matrix.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCSVNumericObjectFileWriter.h"
#include <vnl/algo/vnl_svd.h>
#include <itkStatisticsImageFilter.h>
#include "itkImageRegionIterator.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientImageFilter.h"
#include "itkCovariantVector.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "PatchAnalysis.h"
#include "PatchAnalysis.hxx"

template< class ImageType >
SEXP patchAnalysisHelper(
    SEXP r_inimg,
    SEXP r_outimg,
    Rcpp::NumericVector sigma,
    bool sigmaInPhysicalCoordinates,
    unsigned int kernelwidth)
{
  typedef float InputPixelType;
  typedef typename ImageType::Pointer ImagePointerType;
  typename ImageType::Pointer inimg =
    Rcpp::as< ImagePointerType >( r_inimg );
  typename ImageType::Pointer outimg =
    Rcpp::as< ImagePointerType >( r_outimg );

    patchAnalysisArgumentType args;
    args.inputName               = "";
    args.maskName                = "";
    args.outProjectionName       = "projectedPatches";
    args.eigvecName              = "";
    args.patchSize               = 3;
    args.targetVarianceExplained = 0.95;
    args.outEigvecMatrixName     = "";
    args.inEigvecMatrixName      = "";
    args.numberOfSamplePatches   = 1000;
    args.verbose                 = 0;
    args.orientationInvariant    = false;
    args.outPatchName            = "";
    args.meanCenter              = true;
    args.help                    = 0;

    PatchAnalysis< InputPixelType, 2 >( args );

//  outimg = filter->GetOutput();
//  r_outimg = Rcpp::wrap( outimg );
  return( r_outimg );
}

RcppExport SEXP patchAnalysis( SEXP r_inimg,
    SEXP r_outimg,
    SEXP r_sigma,
    SEXP sigmaInPhysicalCoordinates,
    SEXP r_kernelwidth )
{
try
{

  Rcpp::S4 antsImage( r_inimg );
  std::string pixeltype = Rcpp::as< std::string >( antsImage.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< int >( antsImage.slot( "dimension" ) );
  bool physicalSpacing = Rcpp::as< bool >( sigmaInPhysicalCoordinates );
  Rcpp::NumericVector sigma( r_sigma );
  unsigned int kernelwidth = Rcpp::as< unsigned int >( r_kernelwidth );

  if ( (pixeltype == "float") & ( dimension == 2 ) )
  {
    typedef float PixelType;
    const unsigned int dim = 2;
    typedef itk::Image< PixelType, dim > ImageType;
    SEXP outimg = patchAnalysisHelper< ImageType >(
        r_inimg, r_outimg, sigma, physicalSpacing, kernelwidth);
    return( outimg );
  }
  else
  {
    Rcpp::stop("Unsupported image dimension or pixel type.");
  }
}

catch( itk::ExceptionObject & err )
{
  Rcpp::Rcout << "ITK ExceptionObject caught!" << std::endl;
  forward_exception_to_r( err );
}
catch( const std::exception& exc )
{
  Rcpp::Rcout << "STD ExceptionObject caught!" << std::endl;
  forward_exception_to_r( exc );
}
catch( ... )
{
  Rcpp::stop( "C++ exception (unknown reason)");
}
 return Rcpp::wrap(NA_REAL); // should not be reached
}
