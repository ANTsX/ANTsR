#include <exception>
#include <vector>
#include <string>
#include <RcppANTsR.h>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkLabelContourImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkFastMarchingExtensionImageFilterBase.h"
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"

template< class ImageType >
SEXP fastMarchingExtension( SEXP r_speedImage, SEXP r_labelImage, SEXP r_valueImage )
{
  typedef typename ImageType::Pointer       ImagePointerType;
  typedef typename ImageType::PixelType     PixelType;

  ImagePointerType speedImage = Rcpp::as<ImagePointerType>( r_speedImage );
  ImagePointerType labelImage = Rcpp::as<ImagePointerType>( r_labelImage );
  ImagePointerType valueImage = Rcpp::as<ImagePointerType>( r_valueImage );

  typedef typename itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef itk::FastMarchingThresholdStoppingCriterion< ImageType, ImageType >
    CriterionType;
  typedef typename CriterionType::Pointer CriterionPointer;
  CriterionPointer criterion = CriterionType::New();
  criterion->SetThreshold( (PixelType) 1.e9 ); // something large

  typedef  itk::FastMarchingExtensionImageFilterBase<ImageType, ImageType,
    PixelType,1>  MarcherBaseType;

  typedef typename MarcherBaseType::LabelImageType LabelImageType;

  typename MarcherBaseType::Pointer                             fastMarching;
  typedef itk::BinaryThresholdImageFilter<ImageType, ImageType> ThresholderType;

  typename ThresholderType::Pointer thresholder = ThresholderType::New();
  thresholder->SetInput( labelImage );
  thresholder->SetLowerThreshold( (PixelType) 0.5 );
  thresholder->SetUpperThreshold( (PixelType) 1.001 );
  thresholder->SetInsideValue( (PixelType) 1 );
  thresholder->SetOutsideValue( (PixelType) 0 );

  typedef itk::LabelContourImageFilter<ImageType, ImageType>  ContourFilterType;
  typename ContourFilterType::Pointer contour = ContourFilterType::New();
  contour->SetInput( thresholder->GetOutput() );
  contour->FullyConnectedOff();
  contour->SetBackgroundValue( itk::NumericTraits<typename LabelImageType::PixelType>::ZeroValue() );
  contour->Update();
  typename ImageType::Pointer contourImage = contour->GetOutput();

  fastMarching = MarcherBaseType::New();
  fastMarching->SetInput( speedImage );
  typedef typename MarcherBaseType::NodePairType           NodePairType;
  typedef typename MarcherBaseType::NodePairContainerType  NodePairContainerType;
  typedef typename MarcherBaseType::AuxValueVectorType     AuxValueVectorType;
  typedef typename MarcherBaseType::AuxValueContainerType  AuxValueContainerType;
  typename AuxValueContainerType::Pointer auxAliveValues = AuxValueContainerType::New();
  typename AuxValueContainerType::Pointer auxTrialValues = AuxValueContainerType::New();
  typename NodePairContainerType::Pointer seeds = NodePairContainerType::New();
  seeds->Initialize();
  typename NodePairContainerType::Pointer alivePoints = NodePairContainerType::New();
  alivePoints->Initialize();
  unsigned int seedct = 0, alivect = 0;

  Iterator vfIter2( labelImage,  labelImage->GetLargestPossibleRegion() );
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
    typename ImageType::IndexType ind = vfIter2.GetIndex();
    double labval = labelImage->GetPixel( ind );
    double contourval = contourImage->GetPixel( ind );
    if ( ( (unsigned int) contourval == 1 )  )
      {
      seeds->push_back( NodePairType(  ind, 0. ) );
      AuxValueVectorType vector;
      vector[0] = valueImage->GetPixel( ind  );
      auxTrialValues->push_back( vector );
      seedct++;
      }
    if (  ( labval > 0  ) && ((unsigned int) contourval != 1 ) )
      {
      alivePoints->push_back( NodePairType(  ind, 0. ) );
      AuxValueVectorType vector;
      vector[0] = valueImage->GetPixel( ind  );
      auxAliveValues->push_back( vector );
      alivect++;
      }
  }
  fastMarching->SetTrialPoints(  seeds  );
  fastMarching->SetAuxiliaryTrialValues( auxTrialValues );
  fastMarching->SetAlivePoints( alivePoints );
  fastMarching->SetAuxiliaryAliveValues( auxAliveValues );
  fastMarching->SetStoppingCriterion( criterion );
  fastMarching->Update();

  //return Rcpp::wrap( fastMarching->GetAuxiliaryImage(0) );
  return Rcpp::wrap(0);
}


RcppExport SEXP fastMarchingExtension( SEXP r_speedImage, SEXP r_labelImage, SEXP r_valueImage )
{
try
{

  Rcpp::S4 speedImage( r_speedImage );
  Rcpp::S4 labelImage( r_labelImage );
  Rcpp::S4 valueImage( r_valueImage );

  //unsigned int components = Rcpp::as<unsigned int>(speedImage.slot( "components"));
  unsigned int dimension = Rcpp::as<unsigned int>(speedImage.slot( "dimension"));
  std::string pixeltype = Rcpp::as< std::string >(speedImage.slot( "pixeltype" ));

  if ( pixeltype == "double")
    {
    typedef double PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "float")
    {
    typedef float PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "unsigned int")
    {
    typedef unsigned int PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else if ( pixeltype == "unsigned char")
    {
    typedef unsigned char PixelType;
    if ( dimension == 2 )
      {
      const unsigned int dim = 2;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 3 )
      {
      const unsigned int dim = 3;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else if ( dimension == 4 )
      {
      const unsigned int dim = 4;
      typedef itk::Image<PixelType,dim>       ImageType;
      return fastMarchingExtension<ImageType>(r_speedImage, r_labelImage, r_valueImage);
      }
    else
      {
      Rcpp::stop( "Unsupported image dimension" );
      }
    }
  else
    {
    Rcpp::stop( "Unsupported pixeltype");
    }
}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  forward_exception_to_r( err );
  }
catch( const std::exception& exc )
  {
  Rcpp::Rcout << "STD ExceptionObject caught !" << std::endl;
  forward_exception_to_r( exc );
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
