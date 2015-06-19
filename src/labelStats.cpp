#include <Rcpp.h>
#include "itkImage.h"
#include "itkLabelStatisticsImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

template< unsigned int Dimension >
Rcpp::DataFrame labelStatsHelper(
  typename itk::Image< float, Dimension >::Pointer image,
  typename itk::Image< unsigned int, Dimension>::Pointer labelImage)
{
  typedef float PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType>                    Iterator;
  typedef itk::Image< unsigned int, Dimension > LabelImageType;
  typedef itk::LabelStatisticsImageFilter< ImageType, LabelImageType >
    LabelStatisticsImageFilterType;
  typename LabelStatisticsImageFilterType::Pointer labelStatisticsImageFilter =
    LabelStatisticsImageFilterType::New();
  labelStatisticsImageFilter->SetInput( image );
  labelStatisticsImageFilter->SetLabelInput( labelImage );
  labelStatisticsImageFilter->Update();

  typedef typename LabelStatisticsImageFilterType::ValidLabelValuesContainerType
    ValidLabelValuesType;

  long nlabs = labelStatisticsImageFilter->GetNumberOfLabels();

  Rcpp::NumericVector labelvals =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector means = Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector mins = Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector maxes = Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector variances =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector counts =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector volumes =
     Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector x =
     Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector y =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector z =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());
  Rcpp::NumericVector t =
    Rcpp::NumericVector(nlabs, Rcpp::NumericVector::get_na());

  typename ImageType::SpacingType spacing = image->GetSpacing();
  float voxelVolume = 1.0;
  for (int ii = 0; ii < spacing.Size(); ii++)
  {
    voxelVolume *= spacing[ii];
  }

  int ii = 0; // counter for label values
  for (typename ValidLabelValuesType::const_iterator
         labelIterator  = labelStatisticsImageFilter->GetValidLabelValues().begin();
         labelIterator != labelStatisticsImageFilter->GetValidLabelValues().end();
         ++labelIterator)
  {
    if ( labelStatisticsImageFilter->HasLabel(*labelIterator) )
    {
      int labelValue = *labelIterator;
      labelvals[ii] = labelValue;
      means[ii]     = labelStatisticsImageFilter->GetMean(labelValue);
      mins[ii]      = labelStatisticsImageFilter->GetMinimum(labelValue);
      maxes[ii]     = labelStatisticsImageFilter->GetMaximum(labelValue);
      variances[ii] = labelStatisticsImageFilter->GetVariance(labelValue);
      counts[ii]    = labelStatisticsImageFilter->GetCount(labelValue);
      volumes[ii]   = labelStatisticsImageFilter->GetCount(labelValue) * voxelVolume;
    }
    ++ii;
  }

  typedef std::vector<float> LabelSetType;
  LabelSetType myLabelSet;
  Iterator It( image, image->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PixelType label = It.Get();
    if( fabs(label) > 0 )
      {
      if( find( myLabelSet.begin(), myLabelSet.end(), label )
          == myLabelSet.end() )
        {
        myLabelSet.push_back( label );
        }
      }
    }
  std::sort(myLabelSet.begin(), myLabelSet.end() );
  typename LabelSetType::const_iterator it;
  unsigned long labelcount = 1;
  for( it = myLabelSet.begin(); it != myLabelSet.end(); ++it )
    {
    float currentlabel = *it;
    typename ImageType::PointType myCenterOfMass;
    myCenterOfMass.Fill(0);
    unsigned long totalct = 0;
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      PixelType label = It.Get();
      if(  label == currentlabel  )
        {
        // compute center of mass
        typename ImageType::PointType point;
        image->TransformIndexToPhysicalPoint( It.GetIndex(), point );
        for( unsigned int i = 0; i < spacing.Size(); i++ )
          {
          myCenterOfMass[i] += point[i];
          }
        totalct++;
        }
      }
    for( unsigned int i = 0; i < spacing.Size(); i++ )
      {
      myCenterOfMass[i] /= (float)totalct;
      }
      x[labelcount]=myCenterOfMass[0];
      y[labelcount]=myCenterOfMass[1];
      if ( Dimension > 2 ) z[labelcount]=myCenterOfMass[2];
      if ( Dimension > 3 ) t[labelcount]=myCenterOfMass[3];
    labelcount++;
    }


  Rcpp::DataFrame labelStats = Rcpp::DataFrame::create(
    Rcpp::Named("LabelValue") = labelvals,
    Rcpp::Named("Mean")       = means,
    Rcpp::Named("Min")         = mins,
    Rcpp::Named("Max")         = maxes,
    Rcpp::Named("Variance")    = variances,
    Rcpp::Named("Count")       = counts,
    Rcpp::Named("Volume")      = volumes,
    Rcpp::Named("x")      = x,
    Rcpp::Named("y")      = y,
    Rcpp::Named("z")      = z,
    Rcpp::Named("t")      = t);
  return (labelStats);
}

RcppExport SEXP labelStats(SEXP r_image, SEXP r_labelImage)
{
  if( r_image == NULL || r_labelImage == NULL  )
    {
    Rcpp::Rcout << "Invalid input arguments." << std::endl ;
    Rcpp::wrap( 1 ) ;
    }
  Rcpp::S4 image( r_image );
  Rcpp::S4 labelImage( r_labelImage );
  std::string pixeltype1 = Rcpp::as< std::string >(image.slot("pixeltype"));
  unsigned int dimension = Rcpp::as < unsigned int > (image.slot("dimension"));

  if ( (pixeltype1.compare("float") != 0))
  {
    Rcpp::Rcout << "Input image must be of float type." << std::endl;
    Rcpp::wrap(1);
  }
  if ( dimension == 2)
  {
    typedef itk::Image< float, 2 > ImageType;
    typedef itk::Image< unsigned int, 2 > LabelImageType;
    typedef ImageType::Pointer ImagePointerType;
    typedef LabelImageType::Pointer LabelImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >(image.slot("pointer")));
    Rcpp::XPtr< LabelImagePointerType > antsimage_xptr2(
      static_cast< SEXP >(labelImage.slot("pointer")));
    return Rcpp::wrap(labelStatsHelper< 2 >(*antsimage_xptr1, *antsimage_xptr2));
  }
  else if ( dimension == 3 )
  {
    typedef itk::Image< float, 3 > ImageType;
    typedef itk::Image< unsigned int, 3 > LabelImageType;
    typedef ImageType::Pointer ImagePointerType;
    typedef LabelImageType::Pointer LabelImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >(image.slot("pointer")));
    Rcpp::XPtr< LabelImagePointerType > antsimage_xptr2(
      static_cast< SEXP >(labelImage.slot("pointer")));
    return Rcpp::wrap(labelStatsHelper< 3 >(*antsimage_xptr1, *antsimage_xptr2));
  }
  else if ( dimension == 4 )
  {
    typedef itk::Image< float, 4 > ImageType;
    typedef itk::Image< unsigned int, 4 > LabelImageType;
     typedef ImageType::Pointer ImagePointerType;
    typedef LabelImageType::Pointer LabelImagePointerType;
    Rcpp::XPtr< ImagePointerType > antsimage_xptr1(
      static_cast< SEXP >(image.slot("pointer")));
    Rcpp::XPtr< LabelImagePointerType > antsimage_xptr2(
      static_cast< SEXP >(labelImage.slot("pointer")));
    return Rcpp::wrap(labelStatsHelper< 4 >(*antsimage_xptr1, *antsimage_xptr2));
  }
  else Rcpp::Rcout << "Dimension " << dimension << " is not supported." << std::endl;
  return Rcpp::wrap(0);
}
