#include <Rcpp.h> 
#include "itkImage.h" 
#include "itkLabelStatisticsImageFilter.h"

template< unsigned int Dimension > 
Rcpp::DataFrame labelStatsHelper( 
  typename itk::Image< float, Dimension >::Pointer image, 
  typename itk::Image< unsigned int, Dimension>::Pointer labelImage)
{
  typedef itk::Image< float, Dimension > ImageType; 
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
  Rcpp::DataFrame labelStats = Rcpp::DataFrame::create(
    Rcpp::Named("LabelValue") = labelvals, 
    Rcpp::Named("Mean")       = means, 
    Rcpp::Named("Min")         = mins, 
    Rcpp::Named("Max")         = maxes, 
    Rcpp::Named("Variance")    = variances, 
    Rcpp::Named("Count")       = counts, 
    Rcpp::Named("Volume")      = volumes); 
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
