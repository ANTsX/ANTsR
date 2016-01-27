#include<algorithm>
#include <exception>
#include <vector>
#include <string>
#include "RcppANTsR.h"
#include <ants.h>
#include "itkImage.h"
#include "itkAffineTransform.h"
#include "itkEuler3DTransform.h"

template< class TimeSeriesImageType >
SEXP antsMotionCorrStatsHelper(
    SEXP r_tsimg,
    SEXP r_maskimg,
    SEXP mocoparams,
    SEXP r_stupidoffset )
{
  typedef double RealType;
  const unsigned int dim = TimeSeriesImageType::ImageDimension;
  typedef typename TimeSeriesImageType::Pointer TimeSeriesImagePointerType;
  typedef typename itk::Image< typename TimeSeriesImageType::PixelType,
          dim - 1 > MaskImageType;
  typedef typename MaskImageType::Pointer MaskImagePointerType;
  typename MaskImageType::Pointer mask = Rcpp::as< MaskImagePointerType >( r_maskimg );
  typename TimeSeriesImageType::Pointer timeseriesImage =
    Rcpp::as< TimeSeriesImagePointerType >( r_tsimg );
  typedef typename TimeSeriesImageType::RegionType           TimeSeriesRegionType;
  typedef typename TimeSeriesImageType::IndexType            TimeSeriesIndexType;
  typedef typename TimeSeriesImageType::SizeType             TimeSeriesSizeType;
  typedef itk::ImageRegionIterator< TimeSeriesImageType > TimeSeriesIteratorType;

  typedef itk::ImageRegionIteratorWithIndex< MaskImageType > MaskIteratorType;
  typedef itk::AffineTransform< RealType, dim - 1 >          AffineTransformType;
  typedef itk::Euler3DTransform< RealType >                  RigidTransformType;

  typename MaskImageType::Pointer map = MaskImageType::New();
  map->SetRegions( mask->GetLargestPossibleRegion() );
  map->Allocate();
  map->FillBuffer( 0 );
  map->SetOrigin( mask->GetOrigin() );
  map->SetSpacing( mask->GetSpacing() );
  map->SetDirection( mask->GetDirection() );

  typename TimeSeriesImageType::Pointer timeseriesDisplacementImage =
    TimeSeriesImageType::New();
  timeseriesDisplacementImage->SetRegions(
      timeseriesImage->GetLargestPossibleRegion() );
  timeseriesDisplacementImage->Allocate();
  timeseriesDisplacementImage->FillBuffer( 0.0 );
  timeseriesDisplacementImage->SetOrigin( timeseriesImage->GetOrigin() );
  timeseriesDisplacementImage->SetSpacing( timeseriesImage->GetSpacing() );
  timeseriesDisplacementImage->SetDirection( timeseriesImage->GetDirection() );

  Rcpp::NumericMatrix moco( mocoparams );
  unsigned int stupidOffset = Rcpp::as< unsigned int >( r_stupidoffset );
  Rcpp::NumericMatrix displacements(moco.nrow(), 2);
  unsigned int nTransformParams = moco.ncol() - stupidOffset;
  for ( int ii = 0; ii < moco.nrow(); ii++ )
  {
    typename AffineTransformType::Pointer affineTransform1 = AffineTransformType::New();
    typename AffineTransformType::Pointer affineTransform2 = AffineTransformType::New();
    typename AffineTransformType::ParametersType params1;
    typename AffineTransformType::ParametersType params2;
    params1.SetSize( nTransformParams );
    params2.SetSize( nTransformParams );

    for (int jj = 0; jj < nTransformParams ; jj++ )
    {
      params1[jj] = moco(ii, jj + stupidOffset );
      if (ii < (moco.nrow() - 1) )
      {
        params2[jj] = moco(ii + 1, jj + stupidOffset );
      }
    }

    if ( nTransformParams == 6 )
    {
      RigidTransformType::Pointer rigid1 = RigidTransformType::New();
      RigidTransformType::Pointer rigid2 = RigidTransformType::New();
      rigid1->SetParameters( params1 );
      rigid2->SetParameters( params2 );

      affineTransform1->SetMatrix( rigid1->GetMatrix() );
      affineTransform1->SetTranslation( rigid1->GetTranslation() );
      affineTransform2->SetMatrix( rigid2->GetMatrix() );
      affineTransform2->SetTranslation( rigid2->GetTranslation() );
    }
    else if ( nTransformParams == 12 )
    {
      affineTransform1->SetParameters( params1 );
      affineTransform2->SetParameters( params2 );
    }
    else
    {
      Rcpp::stop("Unknown transform type.");
    }

    double meanDisplacement = 0.0;
    double maxDisplacement = 0.0;
    double count = 0;
    TimeSeriesRegionType timeSeriesRegion;
    TimeSeriesIndexType timeSeriesIndex;
    TimeSeriesSizeType timeSeriesSize;

    for (int jj = 0; jj < 3; jj++) {
      timeSeriesIndex[jj] = mask->GetLargestPossibleRegion().GetIndex()[jj];
      timeSeriesSize[jj] = mask->GetLargestPossibleRegion().GetSize()[jj];
    }
    timeSeriesIndex[3] = ii;
    timeSeriesSize[3] = 1;
    timeSeriesRegion.SetIndex( timeSeriesIndex );
    timeSeriesRegion.SetSize( timeSeriesSize );

    TimeSeriesIteratorType timeSeriesIterator( timeseriesDisplacementImage,
        timeSeriesRegion );
    MaskIteratorType it( mask, mask->GetLargestPossibleRegion() );
    while ( !it.IsAtEnd() )
    {
      if ( it.Value() > 0 )
      {
        typename MaskImageType::IndexType idx = it.GetIndex();
        typename MaskImageType::PointType pt;
        mask->TransformIndexToPhysicalPoint( idx, pt );
        typename MaskImageType::PointType pt1 =
          affineTransform1->TransformPoint( pt );

        double dist = 0;
        if (ii < moco.nrow() - 1 )
        {
          typename MaskImageType::PointType pt2 =
            affineTransform2->TransformPoint( pt );
          dist = pt1.EuclideanDistanceTo( pt2 );
          map->SetPixel( idx, map->GetPixel(idx) + dist );
        }
        else
        {
          dist = pt.EuclideanDistanceTo( pt1 );
          map->SetPixel( idx, map->GetPixel(idx) + dist );
        }
        if (ii == moco.nrow() - 1)
        {
          dist = 0.0;
        }
        if ( dist > maxDisplacement )
        {
          maxDisplacement = dist;
        }
        meanDisplacement += dist;
        ++count;
        timeSeriesIterator.Set(dist);
      }
      ++it;
      ++timeSeriesIterator;
    }
  meanDisplacement /= count;
  displacements(ii, 0) = meanDisplacement;
  displacements(ii, 1) = maxDisplacement;
  }

  SEXP r_outimg = Rcpp::wrap( timeseriesDisplacementImage );


  return Rcpp::List::create(Rcpp::Named("Displacements") = displacements,
        Rcpp::Named("TimeSeriesDisplacements") = r_outimg);
}

RcppExport SEXP antsMotionCorrStats(
    SEXP r_tsimg,
    SEXP r_mask,
    SEXP r_moco,
    SEXP r_stupidoffset )
{
try
{
  Rcpp::S4 antsImage( r_tsimg );
  std::string pixeltype = Rcpp::as< std::string >( antsImage.slot( "pixeltype" ));
  unsigned int dimension = Rcpp::as< int >( antsImage.slot( "dimension" ));
  if ( (pixeltype != "float") || (dimension != 4) )
  {
    Rcpp::stop("Incorrect dimensions or pixeltype.");
  }
  typedef float PixelType;
  const unsigned int dim = 4;
  typedef itk::Image< PixelType, dim > TimeSeriesImageType;
  return( antsMotionCorrStatsHelper< TimeSeriesImageType >(
        r_tsimg, r_mask, r_moco, r_stupidoffset ) );
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
