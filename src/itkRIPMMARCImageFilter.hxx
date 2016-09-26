/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRIPMMARCImageFilter_hxx
#define itkRIPMMARCImageFilter_hxx

#include "itkRIPMMARCImageFilter.h"

#include "itkArray.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkMeanImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"
#include "itkVarianceImageFilter.h"

#include <numeric>

namespace itk {

template <class TImage>
bool IsInside( typename TImage::Pointer input, typename TImage::IndexType index )
{
  /** FIXME - should use StartIndex - */
  typedef TImage ImageType;
  enum { ImageDimension = ImageType::ImageDimension };
  bool isinside = true;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    float shifted = index[i];
    if( shifted < 0 || shifted >  input->GetLargestPossibleRegion().GetSize()[i] - 1  )
      {
      isinside = false;
      }
    }
  return isinside;
}

template <typename TInputImage, typename TOutputImage>
RIPMMARCImageFilter<TInputImage, TOutputImage>
::RIPMMARCImageFilter() :
  m_RotationInvariant( true ),
  m_MeanCenterPatches( true ),
  m_LearnPatchBasis( false ),
  m_PatchRadius( 3 ),
  m_numberOfVoxelsWithinMask( 0 ),
  m_paddingVoxels( 2 ),
  m_numberOfSamplePatches( 0 )
{
  this->SetNumberOfRequiredInputs( 2 ); // image of interest and mask
  this->m_canonicalFrame = ITK_NULLPTR;
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::GetSamplePatchLocations()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::ExtractSamplePatches()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::ExtractAllPatches()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::LearnEigenPatches()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::ReorientSamplePatches()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::ReorientAllPatches()
{
}

template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::ProjectOnEigenPatches()
{
}


template<typename TInputImage, typename TOutputImage>
void RIPMMARCImageFilter<TInputImage, TOutputImage>
::GenerateData(  )
{
// FIXME - the logic below could be cleaned up
	this->GetSamplePatchLocations( ); // identify points from random mask
	this->ExtractSamplePatches( );     // convert sample points to the matrix
	if ( this->m_LearnPatchBasis )  // determines if we are learning or not
	{
		this->LearnEigenPatches( );  // learn the patches
	} else {
    // use significantPatchEigenvectors as reference
//		this->ReadEigenPatchMatrix(); // just apply the learning, given the eigenpatch
	}
	this->ExtractAllPatches( );
	// because all patches are reoriented to the first (non-rotationally invariant)
	// eigenpatch, we must learn the eigenpatches even if we will in the end use
	// rotationally invariant features.
	if ( this->m_RotationInvariant )
	{
		this->ReorientSamplePatches();
		this->ReorientAllPatches();
		if ( this->m_LearnPatchBasis )
			this->LearnEigenPatches(); // learn the patches
	}
	this->ProjectOnEigenPatches( );

}

template<typename TInputImage, typename TOutputImage>
void
RIPMMARCImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream &os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  if( this->m_RotationInvariant )
    {
    os << indent << "Using RotationInvariant model." << std::endl;
    }
  else
    {
    os << indent << "Using non-RotationInvariant model." << std::endl;
    }

  if( this->m_MeanCenterPatches )
    {
    os << indent << "We will MeanCenterPatches." << std::endl;
    }
  else
    {
    os << indent << "Do not MeanCenterPatches." << std::endl;
    }

  os << indent << "PatchRadius = " << this->m_PatchRadius << std::endl;
}




template< unsigned int ImageDimension, class TRealType, class TImageType,
  class TGradientImageType, class TInterpolator >
vnl_vector< TRealType > ReorientPatchToReferenceFrame(
    itk::NeighborhoodIterator< TImageType > GradientImageNeighborhood1,
    itk::NeighborhoodIterator< TImageType > GradientImageNeighborhood2,
    const typename TImageType::Pointer MaskImage,
    std::vector< unsigned int > IndicesWithinSphere,
    std::vector< double > IndexWeights,
    const typename TGradientImageType::Pointer GradientImage1,
    const typename TGradientImageType::Pointer GradientImage2,
    unsigned int NumberOfValuesPerVoxel,
    TInterpolator Interpolator
    )
{
  /* This function takes a reference patch and a moving patch and rotates
   * the moving patch to match the reference patch.
   * It returns an image equal to Image1, but with the reoriented entries from
   * the moving patch inserted in place of the reference patch.
   * Intended usage is to feed in an eigenvector in a canonical coordinate
   * frame, generated by GenerateMaskImageFromPatch, that consists of only the
   * entries in the eigenvector on a blank background.  The output of this function
   * then is the moving neighborhood reoriented to match the input eigenvector. */

  typedef TRealType RealType;
  typedef typename TImageType::PointType PointType;
  typedef itk::CovariantVector< RealType, ImageDimension > GradientPixelType;
  typedef vnl_vector< RealType > VectorType;
  typedef typename TImageType::IndexType IndexType;
  unsigned int NumberOfIndicesWithinSphere = IndicesWithinSphere.size();
  std::vector< PointType > ImagePatch1;
  std::vector< PointType > ImagePatch2;
  VectorType VectorizedImagePatch1( NumberOfIndicesWithinSphere, 0 );
  VectorType VectorizedImagePatch2( NumberOfIndicesWithinSphere, 0 );
  vnl_matrix< RealType > GradientMatrix1( NumberOfIndicesWithinSphere, NumberOfValuesPerVoxel );
  vnl_matrix< RealType > GradientMatrix2( NumberOfIndicesWithinSphere, NumberOfValuesPerVoxel );
  GradientMatrix1.fill( 0 );
  GradientMatrix2.fill( 0 );

  /*  Calculate center of each image patch so that rotations are about the origin. */
  PointType CenterPointOfImage1;
  PointType CenterPointOfImage2;
  CenterPointOfImage1.Fill( 0 );
  CenterPointOfImage2.Fill( 0 );
  RealType MeanNormalizingConstant = 1.0 / ( RealType ) NumberOfIndicesWithinSphere;
  for( unsigned int ii = 0; ii < NumberOfIndicesWithinSphere; ii++ )
  {
    VectorizedImagePatch1[ ii ] = GradientImageNeighborhood1.GetPixel( IndicesWithinSphere[ ii ] );
    VectorizedImagePatch2[ ii ] = GradientImageNeighborhood2.GetPixel( IndicesWithinSphere[ ii ] );
    IndexType GradientImageIndex1 = GradientImageNeighborhood1.GetIndex( IndicesWithinSphere[ ii ] );
    IndexType GradientImageIndex2 = GradientImageNeighborhood2.GetIndex( IndicesWithinSphere[ ii ] );
    if( ( IsInside< TGradientImageType >( GradientImage1, GradientImageIndex1) ) &&
	( IsInside< TGradientImageType >( GradientImage2, GradientImageIndex2 ) ) )
    {
      GradientPixelType GradientPixel1 = GradientImage1->GetPixel( GradientImageIndex1 ) * IndexWeights[ ii ];
      GradientPixelType GradientPixel2 = GradientImage2->GetPixel( GradientImageIndex2 ) * IndexWeights[ ii ];
      for( unsigned int jj = 0; jj < NumberOfValuesPerVoxel; jj++)
      {
	GradientMatrix1( ii, jj ) = GradientPixel1[ jj ];
	GradientMatrix2( ii, jj ) = GradientPixel2[ jj ];
      }
      PointType Point1;
      PointType Point2;
      GradientImage1->TransformIndexToPhysicalPoint( GradientImageIndex1, Point1 );
      GradientImage2->TransformIndexToPhysicalPoint( GradientImageIndex2, Point2 );
      for( unsigned int dd = 0; dd < ImageDimension; dd++ )
      {
	CenterPointOfImage1[ dd ] = CenterPointOfImage1[ dd ] + Point1[ dd ] * MeanNormalizingConstant;
	CenterPointOfImage2[ dd ] = CenterPointOfImage2[ dd ] + Point2[ dd ] * MeanNormalizingConstant;
      }
      ImagePatch1.push_back( Point1 );
      ImagePatch2.push_back( Point2 );
    }
    else return vnl_vector< TRealType > (1, 0.0 );
  }
  RealType MeanOfImagePatch1 = VectorizedImagePatch1.mean();
  RealType MeanOfImagePatch2 = VectorizedImagePatch2.mean();
  VectorType CenteredVectorizedImagePatch1 = ( VectorizedImagePatch1 - MeanOfImagePatch1 );
  VectorType CenteredVectorizedImagePatch2 = ( VectorizedImagePatch2 - MeanOfImagePatch2 );
  RealType StDevOfImage1 = sqrt( CenteredVectorizedImagePatch1.squared_magnitude()  );
  RealType StDevOfImage2 = sqrt( CenteredVectorizedImagePatch2.squared_magnitude() );
  RealType correlation = inner_product( CenteredVectorizedImagePatch1,
      CenteredVectorizedImagePatch2 ) / ( StDevOfImage1 * StDevOfImage2 );

  bool OK = true;
/*  std::cout << "VectorizedImagePatch1 is (before rotation) " << VectorizedImagePatch1 << std::endl;
  std::cout << "VectorizedImagePatch2 is (before rotation) " << VectorizedImagePatch2 << std::endl;*/
/*  std::cout << "GradientMatrix1 is " << GradientMatrix1 << std::endl;
  std::cout << "GradientMatrix2 is " << GradientMatrix2 << std::endl; */
  vnl_matrix< RealType > CovarianceMatrixOfImage1 = GradientMatrix1.transpose() * GradientMatrix1;
  vnl_matrix< RealType > CovarianceMatrixOfImage2 = GradientMatrix2.transpose() * GradientMatrix2;
  vnl_symmetric_eigensystem< RealType > EigOfImage1( CovarianceMatrixOfImage1 );
  vnl_symmetric_eigensystem< RealType > EigOfImage2( CovarianceMatrixOfImage2 );
/*  std::cout << "CovarianceMatrixOfImage1 is " << CovarianceMatrixOfImage1 << std::endl;
  std::cout << "CovarianceMatrixOfImage2 is " << CovarianceMatrixOfImage2 << std::endl;*/
  int NumberOfEigenvectors = EigOfImage1.D.cols();
  // FIXME: needs bug checking to make sure this is right
  // not sure how many eigenvectors there are or how they're indexed
  vnl_vector< RealType > Image1Eigvec1 = EigOfImage1.get_eigenvector( NumberOfEigenvectors - 1 ); // 0-indexed
  vnl_vector< RealType > Image1Eigvec2 = EigOfImage1.get_eigenvector( NumberOfEigenvectors - 2 );
  vnl_vector< RealType > Image2Eigvec1 = EigOfImage2.get_eigenvector( NumberOfEigenvectors - 1 );
  vnl_vector< RealType > Image2Eigvec2 = EigOfImage2.get_eigenvector( NumberOfEigenvectors - 2 );

  /* Solve Wahba's problem using Kabsch algorithm:
   * arg_min(Q) \sum_k || w_k - Q v_k ||^2
   * Q is rotation matrix, w_k and v_k are vectors to be aligned.
   * Solution:  Denote B = \sum_k w_k v_k^T
   * Decompose B = U * S * V^T
   * Then Q = U * M * V^T, where M = diag[ 1 1 det(U) det(V) ]
   * Refs: http://journals.iucr.org/a/issues/1976/05/00/a12999/a12999.pdf
   *       http://www.control.auc.dk/~tb/best/aug23-Bak-svdalg.pdf */
  vnl_matrix< RealType > B = outer_product( Image1Eigvec1, Image2Eigvec1 );
  if( ImageDimension == 3)
  {
    B = outer_product( Image1Eigvec1, Image2Eigvec1 ) +
        outer_product( Image1Eigvec2, Image2Eigvec2 );
  }
  vnl_svd< RealType > WahbaSVD( B );
  vnl_matrix< RealType > Q_solution = WahbaSVD.V() * WahbaSVD.U().transpose();
  // Now rotate the points to the same frame and sample neighborhoods again.
  for( unsigned int ii = 0; ii < NumberOfIndicesWithinSphere; ii++ )
  {
    PointType RotatedPoint = ImagePatch2[ ii ];
    // We also need vector representation of the point values
    vnl_vector< RealType > RotatedPointVector( RotatedPoint.Size(), 0 );
    // First move center of Patch 1 to center of Patch 2
    for( unsigned int dd = 0; dd < ImageDimension; dd++ )
    {
      RotatedPoint[ dd ] -= CenterPointOfImage2[ dd ];
      RotatedPointVector[ dd ] = RotatedPoint[ dd ];
    }

    // Now rotate RotatedPoint
    RotatedPointVector = ( Q_solution ) * RotatedPointVector;
    for( unsigned int dd = 0; dd < ImageDimension; dd++ )
    {
      RotatedPoint[ dd ] = RotatedPointVector[ dd ] + CenterPointOfImage2[ dd ];
    }
    if( Interpolator->IsInsideBuffer( RotatedPoint) )
    {
      VectorizedImagePatch2[ ii ] = Interpolator->Evaluate( RotatedPoint );
    }
    else OK = false;
  }

  /* This is a nasty little detail:  Because the eigenvector is in the positive quadrant,
   * you can end up with flipped images that are negatively correlated with each other.
   * Here we check for that and correct if necessary.
   */
  MeanOfImagePatch2 = VectorizedImagePatch2.mean();
  CenteredVectorizedImagePatch2 = ( VectorizedImagePatch2 - MeanOfImagePatch2 );

  if(inner_product(CenteredVectorizedImagePatch1, CenteredVectorizedImagePatch2) < 0)
  {

	  vnl_matrix< RealType > B = outer_product( Image1Eigvec1, Image2Eigvec1 );
	  if( ImageDimension == 3)
	  {
		  B = outer_product( Image1Eigvec1, Image2Eigvec1 ) +
				  outer_product( Image1Eigvec2, Image2Eigvec2 );
	  }
	  vnl_svd< RealType > WahbaSVD( B );
	  vnl_matrix< RealType > Q_solution = WahbaSVD.V() * WahbaSVD.U().transpose();
          vnl_matrix< RealType > rotationMat;
          if(ImageDimension == 2)
          {
            const RealType values[4] = {-1.0,0.0,0.0,-1.0};
            rotationMat.set_size(2, 2);
            rotationMat.set(values);
          } else if( ImageDimension == 3)
          {
            const RealType values[9] = {1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, -1.0};
            rotationMat.set_size(3, 3);
            rotationMat.set(values);
          }

          Q_solution = Q_solution * rotationMat;

	  // Now rotate the points to the same frame and sample neighborhoods again.
	  for( unsigned int ii = 0; ii < NumberOfIndicesWithinSphere; ii++ )
	  {
		  PointType RotatedPoint = ImagePatch2[ ii ];
		  // We also need vector representation of the point values
		  vnl_vector< RealType > RotatedPointVector( RotatedPoint.Size(), 0 );
		  // First move center of Patch 1 to center of Patch 2
		  for( unsigned int dd = 0; dd < ImageDimension; dd++ )
		  {
			  RotatedPoint[ dd ] -= CenterPointOfImage2[ dd ];
			  RotatedPointVector[ dd ] = RotatedPoint[ dd ];
		  }

		  // Now rotate RotatedPoint
		  RotatedPointVector = ( Q_solution ) * RotatedPointVector;
		  for( unsigned int dd = 0; dd < ImageDimension; dd++ )
		  {
			  RotatedPoint[ dd ] = RotatedPointVector[ dd ] + CenterPointOfImage2[ dd ];
		  }
		  if( Interpolator->IsInsideBuffer( RotatedPoint) )
		  {
			  VectorizedImagePatch2[ ii ] = Interpolator->Evaluate( RotatedPoint );
		  }
		  else OK = false;
	  }

  }
  return VectorizedImagePatch2;
}


template< class ImageType >
typename ImageType::Pointer ConvertVectorToSpatialImage( vnl_vector< typename ImageType::PixelType > &Vector,
      typename ImageType::Pointer Mask)
{
  typename ImageType::Pointer VectorAsSpatialImage = ImageType::New();
  VectorAsSpatialImage->SetOrigin(Mask->GetOrigin() );
  VectorAsSpatialImage->SetSpacing( Mask->GetSpacing() );
  VectorAsSpatialImage->SetRegions( Mask->GetLargestPossibleRegion() );
  VectorAsSpatialImage->SetDirection( Mask-> GetDirection() );
  VectorAsSpatialImage->Allocate();
  VectorAsSpatialImage->FillBuffer( itk::NumericTraits< typename ImageType::PixelType>::Zero );
  unsigned long VectorIndex = 0;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  IteratorType MaskIterator( Mask, Mask->GetLargestPossibleRegion() );
  for( MaskIterator.GoToBegin(); !MaskIterator.IsAtEnd(); ++MaskIterator)
  {
    if( MaskIterator.Get() >= 0.5 )
    {
      typename ImageType::PixelType Value = 0.0;
      if( VectorIndex < Vector.size() )
      {
	Value = Vector(VectorIndex);
      }
      else
      {
	std::cout << "Size of mask does not match size of vector to be written!" << std::endl;
	std::cout << "Exiting." << std::endl;
	std::exception();
      }
      VectorAsSpatialImage->SetPixel(MaskIterator.GetIndex(), Value);
      ++VectorIndex;
    }
    else
    {
      MaskIterator.Set( 0 );
    }
  }
  return VectorAsSpatialImage;
};



} // end namespace itk

#endif
