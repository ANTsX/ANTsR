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
#ifndef itkRIPMMARCImageFilter_h
#define itkRIPMMARCImageFilter_h

#include <exception>
#include <algorithm>
#include <vector>
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
#include "itkImageToImageFilter.h"

namespace itk {

/**
 * \class RIPMMARCImageFilter
 * \brief Implementation of a rotation invariant patch representation filter.
 *
 * \author Brian B. Avants and Benjamin M. Kandel
 *
 * Contributed by
 *
 * \par REFERENCE
 *
 * J. V. Manjon, P. Coupe, Luis Marti-Bonmati, D. L. Collins,
 * and M. Robles. "Decomposing cerebral blood flow MRI into functional and
 * structural components: a non-local approach based on prediction", Neuroimage,
 * 105:156-70, 2015. DOI: 10.1016/j.neuroimage.2014.10.052
 *
 * \ingroup ITKNoiseFiltering
 */

template< typename TInputImage, typename TOutputImage >
class RIPMMARCImageFilter :
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef RIPMMARCImageFilter     Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Runtime information support. */
  itkTypeMacro( RIPMMARCImageFilter, ImageToImageFilter );

  /** Standard New method. */
  itkNewMacro( Self );

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int,
                       TInputImage::ImageDimension );

  /** Some convenient typedefs. */
  typedef TInputImage                                    InputImageType;
  typedef typename InputImageType::Pointer               InputImagePointer;
  typedef typename InputImageType::PixelType             InputPixelType;
  typedef TOutputImage                                   OutputImageType;
  typedef typename InputImageType::RegionType            RegionType;
  typedef typename InputImageType::IndexType             InputIndexType;

  typedef InputImageType                                 MaskImageType;
  typedef typename MaskImageType::PixelType              MaskPixelType;
  typedef typename MaskImageType::PixelType              LabelType;

  typedef double                                         RealValueType;
  typedef float                                          RealType;
  typedef Image<RealType, ImageDimension>                RealImageType;
  typedef typename RealImageType::Pointer                RealImagePointer;
  typedef typename RealImageType::IndexType              IndexType;

  typedef ConstNeighborhoodIterator<RealImageType>             ConstNeighborhoodIteratorType;
  typedef typename ConstNeighborhoodIteratorType::RadiusType   NeighborhoodRadiusType;
  typedef typename ConstNeighborhoodIteratorType::OffsetType   NeighborhoodOffsetType;

  typedef vnl_matrix< RealValueType >                    vnlMatrixType;

  typedef itk::ConstNeighborhoodIterator< InputImageType >
    NeighborhoodIteratorType;
  typedef typename itk::CovariantVector< typename InputImageType::PixelType,
      ImageDimension>                                                  GradientPixelType;
  typedef typename itk::Image< GradientPixelType, ImageDimension >       GradientImageType;
  typedef typename itk::GradientRecursiveGaussianImageFilter< InputImageType,
      GradientImageType >                                          GradientImageFilterType;
  typedef typename GradientImageFilterType::Pointer                  GradientImageFilterPointer;
  typedef typename itk::NeighborhoodIterator< GradientImageType >    GradientNeighborhoodIteratorType;
  typedef typename itk::LinearInterpolateImageFunction< InputImageType,
      typename InputImageType::PixelType>                             ScalarInterpolatorType;
  typedef typename itk::SmartPointer< GradientImageType >            GradientImagePointer;
  typedef typename ScalarInterpolatorType::Pointer                   InterpPointer;

  typename InputImageType::RegionType               sphereRegion;
  typename InputImageType::IndexType                beginningOfSphereRegion;
  typename InputImageType::SizeType                 sizeOfSphereRegion;

  typedef typename InputImageType::PointType PointType;
  typedef vnl_vector< RealValueType > VectorType;


  /**
   * The image expected for input for noise correction.
   */
  void SetInput1( const InputImageType *image ) { this->SetInput( image ); }

  /**
   * Set mask image function.  If a binary mask image is specified, only
   * those input image voxels corresponding with the mask image.
   */
  void SetMaskImage( const MaskImageType *mask )
    {
    this->SetNthInput( 1, const_cast<MaskImageType *>( mask ) );
    }
  void SetInput2( const MaskImageType *mask ) { this->SetMaskImage( mask ); }

  /**
   * Get mask image function.  If a binary mask image is specified, only
   * those input image voxels corresponding with the mask image.
   */
  const MaskImageType* GetMaskImage() const
    {
    return static_cast<const MaskImageType*>( this->ProcessObject::GetInput( 1 ) );
    }

  /**
   * Get canonical frame image function.
   */
  InputImageType* GetCanonicalFrame() const
    {
    return this->m_CanonicalFrame;
    }

  /**
   * Get canonical frame image function.
   */
  InputImagePointer GetCanonicalFrameK( unsigned int k )
    {
    InputImagePointer eigenvecMaskImage;
    eigenvecMaskImage = this->GenerateMaskImageFromPatch( );
    VectorType canonicalEigenPatchAsVector =
        this->m_SignificantPatchEigenvectors.get_column( k );
    InputImagePointer ivec = this->ConvertVectorToSpatialImage(
            canonicalEigenPatchAsVector, eigenvecMaskImage );
    return ivec;
    }

  /**
   * Set canonical frame image function.
   */
  void SetCanonicalFrame( InputImageType* canfram )
    {
    this->m_CanonicalFrame = canfram;
    }

  /**
   * Employ rotation invariance.
   * Default = true.
   */
  itkSetMacro( RotationInvariant, bool );
  itkGetConstMacro( RotationInvariant, bool );
  itkBooleanMacro( RotationInvariant );

  /**
   * Employ verbose output.
   * Default = true.
   */
  itkSetMacro( Verbose, bool );
  itkGetConstMacro( Verbose, bool );
  itkBooleanMacro( Verbose );
  /**
   * Mean center the patches.
   * Default = true.
   */
  itkSetMacro( MeanCenterPatches, bool );
  itkGetConstMacro( MeanCenterPatches, bool );
  itkBooleanMacro( MeanCenterPatches );


  /**
   * LearnPatchBasis
   * Default = true.
   */
  itkSetMacro( LearnPatchBasis, bool );
  itkGetConstMacro( LearnPatchBasis, bool );
  itkBooleanMacro( LearnPatchBasis );


  /**
   * Number of samples to randomly select from the mask (FIXME replace with random mask).
   */
  itkSetMacro( NumberOfSamplePatches, unsigned int );
  itkGetConstMacro( NumberOfSamplePatches, unsigned int );

  /**
   * Set target variance explained.
   * If greater than zero, sets number of eignevectors.
   * If less than zero, sets target variance explained.
   */
  itkSetMacro( TargetVarianceExplained, RealType );
  itkGetConstMacro( TargetVarianceExplained, RealType );

  /**
   * Get achieved variance explained.
   */
  itkGetConstMacro( AchievedVarianceExplained, RealType );

  /**
   * Patch radius in real physical space (FIXME check this).
   */
  itkSetMacro( PatchRadius, RealType );
  itkGetConstMacro( PatchRadius, RealType );

  /**
   * Set or get the reference patch basis.
   */
  itkSetMacro( SignificantPatchEigenvectors, vnlMatrixType );
  itkGetConstMacro( SignificantPatchEigenvectors, vnlMatrixType );

  /**
   * Set or get the patch basis for the full image.
   */
  itkSetMacro( PatchesForAllPointsWithinMask, vnlMatrixType );
  itkGetConstMacro( PatchesForAllPointsWithinMask, vnlMatrixType );

  /**
   * Get the eigenvector coefficients for the full image.
   */
  itkSetMacro( EigenvectorCoefficients, vnlMatrixType );
  itkGetConstMacro( EigenvectorCoefficients, vnlMatrixType );

  void GetSamplePatchLocations( ); // FIXME
  void ExtractSamplePatches( ); // FIXME
  void ExtractAllPatches( void ); // FIXME
  void LearnEigenPatches( void ); // FIXME
  void ReorientSamplePatches( void ); // FIXME
  void ReorientAllPatches( void ); // FIXME
  void ProjectOnEigenPatches( void ); // FIXME


  vnl_vector< RealValueType > ReorientPatchToReferenceFrame(
    itk::ConstNeighborhoodIterator< TInputImage > GradientImageNeighborhood1,
    itk::ConstNeighborhoodIterator< TInputImage > GradientImageNeighborhood2,
    const typename TInputImage::Pointer MaskImage,
    const typename GradientImageType::Pointer GradientImage1,
    const typename GradientImageType::Pointer GradientImage2,
    InterpPointer Interpolator );

  InputImagePointer ConvertVectorToSpatialImage(
      vnl_vector< RealValueType > &Vector,
      InputImagePointer Mask );

  InputImagePointer GenerateMaskImageFromPatch( );

protected:
  RIPMMARCImageFilter();
  ~RIPMMARCImageFilter() {}

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:

  RIPMMARCImageFilter( const Self& ) ITK_DELETE_FUNCTION;
  void operator=( const Self& ) ITK_DELETE_FUNCTION;

  bool                                        m_RotationInvariant;
  bool                                        m_MeanCenterPatches;
  bool                                        m_LearnPatchBasis;
  bool                                        m_Verbose;
  RealType                                    m_PatchRadius;
  RealType                                    m_TargetVarianceExplained;
  RealType                                    m_AchievedVarianceExplained;

  typename InputImageType::Pointer            m_CanonicalFrame; // frame to rotate all patches to
  vnlMatrixType                               m_EigenvectorCoefficients;
  vnlMatrixType                               m_SignificantPatchEigenvectors;
  vnlMatrixType                               m_PatchesForAllPointsWithinMask;
  vnlMatrixType                               m_vectorizedPatchMatrix;
  vnl_matrix< int >                           m_patchSeedPoints;
  vnlMatrixType                               m_vectorizedSamplePatchMatrix;
  std::vector< unsigned int >                 m_IndicesWithinSphere;
  std::vector< RealValueType >                m_weights;
  long unsigned int                           m_numberOfVoxelsWithinMask;
  // amount of padding around eigenvector for constructing images
  unsigned int                                m_PaddingVoxels;
  unsigned int                                m_NumberOfSamplePatches;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRIPMMARCImageFilter.hxx"
#endif

#endif
