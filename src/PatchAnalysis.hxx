#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include <vnl/algo/vnl_symmetric_eigensystem.h>
#include <itkImageRegionIterator.h>
#include "itkNeighborhoodIterator.h"
#include <stdio.h>

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


template< class ImageType >
typename ImageType::Pointer GenerateMaskImageFromPatch(
    std::vector< unsigned int > &indicesWithinSphere,
    int &radiusOfPatch,
    const  int dimension,
    const  int paddingVoxels = 2)
{
  int sizeOfImage = 2 * radiusOfPatch  + 2 *  paddingVoxels + 1;
  typename ImageType::Pointer maskImage = ImageType::New();
  typename ImageType::IndexType   start;
  typename ImageType::IndexType   beginningOfSphereRegion;
  typename ImageType::SizeType    sizeOfSphereRegion;
  typename ImageType::SizeType    size;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType   originPoint;
  typename ImageType::IndexType   originIndex;

  for( int dd = 0; dd < dimension; dd++ )
  {
    start[ dd ]   = 0;
    size[ dd ]    = sizeOfImage;
    spacing[ dd ] = 1.0;
    originPoint[ dd ] = originIndex[ dd ]  = 0;
    beginningOfSphereRegion[ dd ] = paddingVoxels + radiusOfPatch; // one for each side--this is correct
    sizeOfSphereRegion[ dd ] = radiusOfPatch * 2 + 1;
  }
  typename ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( originIndex );
  maskImage->SetRegions( region );
  maskImage->Allocate( );
  maskImage->SetSpacing( spacing );
  maskImage->SetOrigin( originPoint );
  typedef typename itk::ImageRegionIterator< ImageType > RegionIteratorType;
  RegionIteratorType regionIterator( maskImage, region );
  for ( regionIterator.GoToBegin(); !regionIterator.IsAtEnd(); ++regionIterator)
  {
    regionIterator.Set( 0.0 );
  }
  typename ImageType::RegionType sphereRegion;
  sphereRegion.SetSize( sizeOfSphereRegion );
  sphereRegion.SetIndex( beginningOfSphereRegion );
  typedef itk::NeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( radiusOfPatch );
  NeighborhoodIteratorType SphereRegionIterator( radius, maskImage, sphereRegion );

  typename ImageType::IndexType IndexWithinSphere;
  for( unsigned int ii = 0; ii < indicesWithinSphere.size(); ii++)
  {
    SphereRegionIterator.SetPixel( indicesWithinSphere[ ii ],  1.0 );
  }

  return maskImage;
}



template < class ImageType, const int dimension >
TPatchAnalysis< ImageType, dimension >::TPatchAnalysis( ArgumentType & inputArgs)
{
	SetArguments( inputArgs );
	this->paddingVoxels = 2;
	/*
	 * TODO: initialize mask for patches, patch iterator, etc.
	 */
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::SetArguments( ArgumentType & inputArgs)
{
	args = inputArgs;
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ReadInputImage()
{
	typedef itk::ImageFileReader< ImageType > ReaderType;
	typename ReaderType::Pointer inputImageReader = ReaderType::New();
    char inputNameChar[ args.inputName.size() + 1];
    strcpy( inputNameChar, args.inputName.c_str() );
    inputImageReader->SetFileName( inputNameChar );
    try
    {
    	inputImageReader->Update();
    }
    catch( itk::ExceptionObject& )
    {
    	std::cout << "Could not read input image." << std::endl;
    	exit( EXIT_FAILURE );
    }
    inputImage = inputImageReader->GetOutput();
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ReadMaskImage()
{
	typedef itk::ImageFileReader< ImageType > ReaderType;
	typename ReaderType::Pointer maskImageReader = ReaderType::New();
    char maskNameChar[ args.maskName.size() + 1];
    strcpy( maskNameChar, args.maskName.c_str() );
    maskImageReader->SetFileName( maskNameChar );
    try
    {
    	maskImageReader->Update();
    }
    catch( itk::ExceptionObject& )
    {
    	std::cout << "Could not read mask image." << std::endl;
    	exit( EXIT_FAILURE );
    }
    maskImage = maskImageReader->GetOutput();
}


template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::GetSamplePatchLocations()
{
	patchSeedPoints.set_size( args.numberOfSamplePatches , dimension );
	vnl_vector< int > testPatchSeed( dimension );
	int  patchSeedIterator = 0;
	int  patchSeedAttemptIterator = 0;
	typename ImageType::IndexType patchIndex;
	typename ImageType::SizeType inputSize =
			inputImage->GetLargestPossibleRegion().GetSize();
	if( args.verbose > 0 )
	{
		std::cout << "Attempting to find seed points. Looking for " << args.numberOfSamplePatches <<
				" points out of " << inputSize << " possible points." << std::endl;
	}

	srand( time( NULL) );
	while( patchSeedIterator < args.numberOfSamplePatches )
	{
		for( int i = 0; i < dimension; ++i)
		{
			patchIndex[ i ] = testPatchSeed( i ) = rand() % inputSize[ i ];
		}
		if (maskImage->GetPixel( patchIndex ) >= 1 )
		{
			patchSeedPoints.set_row( patchSeedIterator, testPatchSeed );
			++patchSeedIterator;
		}
		++patchSeedAttemptIterator;
	}
	if( args.verbose > 0)
	{
		std::cout << "Found " << patchSeedIterator <<
				" points in " << patchSeedAttemptIterator <<
				" attempts." << std::endl;
	}
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ExtractSamplePatches()
{
	// allocate matrix based on radial size of patch
	int i = 0 ;
	typename ImageType::IndexType patchIndex;

	for( int j = 0; j < dimension; ++j)
	{
		patchIndex[ j ] = patchSeedPoints( i, j );
	}
	typedef typename itk::NeighborhoodIterator< ImageType > IteratorType;
	typename IteratorType::RadiusType radius;
	radius.Fill( args.patchSize );
	IteratorType Iterator( radius, inputImage,
			inputImage->GetRequestedRegion() );
	typedef typename ImageType::IndexType IndexType;
	IndexType patchCenterIndex;
	patchCenterIndex.Fill( args.patchSize ); // for finding indices within sphere, pick point far enough away from edges
	Iterator.SetLocation( patchCenterIndex );

	// get indices within N-d sphere
	for( int ii = 0; ii < Iterator.Size(); ++ii)
	{
		IndexType index = Iterator.GetIndex( ii );
		float distanceFromPatchCenter = 0.0;
		for( int jj = 0; jj < dimension; ++jj)
		{
			distanceFromPatchCenter +=
					( index[jj] - patchCenterIndex[jj] ) *
					( index[jj] - patchCenterIndex[jj] );
		}
		distanceFromPatchCenter = sqrt(distanceFromPatchCenter);
		if( distanceFromPatchCenter <= args.patchSize )
		{
			indicesWithinSphere.push_back( ii );
			this->weights.push_back( 1.0 );
		}
	}
	std::cout << "Iterator.Size() is " << Iterator.Size() << std::endl;
	std::cout << "IndicesWithinSphere.size() is " << indicesWithinSphere.size() << std::endl;

	  // populate matrix with patch values from points in image
	this->vectorizedSamplePatchMatrix.set_size(
			args.numberOfSamplePatches , indicesWithinSphere.size() );
	this->vectorizedSamplePatchMatrix.fill( 0 );
	for( int i = 0; i < args.numberOfSamplePatches ; ++i)
	{
		for( int j = 0; j < dimension; ++j)
		{
			patchCenterIndex[ j ] = patchSeedPoints( i, j );
		}
		Iterator.SetLocation( patchCenterIndex );
		// get indices within N-d sphere
		for( int j = 0; j < indicesWithinSphere.size(); ++j)
		{
			this->vectorizedSamplePatchMatrix( i, j ) =
					Iterator.GetPixel( indicesWithinSphere[ j ] );
		}
		// mean-center all patches
		if(args.meanCenter) {
			this->vectorizedSamplePatchMatrix.set_row(i, this->vectorizedSamplePatchMatrix.get_row(i) -
					this->vectorizedSamplePatchMatrix.get_row(i).mean());

		}
	}
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::LearnEigenPatches( void )
{
	vnl_svd< typename ImageType::PixelType > svd( vectorizedSamplePatchMatrix );
	vnl_matrix< typename ImageType::PixelType > patchEigenvectors = svd.V();
    double sumOfEigenvalues = 0.0;
    for( int i = 0; i < svd.rank(); i++)
    {
    	sumOfEigenvalues += svd.W(i, i);
    }
    double partialSumOfEigenvalues = 0.0;
    double percentVarianceExplained = 0.0;
	int  i = 0;
	if ( args.targetVarianceExplained < 1 )
	{
		while( percentVarianceExplained < args.targetVarianceExplained && i < svd.rank())
		{
			partialSumOfEigenvalues += svd.W(i, i);
			percentVarianceExplained = partialSumOfEigenvalues /
											  sumOfEigenvalues;
			i++;
		}
		int numberOfSignificantEigenvectors = i;
		if( args.verbose > 0 )
		{
			std::cout << "It took " << numberOfSignificantEigenvectors << " eigenvectors to reach " <<
					args.targetVarianceExplained * 100 << "% variance explained." << std::endl;
		}
	} else {
		i = int(args.targetVarianceExplained);
	}
	significantPatchEigenvectors.set_size( patchEigenvectors.rows(), i);
	significantPatchEigenvectors = patchEigenvectors.get_n_columns(0, i);
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::WriteEigenPatches()
{
	typedef itk::ImageFileWriter< ImageType > ImageWriterType;
	typename ImageType::Pointer eigvecMaskImage;
	eigvecMaskImage = GenerateMaskImageFromPatch< ImageType >(
			indicesWithinSphere, args.patchSize, dimension);
	typename ImageWriterType::Pointer eigvecWriter = ImageWriterType::New();
	for ( unsigned int ii = 0; ii < significantPatchEigenvectors.columns() ; ii++)
	{
		vnl_vector< typename ImageType::PixelType > eigvec =
				significantPatchEigenvectors.get_column( ii );
		char imageIndex[50];
		int j = sprintf(imageIndex, "%03u", ii);
		eigvecWriter->SetInput( ConvertVectorToSpatialImage< ImageType>( eigvec,
						eigvecMaskImage) );
		std::string eigvecFileName = args.eigvecName + imageIndex + ".nii.gz" ;
		eigvecWriter->SetFileName(eigvecFileName);
		eigvecWriter->Update();
	}
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ExtractAllPatches()
{
	// get indices of points within mask
	typedef typename ImageType::IndexType IndexType;
	IndexType patchIndex;
	std::vector< typename ImageType::IndexType > nonZeroMaskIndices;
	typedef itk::ImageRegionIterator< ImageType > ImageIteratorType;
	ImageIteratorType maskImageIterator( maskImage , maskImage->GetLargestPossibleRegion());
	long unsigned int maskImagePointIter = 0;
	for(maskImageIterator.GoToBegin(); !maskImageIterator.IsAtEnd(); ++maskImageIterator)
	{
		if( maskImageIterator.Get() >= 1 ) // threshold at 1
		{
			nonZeroMaskIndices.push_back( maskImageIterator.GetIndex() );
			maskImagePointIter++;
		}
	}
	numberOfVoxelsWithinMask = maskImagePointIter;
	if( args.verbose > 0 ) std::cout << "Number of points within mask is " << numberOfVoxelsWithinMask << std::endl;

	patchesForAllPointsWithinMask.set_size(
			indicesWithinSphere.size(),  numberOfVoxelsWithinMask);
	if( args.verbose > 0 )
	{
		std::cout << "PatchesForAllPointsWithinMask is " << patchesForAllPointsWithinMask.rows() << "x" <<
				patchesForAllPointsWithinMask.columns() << "." << std::endl;
	}
	// extract patches
	typedef typename itk::NeighborhoodIterator< ImageType > IteratorType;
	typename IteratorType::RadiusType radius;
	radius.Fill( args.patchSize );
	IteratorType iterator( radius, inputImage,
			inputImage->GetRequestedRegion() );
	patchesForAllPointsWithinMask.fill( 0 );
	for( long unsigned int i = 0; i < numberOfVoxelsWithinMask; ++i)
	{
		patchIndex = nonZeroMaskIndices[ i ];
		iterator.SetLocation( patchIndex );
		// get indices within N-d sphere
		for( int j = 0; j < indicesWithinSphere.size(); ++j)
		{
			patchesForAllPointsWithinMask( j, i ) = iterator.GetPixel( indicesWithinSphere[ j ] );
		}
		// mean-center
		if(args.meanCenter) {
			this->patchesForAllPointsWithinMask.set_column(i,
					this->patchesForAllPointsWithinMask.get_column(i) -
					this->patchesForAllPointsWithinMask.get_column(i).mean());
		}
	}
	if( args.verbose > 0 ) std::cout << "Recorded patches for all points." << std::endl;
}

template< class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ReorientSamplePatches()
{
	typedef itk::NeighborhoodIterator< ImageType >                     NeighborhoodIteratorType;
	typedef typename itk::CovariantVector< typename ImageType::PixelType,
			dimension>                                                  GradientPixelType;
	typedef typename itk::Image< GradientPixelType, dimension >       GradientImageType;
	typedef typename itk::GradientRecursiveGaussianImageFilter< ImageType,
			GradientImageType >                                          GradientImageFilterType;
	typedef typename GradientImageFilterType::Pointer                  GradientImageFilterPointer;
	typedef typename itk::NeighborhoodIterator< GradientImageType >    GradientNeighborhoodIteratorType;
	typedef typename itk::LinearInterpolateImageFunction< ImageType,
			typename ImageType::PixelType>                             ScalarInterpolatorType;
	typedef typename itk::SmartPointer< GradientImageType >            GradientImagePointer;
	typedef typename ScalarInterpolatorType::Pointer                   InterpPointer;

	typename ImageType::RegionType               sphereRegion;
	typename ImageType::IndexType                beginningOfSphereRegion;
	typename ImageType::SizeType                 sizeOfSphereRegion;
	float gradientSigma =                          1.0;
	typename NeighborhoodIteratorType::RadiusType radius;

	GradientImageFilterPointer    movingGradientFilter = GradientImageFilterType::New();
	GradientImageFilterPointer    fixedGradientFilter =  GradientImageFilterType::New();
	InterpPointer interp1 = ScalarInterpolatorType::New();

	radius.Fill( this->args.patchSize );
	for( int ii = 0; ii < dimension; ii++)
	{
		beginningOfSphereRegion[ii] = this->paddingVoxels + this->args.patchSize;
		sizeOfSphereRegion[ii]      = this->args.patchSize * 2 + 1;
	}
	sphereRegion.SetSize( sizeOfSphereRegion );
	sphereRegion.SetIndex( beginningOfSphereRegion );

	typename ImageType::Pointer eigenvecMaskImage;
	eigenvecMaskImage = GenerateMaskImageFromPatch< ImageType >(
			this->indicesWithinSphere, this->args.patchSize, dimension, this->paddingVoxels);
	//NeighborhoodIteratorType regionIterator()
	vnl_vector< typename ImageType::PixelType > canonicalEigenPatchAsVector =
			this->significantPatchEigenvectors.get_column(0);
	this->canonicalFrame = ConvertVectorToSpatialImage< ImageType >(
			canonicalEigenPatchAsVector, eigenvecMaskImage );
	NeighborhoodIteratorType fixedIterator(radius, canonicalFrame, sphereRegion);
	// compute gradient of canonical frame once, outside the loop
	fixedGradientFilter->SetInput(canonicalFrame);
	fixedGradientFilter->SetSigma(gradientSigma);
	fixedGradientFilter->Update();
	typename GradientImageType::Pointer fixedGradientImage = fixedGradientFilter->GetOutput();

	std::cout << "vectorizedSamplePatchMatrix is " << this->vectorizedSamplePatchMatrix.rows() <<
			"x" << this->vectorizedSamplePatchMatrix.columns() << std::endl;
	for( long int ii = 0; ii < this->vectorizedSamplePatchMatrix.rows(); ii++)
	{
		vnl_vector< typename ImageType::PixelType > vectorizedPatch =
				this->vectorizedSamplePatchMatrix.get_row(ii);
		typename ImageType::Pointer movingImage = ConvertVectorToSpatialImage< ImageType >(
				vectorizedPatch, eigenvecMaskImage);
		NeighborhoodIteratorType movingIterator(radius, movingImage, sphereRegion);
		movingGradientFilter->SetInput(movingImage);
		movingGradientFilter->SetSigma(gradientSigma);
		movingGradientFilter->Update();
		interp1->SetInputImage(movingImage);
		typename GradientImageType::Pointer movingGradientImage = movingGradientFilter->GetOutput();
		vnl_vector< typename ImageType::PixelType > rotatedPatchAsVector =
				ReorientPatchToReferenceFrame< dimension, typename ImageType::PixelType,
				ImageType, GradientImageType, InterpPointer> (
						fixedIterator, movingIterator, eigenvecMaskImage,
						this->indicesWithinSphere,
						this->weights,
						fixedGradientImage,
						movingGradientImage,
						dimension,
						interp1);
		this->vectorizedSamplePatchMatrix.set_row(ii, rotatedPatchAsVector);


	}
}

template< class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ReorientAllPatches()
{
	typedef itk::NeighborhoodIterator< ImageType >                     NeighborhoodIteratorType;
	typedef typename itk::CovariantVector< typename ImageType::PixelType,
			dimension>                                                  GradientPixelType;
	typedef typename itk::Image< GradientPixelType, dimension >       GradientImageType;
	typedef typename itk::GradientRecursiveGaussianImageFilter< ImageType,
			GradientImageType >                                          GradientImageFilterType;
	typedef typename GradientImageFilterType::Pointer                  GradientImageFilterPointer;
	typedef typename itk::NeighborhoodIterator< GradientImageType >    GradientNeighborhoodIteratorType;
	typedef typename itk::LinearInterpolateImageFunction< ImageType,
			typename ImageType::PixelType>                             ScalarInterpolatorType;
	typedef typename itk::SmartPointer< GradientImageType >            GradientImagePointer;
	typedef typename ScalarInterpolatorType::Pointer                   InterpPointer;

	typename ImageType::RegionType               sphereRegion;
	typename ImageType::IndexType                beginningOfSphereRegion;
	typename ImageType::SizeType                 sizeOfSphereRegion;
	float gradientSigma =                          1.0;
	typename NeighborhoodIteratorType::RadiusType radius;

	GradientImageFilterPointer    movingGradientFilter = GradientImageFilterType::New();
	GradientImageFilterPointer    fixedGradientFilter =  GradientImageFilterType::New();
	InterpPointer interp1 = ScalarInterpolatorType::New();

	radius.Fill( this->args.patchSize );
	for( int ii = 0; ii < dimension; ii++)
	{
		beginningOfSphereRegion[ii] = this->paddingVoxels + this->args.patchSize;
		sizeOfSphereRegion[ii]      = this->args.patchSize * 2 + 1;
	}
	sphereRegion.SetSize( sizeOfSphereRegion );
	sphereRegion.SetIndex( beginningOfSphereRegion );

	typename ImageType::Pointer eigenvecMaskImage;
	eigenvecMaskImage = GenerateMaskImageFromPatch< ImageType >(
			this->indicesWithinSphere, this->args.patchSize, dimension, this->paddingVoxels);
	//NeighborhoodIteratorType regionIterator()
	vnl_vector< typename ImageType::PixelType > canonicalEigenPatchAsVector =
			this->significantPatchEigenvectors.get_column(1);
	// the SECOND eigenvector is canonical--1st is constant
	this->canonicalFrame = ConvertVectorToSpatialImage< ImageType >(
			canonicalEigenPatchAsVector, eigenvecMaskImage );
	NeighborhoodIteratorType fixedIterator(radius, canonicalFrame, sphereRegion);
	// compute gradient of canonical frame once, outside the loop
	fixedGradientFilter->SetInput(canonicalFrame);
	fixedGradientFilter->SetSigma(gradientSigma);
	fixedGradientFilter->Update();
	typename GradientImageType::Pointer fixedGradientImage = fixedGradientFilter->GetOutput();

	for( long int ii = 0; ii < this->patchesForAllPointsWithinMask.columns(); ii++)
	{
		vnl_vector< typename ImageType::PixelType > vectorizedPatch =
				this->patchesForAllPointsWithinMask.get_column(ii);
		typename ImageType::Pointer movingImage = ConvertVectorToSpatialImage< ImageType >(
				vectorizedPatch, eigenvecMaskImage);
		NeighborhoodIteratorType movingIterator(radius, movingImage, sphereRegion);
		movingGradientFilter->SetInput(movingImage);
		movingGradientFilter->SetSigma(gradientSigma);
		movingGradientFilter->Update();
		interp1->SetInputImage(movingImage);
		typename GradientImageType::Pointer movingGradientImage = movingGradientFilter->GetOutput();
		vnl_vector< typename ImageType::PixelType > rotatedPatchAsVector =
				ReorientPatchToReferenceFrame< dimension, typename ImageType::PixelType,
				ImageType, GradientImageType, InterpPointer> (
						fixedIterator, movingIterator, eigenvecMaskImage,
						this->indicesWithinSphere,
						this->weights,
						fixedGradientImage,
						movingGradientImage,
						dimension,
						interp1);
		this->patchesForAllPointsWithinMask.set_column(ii, rotatedPatchAsVector);


	}
}


template< class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ProjectOnEigenPatches()
{
	// perform regression from eigenvectors to images
	// Ax = b, where A is eigenvector matrix (number of indices
	// within patch x number of eigenvectors), x is coefficients
	// (number of eigenvectors x 1), b is patch values for a given index
	// (number of indices within patch x 1).
	// output, eigenvectorCoefficients, is then number of eigenvectors
	// x number of patches ('x' solutions for all patches).
	if (args.verbose > 0 ) std::cout << "Computing regression." << std::endl;
	eigenvectorCoefficients.set_size( significantPatchEigenvectors.columns(), numberOfVoxelsWithinMask );
	eigenvectorCoefficients.fill( 0 );
	vnl_svd< typename ImageType::PixelType > RegressionSVD(significantPatchEigenvectors);
	//  EigenvectorCoefficients =  RegressionSVD.solve(PatchesForAllPointsWithinMask);
	//  not feasible for large matrices
	for( long unsigned int i = 0; i < numberOfVoxelsWithinMask; ++i)
	{
		vnl_vector< typename ImageType::PixelType > PatchOfInterest =
				patchesForAllPointsWithinMask.get_column(i);
		vnl_vector< typename ImageType::PixelType > x( significantPatchEigenvectors.columns() );
		x.fill( 0 );
		x = RegressionSVD.solve(PatchOfInterest);
		eigenvectorCoefficients.set_column(i, x);
	}
	vnl_matrix< typename ImageType::PixelType > reconstructedPatches =
			significantPatchEigenvectors * eigenvectorCoefficients;
	vnl_matrix< typename ImageType::PixelType > error =
			reconstructedPatches - patchesForAllPointsWithinMask;
	vnl_vector< typename ImageType::PixelType > percentError(error.columns() );
	for( int i = 0; i < error.columns(); ++i)
	{
		percentError(i) = error.get_column(i).two_norm() /
				(patchesForAllPointsWithinMask.get_column(i).two_norm() + 1e-10);
	}
	if( args.verbose > 0 )
	{
		std::cout << "Average percent error is " << percentError.mean() * 100 << "%, with max of " <<
				percentError.max_value() * 100 << "%." <<  std::endl;
	}
}

template < class ImageType, const int dimension >
void TPatchAnalysis < ImageType, dimension >::WriteProjections()
{
	// convert eigenvectorCoefficients to image and write as .mha
	typedef  itk::Image< typename ImageType::PixelType, 2 > ImageType2D;
	typename ImageType2D::RegionType region;
	typename ImageType2D::IndexType start;
	start[0] = 0;
	start[1] = 0;
	typename ImageType2D::SizeType size;
	size[0] = this->eigenvectorCoefficients.rows();
	size[1] = this->eigenvectorCoefficients.columns();
	region.SetSize(size);
	region.SetIndex(start);
	typename ImageType2D::Pointer image = ImageType2D::New();
	image->SetRegions(region);
	image->Allocate();

	typename ImageType2D::IndexType pixelIndex;
	for( long int ii = 0; ii < this->eigenvectorCoefficients.rows(); ii++)
	{
		for(long int jj = 0; jj < this->eigenvectorCoefficients.columns(); jj++)
		{
			pixelIndex[0] = ii;
			pixelIndex[1] = jj;
			image->SetPixel(pixelIndex, this->eigenvectorCoefficients.get(ii, jj));
		}
	}
	typedef itk::ImageFileWriter< ImageType2D > WriterType;
	typename WriterType::Pointer writer = WriterType::New();
	writer->SetFileName(this->args.outProjectionName + ".mha");
	writer->SetInput(image);
	try
	{
		writer->Update();
	}
	catch(itk::ExceptionObject & err)
	{
		std::cerr << "Exception caught writing patch projections." << std::endl;
		std::cerr << err << std::endl;
	}

}


template < class ImageType, const int dimension >
void TPatchAnalysis < ImageType, dimension >::WritePatchMatrix()
{
	// write out patches for visualization
	typedef  itk::Image< typename ImageType::PixelType, 2 > ImageType2D;
	typename ImageType2D::RegionType region;
	typename ImageType2D::IndexType start;
	start[0] = 0;
	start[1] = 0;
	typename ImageType2D::SizeType size;
	size[0] = this->patchesForAllPointsWithinMask.rows();
	size[1] = this->patchesForAllPointsWithinMask.columns();
	region.SetSize(size);
	region.SetIndex(start);
	typename ImageType2D::Pointer image = ImageType2D::New();
	image->SetRegions(region);
	image->Allocate();

	typename ImageType2D::IndexType pixelIndex;
	for( long int ii = 0; ii < this->patchesForAllPointsWithinMask.rows(); ii++)
	{
		for(long int jj = 0; jj < this->patchesForAllPointsWithinMask.columns(); jj++)
		{
			pixelIndex[0] = ii;
			pixelIndex[1] = jj;
			image->SetPixel(pixelIndex, this->patchesForAllPointsWithinMask.get(ii, jj));
		}
	}
	typedef itk::ImageFileWriter< ImageType2D > WriterType;
	typename WriterType::Pointer writer = WriterType::New();
	writer->SetFileName(this->args.outPatchName + ".mha");
	writer->SetInput(image);
	try
	{
		writer->Update();
	}
	catch(itk::ExceptionObject & err)
	{
		std::cerr << "Exception caught writing patches." << std::endl;
		std::cerr << err << std::endl;
	}

}

template < class ImageType, const int dimension >
void TPatchAnalysis < ImageType, dimension >::WriteEigenPatchMatrix()
{
	// convert eigenvectorCoefficients to image and write as .mha
	typedef  itk::Image< typename ImageType::PixelType, 2 > ImageType2D;
	typename ImageType2D::RegionType region;
	typename ImageType2D::IndexType start;
	start[0] = 0;
	start[1] = 0;
	typename ImageType2D::SizeType size;
	size[0] = this->significantPatchEigenvectors.rows();
	size[1] = this->significantPatchEigenvectors.columns();
	region.SetSize(size);
	region.SetIndex(start);
	typename ImageType2D::Pointer image = ImageType2D::New();
	image->SetRegions(region);
	image->Allocate();

	typename ImageType2D::IndexType pixelIndex;
	for( long int ii = 0; ii < this->significantPatchEigenvectors.rows(); ii++)
	{
		for(long int jj = 0; jj < this->significantPatchEigenvectors.columns(); jj++)
		{
			pixelIndex[0] = ii;
			pixelIndex[1] = jj;
			image->SetPixel(pixelIndex, this->significantPatchEigenvectors.get(ii, jj));
		}
	}
	typedef itk::ImageFileWriter< ImageType2D > WriterType;
	typename WriterType::Pointer writer = WriterType::New();
	writer->SetFileName(this->args.outEigvecMatrixName + ".mha");
	writer->SetInput(image);
	try
	{
		writer->Update();
	}
	catch(itk::ExceptionObject & err)
	{
		std::cerr << "Exception caught writing patch projections." << std::endl;
		std::cerr << err << std::endl;
	}
}

template < class ImageType, const int dimension >
void TPatchAnalysis< ImageType, dimension >::ReadEigenPatchMatrix()
{
	typedef  itk::Image< typename ImageType::PixelType, 2 > ImageType2D;
	typedef itk::ImageFileReader< ImageType2D > ReaderType;
	typename ReaderType::Pointer eigenPatchMatrixReader = ReaderType::New();
	eigenPatchMatrixReader->SetFileName( args.inEigvecMatrixName );
	try
	{
		eigenPatchMatrixReader->Update();
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "Exception caught while reading eigenpatch matrix." << std::endl;
		std::cerr << err << std::endl;
	}
	typename ImageType2D::Pointer eigenvecImage = ImageType2D::New();
	eigenvecImage = eigenPatchMatrixReader->GetOutput();
	typedef itk::ImageRegionIteratorWithIndex< ImageType2D > IteratorType;
	IteratorType eigenPatchMatrixImageIterator( eigenvecImage, eigenvecImage->GetLargestPossibleRegion() );
	typename ImageType2D::SizeType size = eigenvecImage->GetLargestPossibleRegion().GetSize();
	significantPatchEigenvectors.set_size( size[0], size[1] );
	for(eigenPatchMatrixImageIterator.GoToBegin(); !eigenPatchMatrixImageIterator.IsAtEnd();
			++eigenPatchMatrixImageIterator)
	{
		typename ImageType2D::IndexType idx = eigenPatchMatrixImageIterator.GetIndex();
		significantPatchEigenvectors(idx[0], idx[1]) = eigenvecImage->GetPixel(idx);
	}

}

template < class PixelType, const int dimension >
void PatchAnalysis( ArgumentType & args )
{
	typedef itk::Image< PixelType, dimension > ImageType;
	TPatchAnalysis< ImageType, dimension > patchAnalysisObject( args );
	patchAnalysisObject.ReadInputImage( );
	patchAnalysisObject.ReadMaskImage(  );
	patchAnalysisObject.GetSamplePatchLocations( );
	patchAnalysisObject.ExtractSamplePatches( );
	if ( args.inEigvecMatrixName.empty() )
	{
		patchAnalysisObject.LearnEigenPatches( );
	} else {
		patchAnalysisObject.ReadEigenPatchMatrix();
	}
	patchAnalysisObject.ExtractAllPatches( );
	// because all patches are reoriented to the first (non-rotationally invariant)
	// eigenpatch, we must learn the eigenpatches even if we will in the end use
	// rotationally invariant features.
	if ( args.orientationInvariant )
	{
		patchAnalysisObject.ReorientSamplePatches();
		patchAnalysisObject.ReorientAllPatches();
		if ( args.inEigvecMatrixName.empty() )
			patchAnalysisObject.LearnEigenPatches(); // of reoriented samples
	}
	if( !args.eigvecName.empty() )
		patchAnalysisObject.WriteEigenPatches( );
	if( !args.outEigvecMatrixName.empty() )
		patchAnalysisObject.WriteEigenPatchMatrix() ;
	patchAnalysisObject.ProjectOnEigenPatches( );
	patchAnalysisObject.WriteProjections( );
	if (!args.outPatchName.empty())
		patchAnalysisObject.WritePatchMatrix();
}


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





template< unsigned int ImageDimension, class TRealType, class TImageType, 
  class TGradientImageType, class TInterpolator > 
  double PatchCorrelation( 
      itk::NeighborhoodIterator< TImageType > GradientImageNeighborhood1, 
      itk::NeighborhoodIterator< TImageType > GradientImageNeighborhood2, 
      std::vector< unsigned int > IndicesWithinSphere, 
      std::vector< TRealType > IndexWeights, 
      typename TGradientImageType::Pointer GradientImage1, 
      typename TGradientImageType::Pointer GradientImage2, 
      TInterpolator Interpolator )
{
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
  vnl_matrix< RealType > GradientMatrix1( NumberOfIndicesWithinSphere, ImageDimension ); 
  vnl_matrix< RealType > GradientMatrix2( NumberOfIndicesWithinSphere, ImageDimension ); 
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
      for( unsigned int jj = 0; jj < ImageDimension; jj++)
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
      VectorizedImagePatch1.push_back( Point1 ); 
      VectorizedImagePatch2.push_back( Point2 ); 
    }
    else return 0; 
  }
  RealType MeanOfImagePatch1 = VectorizedImagePatch1.mean(); 
  RealType MeanOfImagePatch2 = VectorizedImagePatch2.mean(); 
  VectorType CenteredVectorizedImagePatch1 = ( VectorizedImagePatch1 - MeanOfImagePatch1 ); 
  VectorType CenteredVectorizedImagePatch2 = ( VectorizedImagePatch2 - MeanOfImagePatch2 ); 
  RealType StDevOfImage1 = sqrt( CenteredVectorizedImagePatch1 - MeanOfImagePatch1 ); 
  RealType StDevOfImage2 = sqrt( CenteredVectorizedImagePatch2 - MeanOfImagePatch2 ); 
  RealType correlation = inner_product( CenteredVectorizedImagePatch1, 
      CenteredVectorizedImagePatch2 ) / ( StDevOfImage1 * StDevOfImage2 );

  bool OK = true; 
  
  vnl_matrix< RealType > CovarianceMatrixOfImage1 = GradientMatrix1.transpose() * GradientMatrix1; 
  vnl_matrix< RealType > CovarianceMatrixOfImage2 = GradientMatrix2.transpose() * GradientMatrix2; 
  vnl_symmetric_eigensystem< RealType > EigOfImage1( CovarianceMatrixOfImage1 ); 
  vnl_symmetric_eigensystem< RealType > EigOfImage2( CovarianceMatrixOfImage2 ); 

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
      RotatedPoint[ dd ] = RotatedPointVector + CenterPointOfImage2[ dd ];
    }
    if( Interpolator->IsInsideBuffer( RotatedPoint) )
    {
      VectorizedImagePatch2[ ii ] = Interpolator->Evaluate( RotatedPoint );
    }
    else OK = false; 
  }
  if( OK )
  {
    MeanOfImagePatch2 = VectorizedImagePatch2.mean(); 
    CenteredVectorizedImagePatch2 = ( VectorizedImagePatch2 - MeanOfImagePatch2 ); 
    StDevOfImage2 = sqrt( CenteredVectorizedImagePatch2.squared_magnitude() ); 
    correlation = inner_product( VectorizedImagePatch1, VectorizedImagePatch2 ) / 
      (StDevOfImage1 * StDevOfImage2 ); 
  }
  else correlation = 0; 

  if ( vnl_math_isnan( correlation ) || vnl_math_isinf( correlation )  ) return 0; 
  else return correlation;
}


