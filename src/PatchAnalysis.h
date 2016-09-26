struct patchAnalysisArgumentType
{
  std::string inputName;           // -i option
  std::string maskName;            // -m option
  std::string outProjectionName;   // -p option FIXME--used to be 'outPatchName'
  std::string eigvecName;          // -e option
  std::string outEigvecMatrixName; // -f option
  std::string inEigvecMatrixName; // -g option
  std::string outPatchName;       // -q option
  int patchSize;                  // -s option
  double targetVarianceExplained; // -t option
  int numberOfSamplePatches;      // -n option
  int verbose;                    // -v option
  int help;                       // -h option
  bool orientationInvariant;      // -o option
  bool meanCenter;                // -c option
};

template < class ImageType, const int dimension >
class TPatchAnalysis
{
public:
	TPatchAnalysis( patchAnalysisArgumentType &  );
	void SetArguments( patchAnalysisArgumentType & );
	void ReadInputImage( void );
	void ReadMaskImage( void );
	void GetSamplePatchLocations( void );
	void ExtractSamplePatches( void );
	void ExtractAllPatches( void );
	void LearnEigenPatches( void );
	void WriteEigenPatches( void );
	void WriteEigenPatchMatrix( void ); // TODO  -- done? not debugged
	void ReadEigenPatchMatrix( void );  // TODO -- done? not debugged
	void WritePatchMatrix( void );
	void ReorientSamplePatches( void );
	void ReorientAllPatches( void );
	void ProjectOnEigenPatches( void );
	void WriteProjections( void );

private:
	patchAnalysisArgumentType args;
	typename ImageType::Pointer                 canonicalFrame; // frame to rotate all patches to
	typename ImageType::Pointer                 inputImage;
	typename ImageType::Pointer                 maskImage;
	vnl_matrix < int >                          patchSeedPoints;
	vnl_matrix< typename ImageType::PixelType > vectorizedSamplePatchMatrix;
	std::vector< unsigned int >                  indicesWithinSphere;
	std::vector< double >                         weights;
	vnl_matrix< typename ImageType::PixelType > vectorizedPatchMatrix;
	vnl_matrix< typename ImageType::PixelType > significantPatchEigenvectors;
	vnl_matrix< typename ImageType::PixelType > patchesForAllPointsWithinMask;
	long unsigned int                            numberOfVoxelsWithinMask;
	vnl_matrix< typename ImageType::PixelType > eigenvectorCoefficients;
	int paddingVoxels; // amount of padding around eigenvector for constructing images
};

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
    );

template< class ImageType >
typename ImageType::Pointer ConvertVectorToSpatialImage(
		vnl_vector< typename ImageType::PixelType > &Vector,
		typename ImageType::Pointer Mask);
