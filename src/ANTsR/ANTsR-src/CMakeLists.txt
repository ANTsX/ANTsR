cmake_minimum_required(VERSION 2.8)

project(ANTsR)

# set debug build flags
set( CMAKE_CXX_FLAGS_DEBUG "-g -Wall" )
set( CMAKE_C_FLAGS_DEBUG "-g -Wall" )

if( NOT R_INCLUDE_DIR )
  message( FATAL_ERROR "R_INCLUDE_DIR invalid. Perhaps R was not built as library." )
endif( NOT R_INCLUDE_DIR )
if( NOT RCPP_INCLUDE_DIR )
  message( FATAL_ERROR "RCPP_INCLUDE_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_INCLUDE_DIR )
if( NOT R_LIB_DIR )
  message( FATAL_ERROR "R_LIB_DIR invalid. Perhaps R was not built as library." )
endif( NOT R_LIB_DIR )
if( NOT RCPP_LIB_DIR )
  message( FATAL_ERROR "RCPP_LIB_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_LIB_DIR )

# extract the paths from the variables supplied for rcpp and r include and lib
string( REGEX MATCHALL "(^-I[^ ]+ *)|( -I[^ ]+ *)" RCPP_INCLUDE_DIR_MATCHES ${RCPP_INCLUDE_DIR} )
string( REGEX MATCHALL "(^-I[^ ]+ *)|( -I[^ ]+ *)" R_INCLUDE_DIR_MATCHES ${R_INCLUDE_DIR} )

foreach( loop_var IN LISTS RCPP_INCLUDE_DIR_MATCHES )
  string( REGEX REPLACE "-I([^ ]+)" "\\1" RCPP_INCLUDE_DIR ${loop_var} )
  include_directories( ${RCPP_INCLUDE_DIR} )
endforeach( loop_var )
 
foreach( loop_var IN LISTS R_INCLUDE_DIR_MATCHES )
  string( REGEX REPLACE "-I([^ ]+)" "\\1" R_INCLUDE_DIR ${loop_var} )
  include_directories( ${R_INCLUDE_DIR} )
endforeach( loop_var )

# ANTS
include_directories( ${ANTS_DIR}/include )
link_directories( ${ANTS_DIR}/ANTS/ANTS-build ) # rpath is not getting written for ANTS targets after installation so using build tree

set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )

# ANTsR objects
add_library(RantsApplyTransforms MODULE ${ANTsR_SOURCE_DIR}/src/antsApplyTransforms.cxx)
target_link_libraries(RantsApplyTransforms ${RCPP_LIB_DIR} ${R_LIB_DIR} antsApplyTransforms)
install( TARGETS RantsApplyTransforms DESTINATION . )

add_library(RANTS MODULE ${ANTsR_SOURCE_DIR}/src/ANTS.cxx)
target_link_libraries(RANTS ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTS)
install( TARGETS RANTS DESTINATION . )

add_library(RANTSIntegrateVectorField MODULE ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVectorField.cxx)
target_link_libraries(RANTSIntegrateVectorField ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTSIntegrateVectorField)
install( TARGETS RANTSIntegrateVectorField DESTINATION . )

add_library(RANTSIntegrateVelocityField MODULE ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVelocityField.cxx)
target_link_libraries(RANTSIntegrateVelocityField ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTSIntegrateVelocityField)
install( TARGETS RANTSIntegrateVelocityField DESTINATION . )

add_library(RANTSJacobian MODULE ${ANTsR_SOURCE_DIR}/src/ANTSJacobian.cxx)
target_link_libraries(RANTSJacobian ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTSJacobian)
install( TARGETS RANTSJacobian DESTINATION . )

add_library(RantsMotionCorr MODULE ${ANTsR_SOURCE_DIR}/src/antsMotionCorr.cxx)
target_link_libraries(RantsMotionCorr ${RCPP_LIB_DIR} ${R_LIB_DIR} antsMotionCorr)
install( TARGETS RantsMotionCorr DESTINATION . )

add_library(RantsRegistration MODULE ${ANTsR_SOURCE_DIR}/src/antsRegistration.cxx)
target_link_libraries(RantsRegistration ${RCPP_LIB_DIR} ${R_LIB_DIR} antsRegistration)
install( TARGETS RantsRegistration DESTINATION . )

add_library(RANTSUseDeformationFieldToGetAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/ANTSUseDeformationFieldToGetAffineTransform.cxx)
target_link_libraries(RANTSUseDeformationFieldToGetAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTSUseDeformationFieldToGetAffineTransform)
install( TARGETS RANTSUseDeformationFieldToGetAffineTransform DESTINATION . )

add_library(RANTSUseLandmarkImagesToGetAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/ANTSUseLandmarkImagesToGetAffineTransform.cxx)
target_link_libraries(RANTSUseLandmarkImagesToGetAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} ANTSUseLandmarkImagesToGetAffineTransform)
install( TARGETS RANTSUseLandmarkImagesToGetAffineTransform DESTINATION . )

add_library(RAtropos MODULE ${ANTsR_SOURCE_DIR}/src/Atropos.cxx)
target_link_libraries(RAtropos ${RCPP_LIB_DIR} ${R_LIB_DIR} Atropos)
install( TARGETS RAtropos DESTINATION . )

add_library(RAtroposMin MODULE ${ANTsR_SOURCE_DIR}/src/AtroposMin.cxx)
target_link_libraries(RAtroposMin ${RCPP_LIB_DIR} ${R_LIB_DIR} AtroposMin)
install( TARGETS RAtroposMin DESTINATION . )

add_library(RAverageAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/AverageAffineTransform.cxx)
target_link_libraries(RAverageAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} AverageAffineTransform)
install( TARGETS RAverageAffineTransform DESTINATION . )

add_library(RAverageImages MODULE ${ANTsR_SOURCE_DIR}/src/AverageImages.cxx)
target_link_libraries(RAverageImages ${RCPP_LIB_DIR} ${R_LIB_DIR} AverageImages)
install( TARGETS RAverageImages DESTINATION . )

add_library(RAverageTensorImages MODULE ${ANTsR_SOURCE_DIR}/src/AverageTensorImages.cxx)
target_link_libraries(RAverageTensorImages ${RCPP_LIB_DIR} ${R_LIB_DIR} AverageTensorImages)
install( TARGETS RAverageTensorImages DESTINATION . )

add_library(RCheckTopology MODULE ${ANTsR_SOURCE_DIR}/src/CheckTopology.cxx)
target_link_libraries(RCheckTopology ${RCPP_LIB_DIR} ${R_LIB_DIR} CheckTopology)
install( TARGETS RCheckTopology DESTINATION . )

add_library(RClusterImageStatistics MODULE ${ANTsR_SOURCE_DIR}/src/ClusterImageStatistics.cxx)
target_link_libraries(RClusterImageStatistics ${RCPP_LIB_DIR} ${R_LIB_DIR} ClusterImageStatistics)
install( TARGETS RClusterImageStatistics DESTINATION . )

add_library(RComposeMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/ComposeMultiTransform.cxx)
target_link_libraries(RComposeMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} ComposeMultiTransform)
install( TARGETS RComposeMultiTransform DESTINATION . )

add_library(RComputeSimilarityMetric MODULE ${ANTsR_SOURCE_DIR}/src/ComputeSimilarityMetric.cxx)
target_link_libraries(RComputeSimilarityMetric ${RCPP_LIB_DIR} ${R_LIB_DIR} ComputeSimilarityMetric)
install( TARGETS RComputeSimilarityMetric DESTINATION . )

add_library(RConvertImagePixelType MODULE ${ANTsR_SOURCE_DIR}/src/ConvertImagePixelType.cxx)
target_link_libraries(RConvertImagePixelType ${RCPP_LIB_DIR} ${R_LIB_DIR} ConvertImagePixelType)
install( TARGETS RConvertImagePixelType DESTINATION . )

add_library(RConvertScalarImageToRGB MODULE ${ANTsR_SOURCE_DIR}/src/ConvertScalarImageToRGB.cxx)
target_link_libraries(RConvertScalarImageToRGB ${RCPP_LIB_DIR} ${R_LIB_DIR} ConvertScalarImageToRGB)
install( TARGETS RConvertScalarImageToRGB DESTINATION . )

add_library(RConvertToJpg MODULE ${ANTsR_SOURCE_DIR}/src/ConvertToJpg.cxx)
target_link_libraries(RConvertToJpg ${RCPP_LIB_DIR} ${R_LIB_DIR} ConvertToJpg)
install( TARGETS RConvertToJpg DESTINATION . )

add_library(RConvertVectorFieldToVTK MODULE ${ANTsR_SOURCE_DIR}/src/ConvertVectorFieldToVTK.cxx)
target_link_libraries(RConvertVectorFieldToVTK ${RCPP_LIB_DIR} ${R_LIB_DIR} ConvertVectorFieldToVTK)
install( TARGETS RConvertVectorFieldToVTK DESTINATION . )

add_library(RCopyImageHeaderInformation MODULE ${ANTsR_SOURCE_DIR}/src/CopyImageHeaderInformation.cxx)
target_link_libraries(RCopyImageHeaderInformation ${RCPP_LIB_DIR} ${R_LIB_DIR} CopyImageHeaderInformation)
install( TARGETS RCopyImageHeaderInformation DESTINATION . )

add_library(RCreateDisplacementField MODULE ${ANTsR_SOURCE_DIR}/src/CreateDisplacementField.cxx)
target_link_libraries(RCreateDisplacementField ${RCPP_LIB_DIR} ${R_LIB_DIR} CreateDisplacementField)
install( TARGETS RCreateDisplacementField DESTINATION . )

add_library(RCreateDTICohort MODULE ${ANTsR_SOURCE_DIR}/src/CreateDTICohort.cxx)
target_link_libraries(RCreateDTICohort ${RCPP_LIB_DIR} ${R_LIB_DIR} CreateDTICohort)
install( TARGETS RCreateDTICohort DESTINATION . )

add_library(RCreateImage MODULE ${ANTsR_SOURCE_DIR}/src/CreateImage.cxx)
target_link_libraries(RCreateImage ${RCPP_LIB_DIR} ${R_LIB_DIR} CreateImage)
install( TARGETS RCreateImage DESTINATION . )

add_library(RCreateWarpedGridImage MODULE ${ANTsR_SOURCE_DIR}/src/CreateWarpedGridImage.cxx)
target_link_libraries(RCreateWarpedGridImage ${RCPP_LIB_DIR} ${R_LIB_DIR} CreateWarpedGridImage)
install( TARGETS RCreateWarpedGridImage DESTINATION . )

add_library(RExtractRegionFromImageByMask MODULE ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImageByMask.cxx)
target_link_libraries(RExtractRegionFromImageByMask ${RCPP_LIB_DIR} ${R_LIB_DIR} ExtractRegionFromImageByMask)
install( TARGETS RExtractRegionFromImageByMask DESTINATION . )

add_library(RExtractRegionFromImage MODULE ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImage.cxx)
target_link_libraries(RExtractRegionFromImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ExtractRegionFromImage)
install( TARGETS RExtractRegionFromImage DESTINATION . )

add_library(RExtractSliceFromImage MODULE ${ANTsR_SOURCE_DIR}/src/ExtractSliceFromImage.cxx)
target_link_libraries(RExtractSliceFromImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ExtractSliceFromImage)
install( TARGETS RExtractSliceFromImage DESTINATION . )

add_library(RGetMeshAndTopology MODULE ${ANTsR_SOURCE_DIR}/src/GetMeshAndTopology.cxx)
target_link_libraries(RGetMeshAndTopology ${RCPP_LIB_DIR} ${R_LIB_DIR} GetMeshAndTopology)
install( TARGETS RGetMeshAndTopology DESTINATION . )

add_library(RImageCompare MODULE ${ANTsR_SOURCE_DIR}/src/ImageCompare.cxx)
target_link_libraries(RImageCompare ${RCPP_LIB_DIR} ${R_LIB_DIR} ImageCompare)
install( TARGETS RImageCompare DESTINATION . )

add_library(RImageMath MODULE ${ANTsR_SOURCE_DIR}/src/ImageMath.cxx)
target_link_libraries(RImageMath ${RCPP_LIB_DIR} ${R_LIB_DIR} ImageMath)
install( TARGETS RImageMath DESTINATION . )

add_library(RImageSetStatistics MODULE ${ANTsR_SOURCE_DIR}/src/ImageSetStatistics.cxx)
target_link_libraries(RImageSetStatistics ${RCPP_LIB_DIR} ${R_LIB_DIR} ImageSetStatistics)
install( TARGETS RImageSetStatistics DESTINATION . )

add_library(RKellyKapowski MODULE ${ANTsR_SOURCE_DIR}/src/KellyKapowski.cxx)
target_link_libraries(RKellyKapowski ${RCPP_LIB_DIR} ${R_LIB_DIR} KellyKapowski)
install( TARGETS RKellyKapowski DESTINATION . )

add_library(RKellySlater MODULE ${ANTsR_SOURCE_DIR}/src/KellySlater.cxx)
target_link_libraries(RKellySlater ${RCPP_LIB_DIR} ${R_LIB_DIR} KellySlater)
install( TARGETS RKellySlater DESTINATION . )

add_library(RLabelClustersUniquely MODULE ${ANTsR_SOURCE_DIR}/src/LabelClustersUniquely.cxx)
target_link_libraries(RLabelClustersUniquely ${RCPP_LIB_DIR} ${R_LIB_DIR} LabelClustersUniquely)
install( TARGETS RLabelClustersUniquely DESTINATION . )

add_library(RLabelGeometryMeasures MODULE ${ANTsR_SOURCE_DIR}/src/LabelGeometryMeasures.cxx)
target_link_libraries(RLabelGeometryMeasures ${RCPP_LIB_DIR} ${R_LIB_DIR} LabelGeometryMeasures)
install( TARGETS RLabelGeometryMeasures DESTINATION . )

add_library(RLabelOverlapMeasures MODULE ${ANTsR_SOURCE_DIR}/src/LabelOverlapMeasures.cxx)
target_link_libraries(RLabelOverlapMeasures ${RCPP_LIB_DIR} ${R_LIB_DIR} LabelOverlapMeasures)
install( TARGETS RLabelOverlapMeasures DESTINATION . )

add_library(RLaplacianThickness MODULE ${ANTsR_SOURCE_DIR}/src/LaplacianThickness.cxx)
target_link_libraries(RLaplacianThickness ${RCPP_LIB_DIR} ${R_LIB_DIR} LaplacianThickness)
install( TARGETS RLaplacianThickness DESTINATION . )

add_library(RMeasureImageSimilarity MODULE ${ANTsR_SOURCE_DIR}/src/MeasureImageSimilarity.cxx)
target_link_libraries(RMeasureImageSimilarity ${RCPP_LIB_DIR} ${R_LIB_DIR} MeasureImageSimilarity)
install( TARGETS RMeasureImageSimilarity DESTINATION . )

add_library(RMeasureMinMaxMean MODULE ${ANTsR_SOURCE_DIR}/src/MeasureMinMaxMean.cxx)
target_link_libraries(RMeasureMinMaxMean ${RCPP_LIB_DIR} ${R_LIB_DIR} MeasureMinMaxMean)
install( TARGETS RMeasureMinMaxMean DESTINATION . )

add_library(RMemoryTest MODULE ${ANTsR_SOURCE_DIR}/src/MemoryTest.cxx)
target_link_libraries(RMemoryTest ${RCPP_LIB_DIR} ${R_LIB_DIR} MemoryTest)
install( TARGETS RMemoryTest DESTINATION . )

add_library(RMultiplyImages MODULE ${ANTsR_SOURCE_DIR}/src/MultiplyImages.cxx)
target_link_libraries(RMultiplyImages ${RCPP_LIB_DIR} ${R_LIB_DIR} MultiplyImages)
install( TARGETS RMultiplyImages DESTINATION . )

add_library(RN3BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N3BiasFieldCorrection.cxx)
target_link_libraries(RN3BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} N3BiasFieldCorrection)
install( TARGETS RN3BiasFieldCorrection DESTINATION . )

add_library(RN4BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N4BiasFieldCorrection.cxx)
target_link_libraries(RN4BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} N4BiasFieldCorrection)
install( TARGETS RN4BiasFieldCorrection DESTINATION . )

add_library(RPasteImageIntoImage MODULE ${ANTsR_SOURCE_DIR}/src/PasteImageIntoImage.cxx)
target_link_libraries(RPasteImageIntoImage ${RCPP_LIB_DIR} ${R_LIB_DIR} PasteImageIntoImage)
install( TARGETS RPasteImageIntoImage DESTINATION . )

add_library(RPermuteFlipImageOrientationAxes MODULE ${ANTsR_SOURCE_DIR}/src/PermuteFlipImageOrientationAxes.cxx)
target_link_libraries(RPermuteFlipImageOrientationAxes ${RCPP_LIB_DIR} ${R_LIB_DIR} PermuteFlipImageOrientationAxes)
install( TARGETS RPermuteFlipImageOrientationAxes DESTINATION . )

add_library(RPrintHeader MODULE ${ANTsR_SOURCE_DIR}/src/PrintHeader.cxx)
target_link_libraries(RPrintHeader ${RCPP_LIB_DIR} ${R_LIB_DIR} PrintHeader)
install( TARGETS RPrintHeader DESTINATION . )

add_library(RReorientTensorImage MODULE ${ANTsR_SOURCE_DIR}/src/ReorientTensorImage.cxx)
target_link_libraries(RReorientTensorImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ReorientTensorImage)
install( TARGETS RReorientTensorImage DESTINATION . )

add_library(RResampleImageBySpacing MODULE ${ANTsR_SOURCE_DIR}/src/ResampleImageBySpacing.cxx)
target_link_libraries(RResampleImageBySpacing ${RCPP_LIB_DIR} ${R_LIB_DIR} ResampleImageBySpacing)
install( TARGETS RResampleImageBySpacing DESTINATION . )

add_library(RResampleImage MODULE ${ANTsR_SOURCE_DIR}/src/ResampleImage.cxx)
target_link_libraries(RResampleImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ResampleImage)
install( TARGETS RResampleImage DESTINATION . )

add_library(RResetDirection MODULE ${ANTsR_SOURCE_DIR}/src/ResetDirection.cxx)
target_link_libraries(RResetDirection ${RCPP_LIB_DIR} ${R_LIB_DIR} ResetDirection)
install( TARGETS RResetDirection DESTINATION . )

add_library(Rsccan MODULE ${ANTsR_SOURCE_DIR}/src/sccan.cxx)
target_link_libraries(Rsccan ${RCPP_LIB_DIR} ${R_LIB_DIR} sccan)
install( TARGETS Rsccan DESTINATION . )

add_library(RSetDirectionByMatrix MODULE ${ANTsR_SOURCE_DIR}/src/SetDirectionByMatrix.cxx)
target_link_libraries(RSetDirectionByMatrix ${RCPP_LIB_DIR} ${R_LIB_DIR} SetDirectionByMatrix)
install( TARGETS RSetDirectionByMatrix DESTINATION . )

add_library(RSetOrigin MODULE ${ANTsR_SOURCE_DIR}/src/SetOrigin.cxx)
target_link_libraries(RSetOrigin ${RCPP_LIB_DIR} ${R_LIB_DIR} SetOrigin)
install( TARGETS RSetOrigin DESTINATION . )

add_library(RSetSpacing MODULE ${ANTsR_SOURCE_DIR}/src/SetSpacing.cxx)
target_link_libraries(RSetSpacing ${RCPP_LIB_DIR} ${R_LIB_DIR} SetSpacing)
install( TARGETS RSetSpacing DESTINATION . )

add_library(RSmoothImage MODULE ${ANTsR_SOURCE_DIR}/src/SmoothImage.cxx)
target_link_libraries(RSmoothImage ${RCPP_LIB_DIR} ${R_LIB_DIR} SmoothImage)
install( TARGETS RSmoothImage DESTINATION . )

add_library(RStackSlices MODULE ${ANTsR_SOURCE_DIR}/src/StackSlices.cxx)
target_link_libraries(RStackSlices ${RCPP_LIB_DIR} ${R_LIB_DIR} StackSlices)
install( TARGETS RStackSlices DESTINATION . )

add_library(RStudentsTestOnImages MODULE ${ANTsR_SOURCE_DIR}/src/StudentsTestOnImages.cxx)
target_link_libraries(RStudentsTestOnImages ${RCPP_LIB_DIR} ${R_LIB_DIR} StudentsTestOnImages)
install( TARGETS RStudentsTestOnImages DESTINATION . )

add_library(RSurfaceBasedSmoothing MODULE ${ANTsR_SOURCE_DIR}/src/SurfaceBasedSmoothing.cxx)
target_link_libraries(RSurfaceBasedSmoothing ${RCPP_LIB_DIR} ${R_LIB_DIR} SurfaceBasedSmoothing)
install( TARGETS RSurfaceBasedSmoothing DESTINATION . )

add_library(RSurfaceCurvature MODULE ${ANTsR_SOURCE_DIR}/src/SurfaceCurvature.cxx)
target_link_libraries(RSurfaceCurvature ${RCPP_LIB_DIR} ${R_LIB_DIR} SurfaceCurvature)
install( TARGETS RSurfaceCurvature DESTINATION . )

add_library(RTensorDerivedImage MODULE ${ANTsR_SOURCE_DIR}/src/TensorDerivedImage.cxx)
target_link_libraries(RTensorDerivedImage ${RCPP_LIB_DIR} ${R_LIB_DIR} TensorDerivedImage)
install( TARGETS RTensorDerivedImage DESTINATION . )

add_library(RThresholdImage MODULE ${ANTsR_SOURCE_DIR}/src/ThresholdImage.cxx)
target_link_libraries(RThresholdImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ThresholdImage)
install( TARGETS RThresholdImage DESTINATION . )

add_library(RTileImages MODULE ${ANTsR_SOURCE_DIR}/src/TileImages.cxx)
target_link_libraries(RTileImages ${RCPP_LIB_DIR} ${R_LIB_DIR} TileImages)
install( TARGETS RTileImages DESTINATION . )

add_library(RWarpImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpImageMultiTransform.cxx)
target_link_libraries(RWarpImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} WarpImageMultiTransform)
install( TARGETS RWarpImageMultiTransform DESTINATION . )

add_library(RWarpTensorImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpTensorImageMultiTransform.cxx)
target_link_libraries(RWarpTensorImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} WarpTensorImageMultiTransform)
install( TARGETS RWarpTensorImageMultiTransform DESTINATION . )

add_library(RWarpTimeSeriesImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpTimeSeriesImageMultiTransform.cxx)
target_link_libraries(RWarpTimeSeriesImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} WarpTimeSeriesImageMultiTransform)
install( TARGETS RWarpTimeSeriesImageMultiTransform DESTINATION . )

add_library(RWarpVTKPolyDataMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpVTKPolyDataMultiTransform.cxx)
target_link_libraries(RWarpVTKPolyDataMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} WarpVTKPolyDataMultiTransform)
install( TARGETS RWarpVTKPolyDataMultiTransform DESTINATION . )
