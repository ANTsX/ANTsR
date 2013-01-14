cmake_minimum_required(VERSION 2.8.7)

project(ANTsR)

# set debug build flags
set( CMAKE_CXX_FLAGS_DEBUG "-g -Wall" CACHE STRING "Debug flags" FORCE )
set( CMAKE_C_FLAGS_DEBUG "-g -Wall" CACHE STRING "Debug flags" FORCE )

# check if any of the following required directories are not set; 
if( NOT R_INCLUDE_DIR )
  message( FATAL_ERROR "R_INCLUDE_DIR invalid. Perhaps R was not built as library. Please Re-build R with '--enable-R-shlib'" )
endif( NOT R_INCLUDE_DIR )
if( NOT RCPP_INCLUDE_DIR )
  message( FATAL_ERROR "RCPP_INCLUDE_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_INCLUDE_DIR )
if( NOT R_LIB_DIR )
  message( FATAL_ERROR "R_LIB_DIR invalid. Perhaps R was not built as library. Please Re-build R with '--enable-R-shlib'" )
endif( NOT R_LIB_DIR )
if( NOT RCPP_LIB_DIR )
  message( FATAL_ERROR "RCPP_LIB_DIR invalid. Perhaps Rcpp has not been installed." )
endif( NOT RCPP_LIB_DIR )

# extract the paths from the variables supplied for rcpp and r include and lib; the paths include the flags to compiler and linker
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

# ITK ; ITK_DIR is set by caller
find_package( ITK REQUIRED )
include( ${ITK_USE_FILE} )

# ANTS ; ANTS_DIR is set by caller
include_directories( ${ANTS_DIR}/include )
link_directories( ${ANTS_DIR}/lib )

# all targets must have rpath installed so R can find the dependencies when loading them
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )

# ANTsR targets
add_library(RantsApplyTransforms MODULE ${ANTsR_SOURCE_DIR}/src/antsApplyTransforms.cxx)
target_link_libraries(RantsApplyTransforms ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsApplyTransforms )
install( TARGETS RantsApplyTransforms DESTINATION . )

#add_library(RANTS MODULE ${ANTsR_SOURCE_DIR}/src/ANTS.cxx)
#target_link_libraries(RANTS ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTS )
#install( TARGETS RANTS DESTINATION . )

#add_library(RANTSIntegrateVectorField MODULE ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVectorField.cxx)
#target_link_libraries(RANTSIntegrateVectorField ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTSIntegrateVectorField )
#install( TARGETS RANTSIntegrateVectorField DESTINATION . )

#add_library(RANTSIntegrateVelocityField MODULE ${ANTsR_SOURCE_DIR}/src/ANTSIntegrateVelocityField.cxx)
#target_link_libraries(RANTSIntegrateVelocityField ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTSIntegrateVelocityField )
#install( TARGETS RANTSIntegrateVelocityField DESTINATION . )

#add_library(RANTSJacobian MODULE ${ANTsR_SOURCE_DIR}/src/ANTSJacobian.cxx)
#target_link_libraries(RANTSJacobian ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTSJacobian )
#install( TARGETS RANTSJacobian DESTINATION . )

add_library(RantsMotionCorr MODULE ${ANTsR_SOURCE_DIR}/src/antsMotionCorr.cxx)
target_link_libraries(RantsMotionCorr ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsMotionCorr )
install( TARGETS RantsMotionCorr DESTINATION . )

add_library(RantsRegistration MODULE ${ANTsR_SOURCE_DIR}/src/antsRegistration.cxx)
target_link_libraries(RantsRegistration ${RCPP_LIB_DIR} ${R_LIB_DIR} l_antsRegistration )
install( TARGETS RantsRegistration DESTINATION . )

#add_library(RANTSUseDeformationFieldToGetAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/ANTSUseDeformationFieldToGetAffineTransform.cxx)
#target_link_libraries(RANTSUseDeformationFieldToGetAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTSUseDeformationFieldToGetAffineTransform )
#install( TARGETS RANTSUseDeformationFieldToGetAffineTransform DESTINATION . )

#add_library(RANTSUseLandmarkImagesToGetAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/ANTSUseLandmarkImagesToGetAffineTransform.cxx)
#target_link_libraries(RANTSUseLandmarkImagesToGetAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ANTSUseLandmarkImagesToGetAffineTransform )
#install( TARGETS RANTSUseLandmarkImagesToGetAffineTransform DESTINATION . )

add_library(RAtropos MODULE ${ANTsR_SOURCE_DIR}/src/Atropos.cxx)
target_link_libraries(RAtropos ${RCPP_LIB_DIR} ${R_LIB_DIR} l_Atropos )
install( TARGETS RAtropos DESTINATION . )

#add_library(RAtroposMin MODULE ${ANTsR_SOURCE_DIR}/src/AtroposMin.cxx)
#target_link_libraries(RAtroposMin ${RCPP_LIB_DIR} ${R_LIB_DIR} l_AtroposMin )
#install( TARGETS RAtroposMin DESTINATION . )

#add_library(RAverageAffineTransform MODULE ${ANTsR_SOURCE_DIR}/src/AverageAffineTransform.cxx)
#target_link_libraries(RAverageAffineTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_AverageAffineTransform )
#install( TARGETS RAverageAffineTransform DESTINATION . )

#add_library(RAverageImages MODULE ${ANTsR_SOURCE_DIR}/src/AverageImages.cxx)
#target_link_libraries(RAverageImages ${RCPP_LIB_DIR} ${R_LIB_DIR} l_AverageImages )
#install( TARGETS RAverageImages DESTINATION . )

#add_library(RAverageTensorImages MODULE ${ANTsR_SOURCE_DIR}/src/AverageTensorImages.cxx)
#target_link_libraries(RAverageTensorImages ${RCPP_LIB_DIR} ${R_LIB_DIR} l_AverageTensorImages )
#install( TARGETS RAverageTensorImages DESTINATION . )

#add_library(RCheckTopology MODULE ${ANTsR_SOURCE_DIR}/src/CheckTopology.cxx)
#target_link_libraries(RCheckTopology ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CheckTopology )
#install( TARGETS RCheckTopology DESTINATION . )

#add_library(RClusterImageStatistics MODULE ${ANTsR_SOURCE_DIR}/src/ClusterImageStatistics.cxx)
#target_link_libraries(RClusterImageStatistics ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ClusterImageStatistics )
#install( TARGETS RClusterImageStatistics DESTINATION . )

#add_library(RComposeMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/ComposeMultiTransform.cxx)
#target_link_libraries(RComposeMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ComposeMultiTransform )
#install( TARGETS RComposeMultiTransform DESTINATION . )

#add_library(RComputeSimilarityMetric MODULE ${ANTsR_SOURCE_DIR}/src/ComputeSimilarityMetric.cxx)
#target_link_libraries(RComputeSimilarityMetric ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ComputeSimilarityMetric )
#install( TARGETS RComputeSimilarityMetric DESTINATION . )

#add_library(RConvertImagePixelType MODULE ${ANTsR_SOURCE_DIR}/src/ConvertImagePixelType.cxx)
#target_link_libraries(RConvertImagePixelType ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ConvertImagePixelType )
#install( TARGETS RConvertImagePixelType DESTINATION . )

#add_library(RConvertScalarImageToRGB MODULE ${ANTsR_SOURCE_DIR}/src/ConvertScalarImageToRGB.cxx)
#target_link_libraries(RConvertScalarImageToRGB ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ConvertScalarImageToRGB )
#install( TARGETS RConvertScalarImageToRGB DESTINATION . )

#add_library(RConvertToJpg MODULE ${ANTsR_SOURCE_DIR}/src/ConvertToJpg.cxx)
#target_link_libraries(RConvertToJpg ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ConvertToJpg )
#install( TARGETS RConvertToJpg DESTINATION . )

#add_library(RConvertVectorFieldToVTK MODULE ${ANTsR_SOURCE_DIR}/src/ConvertVectorFieldToVTK.cxx)
#target_link_libraries(RConvertVectorFieldToVTK ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ConvertVectorFieldToVTK )
#install( TARGETS RConvertVectorFieldToVTK DESTINATION . )

#add_library(RCopyImageHeaderInformation MODULE ${ANTsR_SOURCE_DIR}/src/CopyImageHeaderInformation.cxx)
#target_link_libraries(RCopyImageHeaderInformation ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CopyImageHeaderInformation )
#install( TARGETS RCopyImageHeaderInformation DESTINATION . )

#add_library(RCreateDisplacementField MODULE ${ANTsR_SOURCE_DIR}/src/CreateDisplacementField.cxx)
#target_link_libraries(RCreateDisplacementField ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CreateDisplacementField )
#install( TARGETS RCreateDisplacementField DESTINATION . )

#add_library(RCreateDTICohort MODULE ${ANTsR_SOURCE_DIR}/src/CreateDTICohort.cxx)
#target_link_libraries(RCreateDTICohort ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CreateDTICohort )
#install( TARGETS RCreateDTICohort DESTINATION . )

#add_library(RCreateImage MODULE ${ANTsR_SOURCE_DIR}/src/CreateImage.cxx)
#target_link_libraries(RCreateImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CreateImage )
#install( TARGETS RCreateImage DESTINATION . )

#add_library(RCreateWarpedGridImage MODULE ${ANTsR_SOURCE_DIR}/src/CreateWarpedGridImage.cxx)
#target_link_libraries(RCreateWarpedGridImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_CreateWarpedGridImage )
#install( TARGETS RCreateWarpedGridImage DESTINATION . )

#add_library(RExtractRegionFromImageByMask MODULE ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImageByMask.cxx)
#target_link_libraries(RExtractRegionFromImageByMask ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ExtractRegionFromImageByMask )
#install( TARGETS RExtractRegionFromImageByMask DESTINATION . )

#add_library(RExtractRegionFromImage MODULE ${ANTsR_SOURCE_DIR}/src/ExtractRegionFromImage.cxx)
#target_link_libraries(RExtractRegionFromImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ExtractRegionFromImage )
#install( TARGETS RExtractRegionFromImage DESTINATION . )

#add_library(RExtractSliceFromImage MODULE ${ANTsR_SOURCE_DIR}/src/ExtractSliceFromImage.cxx)
#target_link_libraries(RExtractSliceFromImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ExtractSliceFromImage )
#install( TARGETS RExtractSliceFromImage DESTINATION . )

#add_library(RGetMeshAndTopology MODULE ${ANTsR_SOURCE_DIR}/src/GetMeshAndTopology.cxx)
#target_link_libraries(RGetMeshAndTopology ${RCPP_LIB_DIR} ${R_LIB_DIR} l_GetMeshAndTopology )
#install( TARGETS RGetMeshAndTopology DESTINATION . )

#add_library(RImageCompare MODULE ${ANTsR_SOURCE_DIR}/src/ImageCompare.cxx)
#target_link_libraries(RImageCompare ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ImageCompare )
#install( TARGETS RImageCompare DESTINATION . )

add_library(RImageMath MODULE ${ANTsR_SOURCE_DIR}/src/ImageMath.cxx)
target_link_libraries(RImageMath ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ImageMath )
install( TARGETS RImageMath DESTINATION . )

#add_library(RImageSetStatistics MODULE ${ANTsR_SOURCE_DIR}/src/ImageSetStatistics.cxx)
#target_link_libraries(RImageSetStatistics ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ImageSetStatistics )
#install( TARGETS RImageSetStatistics DESTINATION . )

#add_library(RKellyKapowski MODULE ${ANTsR_SOURCE_DIR}/src/KellyKapowski.cxx)
#target_link_libraries(RKellyKapowski ${RCPP_LIB_DIR} ${R_LIB_DIR} l_KellyKapowski )
#install( TARGETS RKellyKapowski DESTINATION . )

#add_library(RKellySlater MODULE ${ANTsR_SOURCE_DIR}/src/KellySlater.cxx)
#target_link_libraries(RKellySlater ${RCPP_LIB_DIR} ${R_LIB_DIR} l_KellySlater )
#install( TARGETS RKellySlater DESTINATION . )

#add_library(RLabelClustersUniquely MODULE ${ANTsR_SOURCE_DIR}/src/LabelClustersUniquely.cxx)
#target_link_libraries(RLabelClustersUniquely ${RCPP_LIB_DIR} ${R_LIB_DIR} l_LabelClustersUniquely )
#install( TARGETS RLabelClustersUniquely DESTINATION . )

#add_library(RLabelGeometryMeasures MODULE ${ANTsR_SOURCE_DIR}/src/LabelGeometryMeasures.cxx)
#target_link_libraries(RLabelGeometryMeasures ${RCPP_LIB_DIR} ${R_LIB_DIR} l_LabelGeometryMeasures )
#install( TARGETS RLabelGeometryMeasures DESTINATION . )

#add_library(RLabelOverlapMeasures MODULE ${ANTsR_SOURCE_DIR}/src/LabelOverlapMeasures.cxx)
#target_link_libraries(RLabelOverlapMeasures ${RCPP_LIB_DIR} ${R_LIB_DIR} l_LabelOverlapMeasures )
#install( TARGETS RLabelOverlapMeasures DESTINATION . )

#add_library(RLaplacianThickness MODULE ${ANTsR_SOURCE_DIR}/src/LaplacianThickness.cxx)
#target_link_libraries(RLaplacianThickness ${RCPP_LIB_DIR} ${R_LIB_DIR} l_LaplacianThickness )
#install( TARGETS RLaplacianThickness DESTINATION . )

#add_library(RMeasureImageSimilarity MODULE ${ANTsR_SOURCE_DIR}/src/MeasureImageSimilarity.cxx)
#target_link_libraries(RMeasureImageSimilarity ${RCPP_LIB_DIR} ${R_LIB_DIR} l_MeasureImageSimilarity )
#install( TARGETS RMeasureImageSimilarity DESTINATION . )

#add_library(RMeasureMinMaxMean MODULE ${ANTsR_SOURCE_DIR}/src/MeasureMinMaxMean.cxx)
#target_link_libraries(RMeasureMinMaxMean ${RCPP_LIB_DIR} ${R_LIB_DIR} l_MeasureMinMaxMean )
#install( TARGETS RMeasureMinMaxMean DESTINATION . )

#add_library(RMemoryTest MODULE ${ANTsR_SOURCE_DIR}/src/MemoryTest.cxx)
#target_link_libraries(RMemoryTest ${RCPP_LIB_DIR} ${R_LIB_DIR} l_MemoryTest )
#install( TARGETS RMemoryTest DESTINATION . )

#add_library(RMultiplyImages MODULE ${ANTsR_SOURCE_DIR}/src/MultiplyImages.cxx)
#target_link_libraries(RMultiplyImages ${RCPP_LIB_DIR} ${R_LIB_DIR} l_MultiplyImages )
#install( TARGETS RMultiplyImages DESTINATION . )

add_library(RN3BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N3BiasFieldCorrection.cxx)
target_link_libraries(RN3BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} l_N3BiasFieldCorrection )
install( TARGETS RN3BiasFieldCorrection DESTINATION . )

#add_library(RN4BiasFieldCorrection MODULE ${ANTsR_SOURCE_DIR}/src/N4BiasFieldCorrection.cxx)
#target_link_libraries(RN4BiasFieldCorrection ${RCPP_LIB_DIR} ${R_LIB_DIR} l_N4BiasFieldCorrection )
#install( TARGETS RN4BiasFieldCorrection DESTINATION . )

#add_library(RPasteImageIntoImage MODULE ${ANTsR_SOURCE_DIR}/src/PasteImageIntoImage.cxx)
#target_link_libraries(RPasteImageIntoImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_PasteImageIntoImage )
#install( TARGETS RPasteImageIntoImage DESTINATION . )

#add_library(RPermuteFlipImageOrientationAxes MODULE ${ANTsR_SOURCE_DIR}/src/PermuteFlipImageOrientationAxes.cxx)
#target_link_libraries(RPermuteFlipImageOrientationAxes ${RCPP_LIB_DIR} ${R_LIB_DIR} l_PermuteFlipImageOrientationAxes )
#install( TARGETS RPermuteFlipImageOrientationAxes DESTINATION . )

#add_library(RPrintHeader MODULE ${ANTsR_SOURCE_DIR}/src/PrintHeader.cxx)
#target_link_libraries(RPrintHeader ${RCPP_LIB_DIR} ${R_LIB_DIR} l_PrintHeader )
#install( TARGETS RPrintHeader DESTINATION . )

#add_library(RReorientTensorImage MODULE ${ANTsR_SOURCE_DIR}/src/ReorientTensorImage.cxx)
#target_link_libraries(RReorientTensorImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ReorientTensorImage )
#install( TARGETS RReorientTensorImage DESTINATION . )

#add_library(RResampleImageBySpacing MODULE ${ANTsR_SOURCE_DIR}/src/ResampleImageBySpacing.cxx)
#target_link_libraries(RResampleImageBySpacing ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ResampleImageBySpacing )
#install( TARGETS RResampleImageBySpacing DESTINATION . )

#add_library(RResampleImage MODULE ${ANTsR_SOURCE_DIR}/src/ResampleImage.cxx)
#target_link_libraries(RResampleImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ResampleImage )
#install( TARGETS RResampleImage DESTINATION . )

#add_library(RResetDirection MODULE ${ANTsR_SOURCE_DIR}/src/ResetDirection.cxx)
#target_link_libraries(RResetDirection ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ResetDirection )
#install( TARGETS RResetDirection DESTINATION . )

add_library(Rsccan MODULE ${ANTsR_SOURCE_DIR}/src/sccan.cxx)
target_link_libraries(Rsccan ${RCPP_LIB_DIR} ${R_LIB_DIR} l_sccan )
install( TARGETS Rsccan DESTINATION . )

#add_library(RSetDirectionByMatrix MODULE ${ANTsR_SOURCE_DIR}/src/SetDirectionByMatrix.cxx)
#target_link_libraries(RSetDirectionByMatrix ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SetDirectionByMatrix )
#install( TARGETS RSetDirectionByMatrix DESTINATION . )

#add_library(RSetOrigin MODULE ${ANTsR_SOURCE_DIR}/src/SetOrigin.cxx)
#target_link_libraries(RSetOrigin ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SetOrigin )
#install( TARGETS RSetOrigin DESTINATION . )

#add_library(RSetSpacing MODULE ${ANTsR_SOURCE_DIR}/src/SetSpacing.cxx)
#target_link_libraries(RSetSpacing ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SetSpacing )
#install( TARGETS RSetSpacing DESTINATION . )

add_library(RSmoothImage MODULE ${ANTsR_SOURCE_DIR}/src/SmoothImage.cxx)
target_link_libraries(RSmoothImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SmoothImage )
install( TARGETS RSmoothImage DESTINATION . )

#add_library(RStackSlices MODULE ${ANTsR_SOURCE_DIR}/src/StackSlices.cxx)
#target_link_libraries(RStackSlices ${RCPP_LIB_DIR} ${R_LIB_DIR} l_StackSlices )
#install( TARGETS RStackSlices DESTINATION . )

#add_library(RStudentsTestOnImages MODULE ${ANTsR_SOURCE_DIR}/src/StudentsTestOnImages.cxx)
#target_link_libraries(RStudentsTestOnImages ${RCPP_LIB_DIR} ${R_LIB_DIR} l_StudentsTestOnImages )
#install( TARGETS RStudentsTestOnImages DESTINATION . )

#add_library(RSurfaceBasedSmoothing MODULE ${ANTsR_SOURCE_DIR}/src/SurfaceBasedSmoothing.cxx)
#target_link_libraries(RSurfaceBasedSmoothing ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SurfaceBasedSmoothing )
#install( TARGETS RSurfaceBasedSmoothing DESTINATION . )

#add_library(RSurfaceCurvature MODULE ${ANTsR_SOURCE_DIR}/src/SurfaceCurvature.cxx)
#target_link_libraries(RSurfaceCurvature ${RCPP_LIB_DIR} ${R_LIB_DIR} l_SurfaceCurvature )
#install( TARGETS RSurfaceCurvature DESTINATION . )

#add_library(RTensorDerivedImage MODULE ${ANTsR_SOURCE_DIR}/src/TensorDerivedImage.cxx)
#target_link_libraries(RTensorDerivedImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_TensorDerivedImage )
#install( TARGETS RTensorDerivedImage DESTINATION . )

add_library(RThresholdImage MODULE ${ANTsR_SOURCE_DIR}/src/ThresholdImage.cxx)
target_link_libraries(RThresholdImage ${RCPP_LIB_DIR} ${R_LIB_DIR} l_ThresholdImage )
install( TARGETS RThresholdImage DESTINATION . )

#add_library(RTileImages MODULE ${ANTsR_SOURCE_DIR}/src/TileImages.cxx)
#target_link_libraries(RTileImages ${RCPP_LIB_DIR} ${R_LIB_DIR} l_TileImages )
#install( TARGETS RTileImages DESTINATION . )

#add_library(RWarpImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpImageMultiTransform.cxx)
#target_link_libraries(RWarpImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_WarpImageMultiTransform )
#install( TARGETS RWarpImageMultiTransform DESTINATION . )

#add_library(RWarpTensorImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpTensorImageMultiTransform.cxx)
#target_link_libraries(RWarpTensorImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_WarpTensorImageMultiTransform )
#install( TARGETS RWarpTensorImageMultiTransform DESTINATION . )

#add_library(RWarpTimeSeriesImageMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpTimeSeriesImageMultiTransform.cxx)
#target_link_libraries(RWarpTimeSeriesImageMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_WarpTimeSeriesImageMultiTransform )
#install( TARGETS RWarpTimeSeriesImageMultiTransform DESTINATION . )

#add_library(RWarpVTKPolyDataMultiTransform MODULE ${ANTsR_SOURCE_DIR}/src/WarpVTKPolyDataMultiTransform.cxx)
#target_link_libraries(RWarpVTKPolyDataMultiTransform ${RCPP_LIB_DIR} ${R_LIB_DIR} l_WarpVTKPolyDataMultiTransform )
#install( TARGETS RWarpVTKPolyDataMultiTransform DESTINATION . )

add_library( RantsImageRead MODULE ${ANTsR_SOURCE_DIR}/src/antsImageRead.cxx )
target_link_libraries( RantsImageRead ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageRead DESTINATION . )

add_library( RantsImageWrite MODULE ${ANTsR_SOURCE_DIR}/src/antsImageWrite.cxx )
target_link_libraries( RantsImageWrite ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageWrite DESTINATION . )

add_library( RantsImage MODULE ${ANTsR_SOURCE_DIR}/src/antsImage.cxx )
target_link_libraries( RantsImage ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImage DESTINATION . )

add_library( RantsImageClone MODULE ${ANTsR_SOURCE_DIR}/src/antsImageClone.cxx )
target_link_libraries( RantsImageClone ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageClone DESTINATION . )

#add_library( RantsBinaryThresholdImageFilter MODULE ${ANTsR_SOURCE_DIR}/src/antsBinaryThresholdImageFilter.cxx )
#target_link_libraries( RantsBinaryThresholdImageFilter ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
#install( TARGETS RantsBinaryThresholdImageFilter DESTINATION . )

#add_library( RantsThresholdImageFilter MODULE ${ANTsR_SOURCE_DIR}/src/antsThresholdImageFilter.cxx )
#target_link_libraries( RantsThresholdImageFilter ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
#install( TARGETS RantsThresholdImageFilter DESTINATION . )

#add_library( RantsPermuteAxesImageFilter MODULE ${ANTsR_SOURCE_DIR}/src/antsPermuteAxesImageFilter.cxx )
#target_link_libraries( RantsPermuteAxesImageFilter ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
#install( TARGETS RantsPermuteAxesImageFilter DESTINATION . )

add_library( RantsImageList MODULE ${ANTsR_SOURCE_DIR}/src/antsImageList.cxx )
target_link_libraries( RantsImageList ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsImageList DESTINATION . )

add_library( RantsMatrix MODULE ${ANTsR_SOURCE_DIR}/src/antsMatrix.cxx )
target_link_libraries( RantsMatrix ${RCPP_LIB_DIR} ${R_LIB_DIR} ${ITK_LIBRARIES} )
install( TARGETS RantsMatrix DESTINATION . )
