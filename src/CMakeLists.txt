cmake_minimum_required(VERSION 2.8.7)
project(ANTsR_SuperBuild)
include( ExternalProject )
# if 'CMAKE_BUILD_TYPE' is not specified by user, assume RelWithDebInfo
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE RelWithDebInfo )
endif( NOT CMAKE_BUILD_TYPE )

#   we have to define NDEBUG due to a possible bug in itkMacro.h --- we
#   require   return static_cast<TTarget>(x); to be implemented rather 
#   than the dynamic_cast
ExternalProject_Add( ANTS
   TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/tmp 
    STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/stamp
    DOWNLOAD_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    GIT_REPOSITORY https://github.com/stnava/ANTs.git
    GIT_TAG 17508fadff899e589c07c90c4f7c42bd5c2d1cdb
    SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    CMAKE_ARGS -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} # build type set by user
               -D COPY_SCRIPT_FILES_TO_BIN_DIR=ON # for useful things like buildtemplateparallel
               -D BUILD_SHARED_LIBS=OFF # R requires shared objects
	       -D BUILD_TESTING=OFF # reduces build time
	       -D USE_SYSTEM_ITK=OFF # use the ITK built above using superbuild or use the one specified by user through 'ITK_DIR'
    cmakedefine NDEBUG 
    BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build
    INSTALL_COMMAND "" )
set( ANTS_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build )
set( ITK_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build/ITKv4-build )
set( ANTsR_DEPENDSON_ANTS ANTS ) # set when ANTS is superbuilt so that ANTS is superbuilt prior to ANTsR
set( ANTS_INC ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src/Examples/include )


message("CMAKE_CXX_FLAGS_RELEASE is ${CMAKE_CXX_FLAGS_RELEASE}")
message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")
