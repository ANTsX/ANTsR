cmake_minimum_required(VERSION 2.8)

project(ANTsR_SuperBuild)
include( ExternalProject )

# if CMAKE_BUILD_TYPE is not specified, assume 'Release'
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE "Release" CACHE STRING "Project build-type" )
endif( NOT CMAKE_BUILD_TYPE )

if( NOT ANTS_DIR )
  ExternalProject_Add( ANTS
    TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/tmp 
    STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/stamp
    DOWNLOAD_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    SVN_REPOSITORY https://advants.svn.sourceforge.net/svnroot/advants/branches/image_passing
    SVN_REVISION -r 1283
    SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    CMAKE_ARGS -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build
    INSTALL_COMMAND "" ) # have to include a blank install command or it will try to install into standard dirs
  set( ANTS_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build )
  set( ANTsR_DEPENDSON_ANTS ANTS ) # set when we need to download ANTS; not set otherwise
endif( NOT ANTS_DIR )

if( NOT ITK_DIR )
  set( ITK_DIR ${ANTS_DIR}/ITKv4-build )
endif( NOT ITK_DIR )

ExternalProject_Add( ANTsR 
  DEPENDS ${ANTsR_DEPENDSON_ANTS}
  TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/tmp
  STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/stamp
  SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-src
  CMAKE_ARGS -D ANTS_DIR=${ANTS_DIR} -D ITK_DIR=${ITK_DIR} -D RCPP_INCLUDE_DIR=${RCPP_INCLUDE_DIR} -D RCPP_LIB_DIR=${RCPP_LIB_DIR}
             -D R_INCLUDE_DIR=${R_INCLUDE_DIR} -D R_LIB_DIR=${R_LIB_DIR} -D CMAKE_INSTALL_PREFIX=${ANTsR_SuperBuild_SOURCE_DIR}
	     -D CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
  BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-build 
  INSTALL_DIR "" ) 
