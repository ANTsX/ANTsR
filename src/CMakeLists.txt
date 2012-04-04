cmake_minimum_required(VERSION 2.8)

project(ANTsR_SuperBuild)
include( ExternalProject )

if( NOT ANTS_DIR )
  ExternalProject_Add( ANTS
    TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/tmp 
    STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/stamp
    DOWNLOAD_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    GIT_REPOSITORY git://advants.git.sourceforge.net/gitroot/advants/advants
    GIT_TAG 9bbc57e5d8caf52b68b02f28acf46fb45b15e5b1
    SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-src
    CMAKE_ARGS -D CMAKE_BUILD_TYPE=Release
    BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build
    INSTALL_COMMAND "" ) # have to include a blank install command or it will try to install into standard dirs
  set( ANTS_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTS/ANTS-build )
  set( ANTsR_DEPENDSON_ANTS ANTS ) # set when we need to download ANTS; not set otherwise
endif( NOT ANTS_DIR )


ExternalProject_Add( ANTsR 
  DEPENDS ${ANTsR_DEPENDSON_ANTS}
  TMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/tmp
  STAMP_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/stamp
  SOURCE_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-src
  CMAKE_ARGS -D ANTS_DIR=${ANTS_DIR} -D RCPP_INCLUDE_DIR=${RCPP_INCLUDE_DIR} -D RCPP_LIB_DIR=${RCPP_LIB_DIR}
             -D R_INCLUDE_DIR=${R_INCLUDE_DIR} -D R_LIB_DIR=${R_LIB_DIR} -D CMAKE_INSTALL_PREFIX=${ANTsR_SuperBuild_SOURCE_DIR}
  BINARY_DIR ${ANTsR_SuperBuild_SOURCE_DIR}/ANTsR/ANTsR-build 
  INSTALL_DIR "" ) # have to include a blank install command or it will try to install into standard dirs
