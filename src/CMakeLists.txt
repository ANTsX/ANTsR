cmake_minimum_required(VERSION 2.8)

project(Ritk)

# find and itk library; create shared object that interacts with it and link against it
find_package(ITK REQUIRED)
include( ${ITK_USE_FILE} )

# headers supplied with the package
include_directories( ${Ritk_SOURCE_DIR}/include )

# extract the paths from the variables supplied for rcpp and r include and lib
string( REGEX REPLACE "^ *-I *(.*) *" "\\1" RCPP_INCLUDE_DIR ${RCPP_INCLUDE_DIR} )
string( REGEX REPLACE "^ *-L *(.*) *" "\\1" RCPP_LIB_DIR ${RCPP_LIB_DIR} )
string( REGEX REPLACE "^ *-I *(.*) *" "\\1" R_INCLUDE_DIR ${R_INCLUDE_DIR} )
string( REGEX REPLACE "^ *-L *(.*) *" "\\1" R_LIB_DIR ${R_LIB_DIR} )

# R and Rcpp include and link directories
include_directories( ${RCPP_INCLUDE_DIR} ${R_INCLUDE_DIR} )
link_directories( ${RCPP_LIB_DIR} ${R_LIB_DIR} )

add_library( Ritk SHARED ${Ritk_SOURCE_DIR}/src/antsRegistration.cxx ${Ritk_SOURCE_DIR}/src/antsCommandLineParser.cxx ${Ritk_SOURCE_DIR}/src/antsCommandLineOption.cxx )
target_link_libraries( Ritk Rcpp R ${ITK_LIBRARIES} )
set_target_properties( Ritk PROPERTIES INSTALL_RPATH_USE_PATH TRUE PREFIX "" )
