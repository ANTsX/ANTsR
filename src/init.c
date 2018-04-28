#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
   this is generated from running within ANTsR dir
   tools::package_native_routine_registration_skeleton(".")
*/

/* .Call calls */
extern SEXP antsAffineInitializer(SEXP);
extern SEXP antsJointFusion(SEXP);
extern SEXP antsMotionCorr(SEXP);
extern SEXP antsMotionCorrStats(SEXP, SEXP, SEXP, SEXP);
extern SEXP centerOfMass(SEXP);
extern SEXP createJacobianDeterminantImageR(SEXP, SEXP, SEXP, SEXP);
extern SEXP eigenanatomyCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fastMarchingExtension(SEXP, SEXP, SEXP);
extern SEXP fsl2antsrTransform(SEXP, SEXP, SEXP, SEXP);
extern SEXP histogramMatchImageR(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imagesToMatrix(SEXP, SEXP, SEXP);
extern SEXP invariantImageSimilarity(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP itkConvolveImage(SEXP, SEXP);
extern SEXP KellyKapowski(SEXP);
extern SEXP LabelGeometryMeasures(SEXP);
extern SEXP reflectionMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP reorientImage(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP robustMatrixTransform(SEXP);
extern SEXP sccanCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sccanX(SEXP);
extern SEXP timeSeriesSubtraction(SEXP, SEXP);
extern SEXP weingartenImageCurvature(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"antsAffineInitializer",          (DL_FUNC) &antsAffineInitializer,           1},
    {"antsJointFusion",                (DL_FUNC) &antsJointFusion,                 1},
    {"antsMotionCorr",                 (DL_FUNC) &antsMotionCorr,                  1},
    {"antsMotionCorrStats",            (DL_FUNC) &antsMotionCorrStats,             4},
    {"centerOfMass",                   (DL_FUNC) &centerOfMass,                    1},
    {"createJacobianDeterminantImageR",(DL_FUNC) &createJacobianDeterminantImageR, 4},
    {"eigenanatomyCpp",                (DL_FUNC) &eigenanatomyCpp,                15},
    {"fastMarchingExtension",          (DL_FUNC) &fastMarchingExtension,           3},
    {"fsl2antsrTransform",             (DL_FUNC) &fsl2antsrTransform,              4},
    {"histogramMatchImageR",           (DL_FUNC) &histogramMatchImageR,            5},
    {"imagesToMatrix",                 (DL_FUNC) &imagesToMatrix,                  3},
    {"invariantImageSimilarity",       (DL_FUNC) &invariantImageSimilarity,       12},
    {"itkConvolveImage",               (DL_FUNC) &itkConvolveImage,                2},
    {"KellyKapowski",                  (DL_FUNC) &KellyKapowski,                   1},
    {"LabelGeometryMeasures",          (DL_FUNC) &LabelGeometryMeasures,           1},
    {"reflectionMatrix",               (DL_FUNC) &reflectionMatrix,                4},
    {"reorientImage",                  (DL_FUNC) &reorientImage,                   6},
    {"robustMatrixTransform",          (DL_FUNC) &robustMatrixTransform,           1},
    {"sccanCpp",                       (DL_FUNC) &sccanCpp,                       19},
    {"sccanX",                         (DL_FUNC) &sccanX,                          1},
    {"timeSeriesSubtraction",          (DL_FUNC) &timeSeriesSubtraction,           2},
    {"weingartenImageCurvature",       (DL_FUNC) &weingartenImageCurvature,        3},
    {NULL, NULL, 0}
};

void R_init_ANTsR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
