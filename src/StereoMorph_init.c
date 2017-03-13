#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP StereoMorph_dilateImage(SEXP, SEXP, SEXP);
extern SEXP StereoMorph_drawRectangle(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP StereoMorph_equalizeImageHist(SEXP);
extern SEXP StereoMorph_erodeImage(SEXP, SEXP, SEXP);
extern SEXP StereoMorph_findBoundaryPoints(SEXP);
extern SEXP StereoMorph_findCornerSubPix(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP StereoMorph_generateQuads(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP StereoMorph_intCornersFromQuads(SEXP, SEXP);
extern SEXP StereoMorph_meanBlurImage(SEXP, SEXP);
extern SEXP StereoMorph_orderCorners(SEXP, SEXP, SEXP);
extern SEXP StereoMorph_rgbToGray(SEXP, SEXP, SEXP);
extern SEXP StereoMorph_thresholdImageMatrix(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"StereoMorph_dilateImage",          (DL_FUNC) &StereoMorph_dilateImage,          3},
    {"StereoMorph_drawRectangle",        (DL_FUNC) &StereoMorph_drawRectangle,        5},
    {"StereoMorph_equalizeImageHist",    (DL_FUNC) &StereoMorph_equalizeImageHist,    1},
    {"StereoMorph_erodeImage",           (DL_FUNC) &StereoMorph_erodeImage,           3},
    {"StereoMorph_findBoundaryPoints",   (DL_FUNC) &StereoMorph_findBoundaryPoints,   1},
    {"StereoMorph_findCornerSubPix",     (DL_FUNC) &StereoMorph_findCornerSubPix,     5},
    {"StereoMorph_generateQuads",        (DL_FUNC) &StereoMorph_generateQuads,        9},
    {"StereoMorph_intCornersFromQuads",  (DL_FUNC) &StereoMorph_intCornersFromQuads,  2},
    {"StereoMorph_meanBlurImage",        (DL_FUNC) &StereoMorph_meanBlurImage,        2},
    {"StereoMorph_orderCorners",         (DL_FUNC) &StereoMorph_orderCorners,         3},
    {"StereoMorph_rgbToGray",            (DL_FUNC) &StereoMorph_rgbToGray,            3},
    {"StereoMorph_thresholdImageMatrix", (DL_FUNC) &StereoMorph_thresholdImageMatrix, 4},
    {NULL, NULL, 0}
};

void R_init_StereoMorph(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}