\name{StereoMorph-package}
\alias{StereoMorph-package}
\alias{StereoMorph}

\alias{alignShapesToMidline}
\alias{checkCornerOrder}
\alias{cprod_SM}
\alias{createErrorPlots}
\alias{dilateImage}
\alias{distortionError}
\alias{dltCCEstimateStartParams}
\alias{drawRectangle}
\alias{drawShapes}
\alias{equalizeImageHist}
\alias{erodeImage}
\alias{estimateDistortion}
\alias{estimateDLTCoefficients}
\alias{findBoundaryPoints}
\alias{findCornerSubPix}
\alias{findEpipolarTangencyAngles}
\alias{findHomography}
\alias{findInterpointDistanceError}
\alias{generateQuads}
\alias{gridPointsFitError}
\alias{intCornersFromQuads}
\alias{inverseGridTransform}
\alias{list2XML4R}
\alias{listToJSONStr}
\alias{matchCurvePoints}
\alias{meanBlurImage}
\alias{orderCorners}
\alias{print_processing_times}
\alias{process_digitize_images_input}
\alias{readLandmarksToArray}
\alias{readXML4R}
\alias{readXMLLines}
\alias{reflectMissingShapes}
\alias{removeOutlierCorners}
\alias{rgbToGray}
\alias{rotationMatrixZYX_SM}
\alias{undistort}
\alias{uvector_SM}
\alias{testCalibration}
\alias{thresholdImageMatrix}
\alias{tMatrixDC_SM}
\alias{XML4R2list}
\alias{XML4R2listLines}

\docType{package}
\title{Stereo Camera Morphometrics}
\description{
\strong{StereoMorph} provides functions for the collection of 3D points and curves using a stereo camera setup. \strong{StereoMorph} can also be used for \href{https://aaronolsen.github.io/software/digitizing.html}{collecting 2D shape data from photographs}. Please see \href{https://aaronolsen.github.io/software/stereomorph.html}{StereoMorph tutorials} for step-by-step tutorials on how to use \strong{StereoMorph} for 2D or 3D shape data collection.
}
\details{
\tabular{ll}{
Package: \tab StereoMorph\cr
Type: \tab Package\cr
Version: \tab 1.5.2\cr
Date: \tab 2016-06-04\cr
License: \tab GPL-2\cr
}
}
\author{
Aaron Olsen, Annat Haber

Maintainer: Aaron Olsen \email{aarolsen@gmail.com}
}
\keyword{ package }
\seealso{
\href{https://cran.r-project.org/package=svgViewR}{svgViewR}, 
\href{https://cran.r-project.org/package=bezier}{bezier}
}
