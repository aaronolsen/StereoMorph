\name{reconstructStereoSets}
\alias{reconstructStereoSets}
\title{3D reconstruction of landmark and curves from stereo coordinates}
\description{
This function reconstructs and unifies landmarks and curves from multiple stereo sets. This function is a wrapper integrating \code{\link{dltReconstruct}}, \code{\link{dltMatchCurvePoints}}, and \code{\link{unifyLandmarks}}.
}
\usage{
reconstructStereoSets(shapes.2d, shapes.3d, cal.file, set.names = NULL, 
     min.common = 3, unify = TRUE, reconstruct.curves = TRUE, 
     even.spacing = NULL, print.progress = TRUE, verbose = FALSE, 
     update.only = FALSE, min.direct.tangency = 25, min.fill.tangency = 10, 
     epi.err.weight = 0, rec.err.weight = 1, curves.as.landmarks = FALSE, 
     curve.name.width = 5)
}
\arguments{
  \item{shapes.2d}{file path to a folder containing 2D (digitized) shape files, separated by view into different folders.}
  \item{shapes.3d}{file path to a folder where the 3D shape files will be saved (if it does not already exist one will be created).}
  \item{cal.file}{file path to calibration file created by \code{\link{calibrateCameras}}.}
  \item{set.names}{vector of object or specimen names to be processed by the function. If \code{NULL} (default) all the files in \code{shapes.2d} will be processed.}
  \item{min.common}{integer indicating the minimum number of common points required for unification of landmark sets.}
  \item{unify}{logical indicating whether to unify different aspects of the same object or specimen. If sets are to be unified, the filenames should end in '_a#' (e.g. '_a1', '_a2', etc.) to indicate different aspects of the same object.}
  \item{reconstruct.curves}{logical indicating whether to reconstruct curves.}
  \item{even.spacing}{specifies the number of evenly spaced points to be on each curve. This can be an integer (if the number of points for all curve(s) is the same), a list (in which the names of the list elements correspond to the curve names), or a .txt file containing a two column matrix of curve names and the number of points on each curve, separated by tabs and without quotes.}
  \item{print.progress}{logical indicating whether function processes should be printed to the console.}
  \item{verbose}{logical indicating whether \code{print.progress} should be detailed.}
  \item{update.only}{logical indicating whether function should only reconstruct sets for which the 2D data has been modified. If \code{TRUE} the function will not process all files in \code{shapes.2d}, only those which have been modified since the last function call.}
  \item{min.direct.tangency}{input parameter passed to \code{\link{dltMatchCurvePoints}} (see that function's documentation for details).}
  \item{min.fill.tangency}{input parameter passed to \code{\link{dltMatchCurvePoints}} (see that function's documentation for details).}
  \item{epi.err.weight}{input parameter passed to \code{\link{dltMatchCurvePoints}} (see that function's documentation for details).}
  \item{rec.err.weight}{input parameter passed to \code{\link{dltMatchCurvePoints}} (see that function's documentation for details).}
  \item{curves.as.landmarks}{logical indicating whether curve points should be saved as landmarks (will be added to any existing landmarks).}
  \item{curve.name.width}{integer indicating the width of numbers added to the curve name in generating curve-to-landmark names. If \code{curves.as.landmarks} is \code{TRUE}, curve landmarks will be created by adding numbers to the end of the curve name. For example, a \code{curve.name.width} value of 5 would be 'curve_name00001'.}
}
\value{
NULL
}
\details{
Please see \href{https://aaronolsen.github.io/software/stereomorph.html}{StereoMorph tutorials} for step-by-step tutorials on how to use \strong{StereoMorph} for 2D or 3D shape data collection.
}
\author{Aaron Olsen}
\seealso{
\code{\link{calibrateCameras}},
\code{\link{digitizeImages}}
}
\keyword{ DLT }