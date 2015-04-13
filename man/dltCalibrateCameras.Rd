\name{dltCalibrateCameras}
\alias{dltCalibrateCameras}
\alias{summary.dltCalibrateCameras}
\alias{print.summary.dltCalibrateCameras}
\title{Finds the optimized DLT coefficients for a stereo camera setup}
\description{
This function uses the corners from a grid positioned in several different orientations within a stereo camera setup to estimate the DLT calibration coefficients that minimize reconstruction error.
}
\usage{
dltCalibrateCameras(coor.2d, nx, grid.size, c.run = FALSE, reduce.grid.dim = 3,
                    fit.min.break = 1, nlm.iter.max.init = 100, objective.min.init = 10, 
                    nlm.eval.max = 350, nlm.iter.max = 250, nlm.calls.max = 100,
                    objective.min = 1, grid.incl.min=2, start.param=NULL, 
                    print.progress = FALSE)

\method{summary}{dltCalibrateCameras}(object, ...)
}
\arguments{
  \item{coor.2d}{a four-dimensional array of grid points. The first two dimensions correspond to each matrix of grid points, the third corresponds to each grid position/orientation and the fourth corresponds to each camera view. Can be read from file by the function \code{\link{readCheckerboardsToArray}}.}
  \item{nx}{the number of points along the first dimension (e.g. this would be the number of points in each row if points in \code{coor.2d} are listed first by row). The number of points along the second dimension is calculated based on the total number of points per view and orientation.}
  \item{grid.size}{the size of the grid squares in real-world units (e.g. millimeters).}
  \item{c.run}{a logical indicating whether a second optimization should be performed on the calibration coefficients.}
  \item{reduce.grid.dim}{a numeric indicating the number of grid points along each dimension for each grid after resampling. The total number of resampled points is \code{reduce.grid.dim^2}. Resampling can be turned off by setting this to \code{0} or \code{FALSE}. The default is recommended. \code{reduce.grid.dim} must be greater than two.}
  \item{fit.min.break}{passed to \code{resampleGridImagePoints()}. A minimum returned by \code{nlminb()} (indicating goodness of fit in pixel coordinates) at which \code{resampleGridImagePoints()} will stop iterating to find a better fit for each checkerboard grid. Ignored if \code{reduce.grid.dim} is \code{0} or \code{FALSE}.}
  \item{nlm.iter.max.init}{The maximum number of iterations to be performed by \code{nlminb()} during initial coefficient optimization, passed as a control parameter to \code{nlminb()}. These are the number of iterations for an initial determination of whether the function is likely to converge on the correct estimate.}
  \item{objective.min.init}{The objective used during the initial coefficient optimization, passed as a control parameter to \code{nlminb()}, to determine whether the function is close to convergence.}
  \item{nlm.eval.max}{The maximum number of evaluations to be performed by \code{nlminb()} during primary coefficient optimization, passed as a control parameter to \code{nlminb()}. Keeping this value as low as possible without excluding actual convergence speeds performance of the function by preventing the function from stalling far from the optimal values.}
  \item{nlm.iter.max}{The maximum number of iterations to be performed by \code{nlminb()} during primary coefficient optimization, passed as a control parameter to \code{nlminb()}. Keeping this value as low as possible without excluding actual convergence speeds performance of the function by preventing the function from stalling far from the optimal values.}
  \item{nlm.calls.max}{The maximum number of different sets of random starting parameters to use during coefficient optimization. This parameter cannot exceed 576.}
 \item{objective.min}{The expected mean reconstruction error when optimizing the calibration coefficients (the minimum, or \code{objective} value returned by \code{nlminb()}). A value between 0.7 and 3 should be reasonable.}
  \item{grid.incl.min}{The minimum number of grids to include during coefficient optimization.}
  \item{start.param}{An set of fixed starting parameters to be used during coefficient optimization. This parameter is intended primarily for debugging.}
  \item{print.progress}{a logical indicating whether the progress of the function should be printed while running. This includes the error in grid re-sampling, an iteration count during optimization and other outputs relating to the optimization.}
  \item{object}{a list of class \code{"dltCalibrateCameras"} (the output of \code{dltCalibrateCameras()}).}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
Calibration is the most challenging step in stereo camera data collection. Most fundamentally, DLT calibration requires a set of 3D coordinates and their corresponding 2D pixel coordinates in each camera view in order to derive calibration coefficients (see \code{\link{dltCoefficients}}). These coefficients can then be used to reconstruct any point in 3D given its 2D pixel coordinates in two or more camera views. DLT calibration has traditionally been done using a "calibration object", typically a 3D box-shaped structure filled with markers at known 3D positions. Such objects require the use of high precision machining in order to achieve an accurate calibration and the calibration points are usually digitized manually.

The \code{dltCalibrateCameras()} function provides a camera calibration routine that is easier to implement and potentially more accurate. This function uses the corners from a grid positioned in several different orientations within the calibration space to estimate the DLT calibration coefficients that minimize reconstruction error. The easiest method for obtaining these corner points is to print a checkerboard pattern (using \code{\link{drawCheckerboard}}), attach the pattern to flat, hard surface and use \code{\link{findCheckerboardCorners}} to automatically extract the pixel coordinates of the internal corners. For a step-by-step tutorial on how to create a checkerboard pattern, see \href{http://stereomorph.blogspot.com/2014/04/creating-checkerboard-pattern.html}{Creating a checkerboard pattern}.

The grid pattern should be photographed in at least four different positions and orientations spanning the volume to be calibrated (the tutorial files loaded with StereoMorph include eight different positions). Using only a couple of positions will result in uneven sampling of the calibration volume causing larger errors in some regions relative to others. Additionally, using only a single orientation of the checkerboard will produce higher errors along a particular dimension relative to the others. Once the pixel coordinates of the grid points (e.g. the internal corners of the checkerboard pattern) have been extracted from all of the calibration images, they should be read into an array using \code{\link{readCheckerboardsToArray}}. This function allows for the point order to be reversed along rows, columns or both. If one of the cameras views the pattern upside down relative to another camera or if the pattern is in a different orientation, the grid points may be extracted in a different order.  This can be fixed using the \code{row.reverse} and \code{col.reverse} arguments in \code{\link{readCheckerboardsToArray}}. It is \strong{essential} that the grid points extracted from each camera view correspond to each other row-by-row or else the calibration will not work.

\code{dltCalibrateCameras()} first calls \code{\link{resampleGridImagePoints}} to downsample the number of grid points. \code{reduce.grid.dim} is the downsample number (the default is 3, meaning 3x3 or nine points per grid). Downsampling can be turned off by setting \code{reduce.grid.dim} to \code{0}, although this is not recommended as it will increase run-time substantially without increasing accuracy. A camera perspective model is fit to the full point set such that the number of points input to the coefficient optimization can be reduced (thereby reducing run-time) without losing any relevant information (see \code{\link{resampleGridImagePoints}}). If \code{print.progress} is set to \code{TRUE}, the mean and maximum fit error is printed for each input grid. As the fitting does not take into account lens distortion, high fit errors may indicate large distortional effects.

Since each checkerboard grid has been photographed in an arbitrary position and orientation, the 3D coordinates of the grid points are unknown. However, if the first grid is fixed, each additional grid can be described by applying six transformation parameters relative to the first (three translational and three rotational). Using the reduced grid point set, \code{dltCalibrateCameras()} uses \code{nlminb()} to search for the six transformation parameters per grid that minimizes the RMS error when the 3D coordinates are input (with the corresponding 2D coordinates) to \code{\link{dltCoefficients}}. In effect, \code{dltCalibrateCameras()} solves for the position of each grid in 3D space using the error from \code{\link{dltCoefficients}} as an optimality criterion. Since the first grid is fixed, the optimization will search for 6*(n-1) parameters, where n is the number of separate grid orientations. \code{nlminb()} calls the function \code{\link{dltTransformationParameterRMSError}}.

In order to fully explore the parameter space, \code{dltCalibrateCameras()} calls \code{nlminb()} several times with a different set of randomly generated starting parameters to estimate the transformation parameters for each additional grid. The number of different sets of starting parameters is determined by \code{nlm.calls.max}. An initial optimization run is intended to quickly determine whether a particular set of starting parameters is likely to lead to convergence. The number of iterations for this initial optimization is determined by \code{nlm.iter.max.init} and the objective used in determining likely convergence is \code{objective.min.init}. If it is determined that the starting parameters are likely to lead to convergence below \code{objective.min}, \code{nlminb()} is allowed to continue optimizing. For each grid, the solution yielding the lowest error, or the first solution below the \code{objective.min} threshold, is retained for the next grid optimization.

These optimal transformation parameters are then used to obtain the 3D coordinates of the original grid points (not downsampled). Once these 3D coordinates are known, the 3D and 2D pixel coordinates are input to \code{\link{dltCoefficients}} to obtain the 11 calibration coefficients per camera. For this reason, the calibration coefficient RMS Error (\code{coefficient.rmse}) returned will differ slightly from the reported final \code{nlminb()} minimum (\code{t.min}).

If \code{c.run} is set to \code{TRUE}, \code{dltCalibrateCameras()} performs a second optimization on the calibration coefficients themselves. \code{nlminb()} is used, this time calling \code{\link{dltCoefficientRMSError}}, to find the 11 calibration coefficients per view that minimizes the reconstruction RMS error. Note that \code{\link{dltCoefficients}} cannot be used as with the previous optimization because the coefficients must be an input. Running this second optimization seems to have little effect in increasing the accuracy of the calibration but is included as this may be useful for some stereo setups.

For a step-by-step tutorial on how to use \code{dltCalibrateCameras()} see \href{http://stereomorph.blogspot.com/2014/04/calibrating-stereo-cameras.html}{Calibrating stereo cameras}.
}
\value{
a list of class \code{"dltCalibrateCameras"} with the following elements:
  \item{cal.coeff}{a matrix of 11 optimized DLT calibration coefficients per camera view.}
  \item{coor.3d}{the optimized 3D coordinates of the input grid points in \code{coor.2d}.}
  \item{mean.reconstruct.rmse}{the RMS error when \code{coor.2d} and the optimized calibration coefficients \code{cal.coeff} are input to \code{\link{dltReconstruct}}.}
  \item{coefficient.rmse}{the RMS error when \code{coor.2d} and the optimized 3D coordinates \code{coor.3d} are input to \code{\link{dltCoefficients}}.}
  \item{t.param.final}{the final transformation parameters reported by \code{nlminb()} from the first optimization. \code{'t.'} refers to the transformation optimization.}
  \item{t.min}{the minimum reported by \code{nlminb()} from the first optimization. This is the mean RMS error across all camera views returned by \code{\link{dltCoefficients}} for the downsampled grid points.}
  \item{t.runtime}{the run-time (in seconds) for the first optimization.}
if \code{c.run} is \code{FALSE}, the following are \code{NA}. Otherwise,
  \item{c.param.init}{the initial parameters for the second optimization. \code{'c.'} refers to the coefficient optimization.}
  \item{c.param.final}{the final parameters reported by \code{nlminb()} from the second optimization.}
  \item{c.min}{the minimum reported by \code{nlminb()} from the second optimization. This is the mean RMS error across all camera views returned by \code{\link{dltReconstruct}}.}
  \item{c.iter}{the number of iterations reported by \code{nlminb()} from the second optimization.}
  \item{c.runtime}{the run-time (in seconds) for the second optimization.}
}
\references{
For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\seealso{
\code{\link{dltTestCalibration}},
\code{\link{dltCoefficients}},
\code{\link{readCheckerboardsToArray}},

\code{\link{transformPlanarCalibrationCoordinates}},
\code{\link{dltTransformationParameterRMSError}},

\code{\link{dltCoefficientRMSError}}
}
\examples{
## SET NUMBER OF INTERNAL CORNERS FOR CALIBRATION GRIDS
nx <- 21
ny <- 14

## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CHECKERBOARD CORNERS FROM CALIBRATION IMAGE SET
## THE TUTORIAL INCLUDES 8 CALIBRATION IMAGES FROM TWO CAMERA VIEWS
file <- matrix(c(paste0(fdir, "cal_a", 1:8, "_v1.txt"), 
  paste0(fdir, "cal_a", 1:8, "_v2.txt")), ncol=2)

## READ IN CHECKERBOARD CORNERS
## NOTE THAT col.reverse IS USED TO MAKE POINTS CORRESPOND
coor.2d <- readCheckerboardsToArray(file=file, nx=nx, ny=ny, col.reverse=TRUE)

## SET GRID SIZE (IN MM)
grid.size <- 6.347889

\dontrun{
## CALIBRATE CAMERAS
## TO REDUCE RUN-TIME, WE JUST USE CORNERS FROM TWO IMAGES (1 AND 5)
dlt_calibrate_cameras <- dltCalibrateCameras(coor.2d=coor.2d[, , c(1, 5), ], nx=nx, 
  grid.size=grid.size, c.run=FALSE, print.progress=TRUE)

## RUN CALIBRATION ON ALL IMAGES, ACCURACY IS GREATLY IMPROVED
dlt_calibrate_cameras <- dltCalibrateCameras(coor.2d=coor.2d, nx=nx, 
  grid.size=grid.size, c.run=FALSE, print.progress=TRUE)

## PRINT SUMMARY
summary(dlt_calibrate_cameras)
}
}
\keyword{ DLT }
\keyword{ calibration }