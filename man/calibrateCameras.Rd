\name{calibrateCameras}
\alias{calibrateCameras}
\alias{print.calibrateCameras}
\title{Finds the optimized DLT coefficients for a stereo camera setup}
\description{
This function uses a checkerboard at different positions and angles within a stereo camera setup to estimate DLT calibration coefficients for use in stereo reconstruction. This function is a wrapper for the function \code{dltCalibrateCameras}.
}
\usage{
calibrateCameras(img.dir, sq.size, nx, ny, cal.file, corner.dir,
                 print.progress = TRUE, flip.view = FALSE, verify.dir = NULL,
                 min.views = 'max', exec.dir = '', undistort = FALSE, 
                 num.aspects.read = 'auto', num.sample.est = 'auto', 
                 num.sample.sets = 'auto', num.aspects.sample = 'auto', 
                 max.sample.optim = 30, nlm.calls.max = 20, fit.min.break = 1,
                 objective.min = 1, objective.min.break = 5, with.circles = FALSE, 
                 sample.est = NULL, ...)
}
\arguments{
  \item{img.dir}{folder containing images of the checkerboard pattern, separated into separate folders by view.}
  \item{sq.size}{character string indicating size of checkerboard squares (length along one dimension) including the unit of measure (e.g. '6.35 mm').}
  \item{nx}{integer indicating the number of internal corners along one dimension of the checkerboard.}
  \item{ny}{integer indicating the number of internal corners along the other dimension of the checkerboard.}
  \item{cal.file}{file path to the output calibration file (if it does not already exist one will be created).}
  \item{corner.dir}{folder where the detected checkerboard corners will be saved (if it does not already exist one will be created).}
  \item{print.progress}{logical indicating whether function processes should be printed to the console.}
  \item{flip.view}{logical indicating whether one camera view is upside-down relative to the other.}
  \item{verify.dir}{folder where the images of the checkerboards with detected corners overlaid will be written (if it does not already exist one will be created).}
  \item{min.views}{integer indicating the minimum views in which corners must be detected in order to use in coefficient estimation. If set to 'max' (default) this will be equal to the number of input views.}
  \item{exec.dir}{file path to folder containing external executables for reading video files (still under development).}
  \item{undistort}{logical indicating whether to estimate lens distortion correction coefficients (still under development).}
  \item{num.aspects.read}{number (integer) of frames to be read from video input. Requires external executables (still under development).}
  \item{num.sample.est}{number (integer) of aspects from total that will be sampled calibration coefficient estimation.}
  \item{num.sample.sets}{number (integer) of unique sets of aspects to try.}
  \item{num.aspects.sample}{number (integer) of aspects to sample for each set.}
  \item{max.sample.optim}{maximum number (integer) of aspects to be used in identifying the best DLT coefficient set (once already estimated).}
  \item{nlm.calls.max}{maximum number (integer) of different sets of random starting parameters to use during coefficient optimization. This parameter cannot exceed 576.}
  \item{fit.min.break}{minimum error at which \code{resampleGridImagePoints()} will stop iterating to find a better fit.}
  \item{objective.min.break}{minimum error at which optimization will stop estimating the position of additional checkerboards.}
  \item{objective.min}{The expected mean reconstruction error when optimizing the calibration coefficients (the minimum, or \code{objective} value returned by \code{nlminb()}). A value between 0.2 and 1.2 should be reasonable.}
  \item{with.circles}{logical indicating whether the checkerboard pattern includes concentric circles for identifying the starting corner. Requires external executables (still under development).}
  \item{sample.est}{vector of explicitly defined aspects to use in coefficient estimation.}
  \item{...}{further arguments to be passed to \code{\link{dltCalibrateCameras}}.}
}
\value{
a list of class \code{"calibrateCameras"} with the following elements:
  \item{cal.coeff}{a matrix of 11 optimized DLT calibration coefficients per camera view.}
  \item{mean.reconstruct.rmse}{the RMS error when \code{coor.2d} and the optimized calibration coefficients \code{cal.coeff} are input to \code{\link{dltReconstruct}}.}
  \item{coefficient.rmse}{the RMS error when \code{coor.2d} and the optimized 3D coordinates \code{coor.3d} are input to \code{\link{dltCoefficients}}.}
}
\details{
Please see \href{https://aaronolsen.github.io/software/stereomorph.html}{StereoMorph tutorials} for step-by-step tutorials on how to use \strong{StereoMorph} for 2D or 3D shape data collection.
}
\references{
For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\seealso{
\code{\link{digitizeImages}},
\code{\link{reconstructStereoSets}}
}
\keyword{ DLT }
\keyword{ calibration }