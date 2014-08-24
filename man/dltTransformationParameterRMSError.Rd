\name{dltTransformationParameterRMSError}
\alias{dltTransformationParameterRMSError}
\title{Returns the error during transformation parameter optimization}
\description{
Returns the RMS error from \code{\link{dltCoefficients}} after applying a given set of transformation parameters to grid points in stereo camera calibration. This function is called internally by the function \code{\link{dltCalibrateCameras}} to estimate the position and orientation of a set of calibration grid points that minimizes calibration error.
}
\usage{
dltTransformationParameterRMSError(p, coor.2d, nx, ny, sx, sy = NULL, 
                                   p.fixed = NULL)
}
\arguments{
  \item{p}{a vector of six transformation parameters per grid. The first three being rotational parameters (rotation about the z, y and x axes, respectively) and the second three being translational parameters (translation along the x, y and z axes, respectively). For more than one grid, these six values are concatenated as a vector.}
  \item{coor.2d}{a four-dimensional array of grid points passed from \code{\link{dltCalibrateCameras}}.}
  \item{nx}{the number of points along the first dimension (e.g. this would be the number of points in each row if points are listed first by row).}
  \item{ny}{the number of points along the second dimension (e.g. this would be the number of points in each column if points are listed first by row).}
  \item{sx}{a scaling factor along the first dimension.}
  \item{sy}{a scaling factor along the second dimension. If the grid blocks are squares, this can be left as \code{NULL} and only \code{sx} will be used.}
  \item{p.fixed}{a set of transformation parameters to be appended to the beginning of \code{p} that will are fixed (constant) during the optimization step.}
}
\value{the mean RMS error from \code{\link{dltCoefficients}} across all views.}
\author{Aaron Olsen}
\seealso{
\code{\link{transformPlanarCalibrationCoordinates}},
\code{\link{dltCoefficients}},
\code{\link{dltCalibrateCameras}}
}
\keyword{ DLT }