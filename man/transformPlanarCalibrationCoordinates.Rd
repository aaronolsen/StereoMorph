\name{transformPlanarCalibrationCoordinates}
\alias{transformPlanarCalibrationCoordinates}
\title{Performs rotational and translational transformations to a planar grid}
\description{
This function rotates and translates a planar grid or grids according to specified transformation parameters. This function is called by \code{\link{dltCalibrateCameras}}, to find the optimal transformation parameters for a set of arbitrarily oriented grid points that minimizes DLT calibration error. This function is also called by \code{\link{dltTestCalibration}} to generate an ideal grid for accuracy testing.
}
\usage{
transformPlanarCalibrationCoordinates(tpar, nx, ny, sx, sy = NULL)
}
\arguments{
  \item{tpar}{a vector of six transformation parameters per grid. The first three being rotational parameters (rotation about the z, y and x axes, respectively) and the second three being translational parameters (translation along the x, y and z axes, respectively). For more than one grid, these six values are concatenated as a vector.}
  \item{nx}{the number of points along the first dimension (e.g. this would be the number of points in each row if points are listed first by row).}
  \item{ny}{the number of points along the second dimension (e.g. this would be the number of points in each column if points are listed first by row).}
  \item{sx}{a scaling factor along the first dimension.}
  \item{sy}{a scaling factor along the second dimension. If the grid blocks are squares, this can be left as \code{NULL} and only \code{sx} will be used.}
}
\value{a matrix of transformed 3D grid coordinates}
\author{Aaron Olsen}
\seealso{
\code{\link{dltCalibrateCameras}},
\code{\link{dltTransformationParameterRMSError}},
\code{\link{dltTestCalibration}}
}
\keyword{ grid functions }