\name{dltCoefficientRMSError}
\alias{dltCoefficientRMSError}
\title{Returns the error during calibration coefficient optimization}
\description{
Returns the RMS error from \code{\link{dltReconstruct}} when optimizing the calibration coefficients. This function is called internally by \code{\link{dltCalibrateCameras}}.
}
\usage{
dltCoefficientRMSError(p, coor.2d)
}
\arguments{
  \item{p}{a vector of the current, 11-parameter calibration coefficients.}
  \item{coor.2d}{a four-dimensional array of grid points passed from \code{\link{dltCalibrateCameras}}.}
}
\value{the mean RMS error from \code{\link{dltReconstruct}} across all views.}
\author{Aaron Olsen}
\seealso{
\code{\link{transformPlanarCalibrationCoordinates}},
\code{\link{dltReconstruct}},
\code{\link{dltCalibrateCameras}}
}
\keyword{ DLT }