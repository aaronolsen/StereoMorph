\name{imagePlaneGridTransformError}
\alias{imagePlaneGridTransformError}
\title{Returns imagePlaneGridTransform error}
\description{
Returns the mean error between a matrix of grid points and a matrix of transformed grid points (produced by \code{\link{imagePlaneGridTransform}}). This function is called internally by the function \code{\link{resampleGridImagePoints}} in evaluating the goodness of fit between imaged grid points and grid points produced by an image perspective model.
}
\usage{
imagePlaneGridTransformError(p, nx, ny, grid)
}
\arguments{
  \item{p}{a vector of 12 grid parameters (see \code{\link{imagePlaneGridTransform}}).}
  \item{nx}{the number of points along the first dimension.}
  \item{ny}{the number of points along the second dimension.}
  \item{grid}{a matrix of grid points to be compared against the model grid points.}
}
\value{the mean error.}
\author{Aaron Olsen}
\seealso{
\code{\link{imagePlaneGridTransform}}, \code{\link{resampleGridImagePoints}}
}