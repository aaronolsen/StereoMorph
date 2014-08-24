\name{rotationMatrixZYX}
\alias{rotationMatrixZYX}
\title{Returns a matrix to rotate points along the z-, y- and x-axes}
\description{
This function returns a rotation matrix that rotates a three-column point matrix about the z-, y- and x-axes, in that order. The three angles of rotation can be specified by a single vector of length three or three separate parameters.
}
\usage{
rotationMatrixZYX(t, t2 = NULL, t3 = NULL)
}
\arguments{
  \item{t}{angle (in radians) to rotate around the z-axis or a vector of three angles to rotate around the z-, y- and x-axes, in that order.}
  \item{t2}{if \code{t} is a single numeric, this is the angle (in radians) to rotate around the y-axis.}
  \item{t3}{if \code{t} is a single numeric, this is the angle (in radians) to rotate around the x-axis.}
}
\value{a 3x3 rotation matrix.}
\references{
\url{http://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions}
}
\author{Aaron Olsen}
\seealso{
\code{\link{transformPlanarCalibrationCoordinates}}
}
\examples{
## SPECIFY 3D POINT SET
m <- matrix(c(0,0,0, 1,2,1, 3,0,3, -2,4,1), nrow=4, ncol=3, byrow=TRUE)

## ROTATE 180 DEGREES ABOUT THE Z AXIS
## X AND Y VALUES ARE OPPOSITE AND Z VALUES UNCHANGED
m \%*\% rotationMatrixZYX(pi, 0, 0)

## ROTATE 180 DEGREES ABOUT THE X AXIS
## Y AND Z VALUES ARE OPPOSITE AND X VALUES UNCHANGED
m \%*\% rotationMatrixZYX(0, 0, pi)

## ROTATE 90 DEGREES ABOUT THE Z-, THEN Y-, THEN X-AXIS
m \%*\% rotationMatrixZYX(pi/2, pi/2, pi/2)

## ROTATE 90 DEGREES ABOUT THE Z-, THEN Y-, THEN X-AXIS
m \%*\% rotationMatrixZYX(c(pi/2, pi/2, pi/2))
}
\keyword{ rotation matrix }