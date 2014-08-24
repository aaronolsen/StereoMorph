\name{distanceGridUnits}
\alias{distanceGridUnits}
\title{Returns the distances between pairs of points on a square grid}
\description{This function returns the distances in grid units between pairs of points on a square grid. This function is used in testing the accuracy of a calibration by comparing theoretical distances among grid points to measured distances.}
\usage{distanceGridUnits(pairs, nx)}
\arguments{
  \item{pairs}{a two-column matrix specifying the pairs of points between which the distance is to be found.}
  \item{nx}{the number of points in the first dimension along which grid points are counted (see "Details").}
}
\details{
This function returns the distances in grid units between pairs of points on a square grid. Thus, adjoining points in the same row or column would be separated by a distance of one. The returned distances can then be multiplied by the grid square size to obtain the distances between pairs of points in real-world units (e.g. mm).

The input \code{pairs} is a two-column matrix specifying the pairs of points, with the first column corresponding to one point and the second column to the other. The numbers in \code{pairs} are indices of grid points (not point coordinates themselves). The assumed numbering scheme for the grid points is as follows: the points are first numbered across the first dimension (of length \code{nx}) and then along a second dimension. For example, on a 5x4 grid points 1-5 would be across the first row, 6-10 across the second row, etc. For each row the point numbering starts in the same column. For \code{distanceGridUnits()} the number of columns does not need to be specified since this can be found from the point index in \code{pairs}. See "Examples" for an explanation of the numbering scheme.
}
\value{a vector of the distances between the specified pairs of grid points.}
\author{
Aaron Olsen
}
\seealso{
\code{\link{dltTestCalibration}}
}
\examples{
## INDICES OF POINT PAIRS ON A GRID WITH 5 ROWS
pairs <- matrix(c(1,1, 5,10, 6,16, 1,20), nrow=4, ncol=2, byrow=TRUE)

## FIND THE DISTANCE BETWEEN PAIRS OF POINTS IN GRID UNITS
## NOTE LAST DISTANCE IS 5 BECAUSE IT IS A 3,4,5-TRIANGLE
distanceGridUnits(pairs, nx=5)

## FOR ILLUSTRATION, HERE IS A GRID WITH 5 ROWS AND 4 COLUMNS
xy <- cbind(rep(0:4, 4), c(rep(0, 5), rep(1, 5), rep(2, 5), rep(3, 5)))

## PLOT THESE POINTS
plot(xy)

## PLOT LINE SEGMENTS CONNECTING THE PAIRS ABOVE
segments(x0=xy[pairs[, 1], 1], y0=xy[pairs[, 1], 2],
  x1=xy[pairs[, 2], 1], y1=xy[pairs[, 2], 2],
  col=c('blue', 'red', 'purple', 'green'))
}
\keyword{ grid functions }