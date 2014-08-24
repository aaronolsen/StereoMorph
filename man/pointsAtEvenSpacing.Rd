\name{pointsAtEvenSpacing}
\alias{pointsAtEvenSpacing}
\title{Generates evenly spaced points from point matrix}
\description{
This function takes a matrix of points, calculates the cumulative distance from start to end and then uses the cumulative distance and intermediate points to generate evenly spaced points between the start and end points. Linear interpolation is used between neighboring points, so the returned points will either coincide with the input points or fall on straight lines between consecutive points.
}
\usage{
pointsAtEvenSpacing(x, n)
}
\arguments{
  \item{x}{a matrix or landmark list of points of any number of dimensions. If input is a list, only the first element is used.}
  \item{n}{the number of points to generate, including the start and end points.}
}
\details{
The function first removes all \code{NA} values. Then, the cumulative distance is calculated from the first to last point. The last value is taken as the total length of the line or curve, defined by matrix \code{x}. This total length is divided by \code{n-1} to find a uniform segment length that will separate \code{n} evenly spaced points, including the first and last non-\code{NA} values in \code{x}.

The function iterates through \code{x}, finding the point that is at a distance equal to or just less than the segment length from the previous point. If the selected point is at a distance less than the segment length from the previous point, a point is chosen on the line between this point and the next to complete the full segment length. In this way, returned points will either coincide with the input points or fall on straight lines between consecutive points.

In the simplest implementation, \code{pointsAtEvenSpacing()} can be used for linear interpolation (see first example below). Define the start and end points in \code{x} as a two-row matrix and then select the number of points to include on the line.

If \code{x} represents densely sampled points on a curve (see the second example below) and if the curve can be approximated by straight lines between consecutive points, then \code{pointsAtEvenSpacing()} will provide comparable results to other methods, such as function fitting. This is especially useful for curves not easily fit by a mathematical function.

For examples on how to use \code{pointsAtEvenSpacing()} see \href{http://stereomorph.blogspot.com/2014/04/reconstructing-2d-points-and-curves.html}{Reconstructing 2D points and curves into 3D}.
}
\value{a matrix of \code{n} points. The start and end points correspond to the first and last non-NA values in \code{x}.}
\author{Aaron Olsen}
\seealso{
\code{\link{imagePlaneGridTransform}}, \code{\link{resampleGridImagePoints}}, \code{\link{imagePlaneGridTransformError}}
}
\examples{
## LINEAR INTERPOLATION ##
## CREATE A MATRIX OF TWO POINTS
two_points <- matrix(c(0, 10, 0, 10), nrow=2, ncol=2)

## GENERATE 20 POINTS ALONG THE LINE
pts_aes <- pointsAtEvenSpacing(x=two_points, n=20)

## PLOT THE LINE
plot(two_points, type='l')

## AND THE POINTS ALONG THE LINE
points(pts_aes, col='red')


## POINTS ALONG A CURVE ##
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## GET 3D LANDMARK AND CURVE POINT FILE AND READ INTO A MATRIX
lm.matrix <- readLandmarksToMatrix(paste0(fdir, "lm_3d_a2.txt"), row.names=1)

## PLOT THE LANDMARKS AND CURVE POINTS
pts <- na.omit(lm.matrix)
r <- abs(apply(pts, 2, 'max') - apply(pts, 2, 'min'))

\dontrun{
## PLOT USING THE RGL PACKAGE
plot3d(pts, aspect=c(r[1]/r[3], r[2]/r[3], 1), size=0.5)
}

## CONVERT LANDMARKS TO LIST FORMAT TO EASILY ACCESS CURVE POINTS
lm.list <- landmarkMatrixToList(lm.matrix)

## CREATE 10 EVENLY SPACED POINTS ALONG ONE CURVE
lm.list$pterygoid_crest_R <- pointsAtEvenSpacing(x=lm.list$pterygoid_crest_R, n=10)

## CREATE 15 ALONG ANOTHER
lm.list$tomium_R <- pointsAtEvenSpacing(x=lm.list$tomium_R, n=15)

## CONVERT BACK TO MATRIX
lm.matrix <- landmarkListToMatrix(lm.list)

\dontrun{
## PLOT NEW EVENLY SPACED POINTS WITH PREVIOUS POINTS
plot3d(lm.matrix, add=T, size=4, col='red')
}
}