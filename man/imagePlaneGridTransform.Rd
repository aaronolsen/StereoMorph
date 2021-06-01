\name{imagePlaneGridTransform}
\alias{imagePlaneGridTransform}
\title{Performs image perspective transformations to a grid}
\description{
This function takes parameters describing a 3D planar grid projected onto a 2D image plane and returns a grid of specified dimensions. Users will probably not call this function directly. Rather, it is used by \code{\link{resampleGridImagePoints}} to produce grid points with the same transformations as an imaged grid but with fewer points.
}
\usage{
imagePlaneGridTransform(p, nx, ny)
}
\arguments{
  \item{p}{a vector of 12 grid parameters. The first eight values are the x,y-coordinates of the four grid corners (\code{x1}, \code{y1}, \code{x2}, \code{y2}, etc.) and the last four values are transformation parameters for slope and interpoint spacing.}
  \item{nx}{the number of points along the first dimension. Note that although the grid can have a different number of rows than columns, the grid units themselves should be square (of uniform size in both dimensions).}
  \item{ny}{the number of points along the second dimension.}
}
\details{
When taking a photo of planar grid points (such as the internal corners of a checkerboard pattern) arbitrarily oriented in 3D space, the distribution of grid points in a 2D photograph will reflect several transformations. The grid may be translated to any position within the image plane and rotated by any angle. Additionally, if the grid plane is not parallel to the image plane, perspective effects will cause points further away to appear closer together. When arbitrary 3D position and perspective effects are combined, the transformation of a planar grid can be quite extreme (see example).

\code{imagePlaneGridTransform()} takes 12 parameters describing these effects and applies them to a grid of the specified dimensions, returning the transformed grid points. The first eight parameters are the x,y-coordinates of the four grid corners (\code{x1}, \code{y1}, \code{x2}, \code{y2}, etc.). The ninth and tenth parameters describe how interpoint spacing changes from row-to-row and column-to-column, respectively. This is the \code{a} parameter in the function \code{\link{quadraticPointsOnInterval}}. A value of zero indicates uniform spacing between consecutive points across all rows while values less or greater than zero indicates points that become closer together or further apart from one row or column to the next. The eleventh and twelfth parameters are analogous to the ninth and tenth parameters but describe how spacing changes between rows and columns instead of between points. These last two parameters are also the \code{a} parameter in the function \code{\link{quadraticPointsOnInterval}}.

Currently, \code{imagePlaneGridTransform()} does not currently account for lens distortion (e.g. barrel, pincushion, etc.). If distortion is significant, users should undistort the photographs prior to using \code{imagePlaneGridTransform()}. It is hoped that future versions will include additional parameters to account for lens distortion.

Users will probably not call \code{imagePlaneGridTransform()} directly. In this package, this function is used by \code{\link{resampleGridImagePoints}} to both fit transformation parameters to a matrix of imaged grid points and to produce a transformed grid consisting of fewer points. In this way, fewer points (but representing the same amount of information) can be used in more computationally intensive steps.
}
\value{a matrix of transformed grid points.}
\author{Aaron Olsen}
\seealso{
\code{\link{resampleGridImagePoints}}, \code{\link{quadraticPointsOnInterval}}, \code{\link{imagePlaneGridTransformError}}
}
\examples{
## SET GRID PARAMETERS
## THE FIRST 8 NUMBERS ARE CORNERS
## THE LAST 4 NUMBERS ARE TRANSFORMATION PARAMETERS
p <- c(3656, 379, 707, 264, 383, 1034, 3984, 1164, 63.772, -25.211, -0.818, -3.339)

## CREATE TRANSFORMED GRID
grid <- imagePlaneGridTransform(p=p, nx=21, ny=14)

## PLOT GRID
plot(grid)

## MARK CORNERS OF GRID FROM p
points(matrix(p[1:8], nrow=4, ncol=2, byrow=TRUE), col='red', lwd=2, cex=1.5)
}