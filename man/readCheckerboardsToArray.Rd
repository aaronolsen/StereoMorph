\name{readCheckerboardsToArray}
\alias{readCheckerboardsToArray}
\title{Reads file(s) containing grid points into an array}
\description{
This function reads grid point matrices into an array from a matrix files allowing for point order reversals along rows, columns, or both.
}
\usage{
readCheckerboardsToArray(file, nx, ny, col.reverse = FALSE, row.reverse = FALSE, 
                         na.omit=FALSE, ...)
}
\arguments{
  \item{file}{a matrix of file paths to be read into an array. Each file path should correspond to a file containing a single landmark matrix.}
  \item{nx}{the number of internal corners in the first dimension along which grid points are ordered.}
  \item{ny}{the number of internal corners in the second dimension along which grid points are ordered.}
  \item{col.reverse}{a logical indicating whether the column order of grid points should be reversed. Can be either single value, a vector or a matrix.}
  \item{row.reverse}{a logical indicating whether the row order of grid points should be reversed. Can be either single value, a vector or a matrix.}
  \item{na.omit}{whether landmarks with NA values in any file should be omitted.}
  \item{...}{further arguments to be passed to \code{readLandmarksToArray()}.}
}
\details{
When using planar grid points to find an optimal stereo calibration, ensuring that the grid point coordinates are listed in the same order from different camera views is challenging. When cameras are viewing the same points from different orientations (e.g. one camera is upside-down relative to another) and when the checkerboard itself is in different orientations, columns and/or rows in one grid point matrix could be flipped relative to another camera view.

\code{readCheckerboardsToArray()} enables correction for this by allowing users to specify whether the rows, columns or both should be reversed after the points are read from a file. If the checkerboard changes orientation within a single camera view it could be necessary to specify row and/or column reversals individually for each file. \code{col.reverse} and \code{row.reverse} can both be either a single logical, a vector of logicals or a matrix of logicals. This allows \code{col.reverse} and \code{row.reverse} to be specified for all files in a vector or matrix or for each file separately. Vector inputs of \code{col.reverse} and \code{row.reverse} with a matrix input of \code{file} will be applied to each column of \code{file} (see last example below).

Row reversal means that point order is reversed along the second dimension (the order along the first dimension is kept intact). Column reversal means that point order is reversed along the first dimension (the order along the second dimension is kept intact). Setting both \code{col.reverse} and \code{row.reverse} to \code{TRUE} is equivalent to reversing the order of points from start to end (row and column structures have no effect). These operations are perhaps best understood through the examples below. For an example of the grid point ordering scheme, also see \code{\link{distanceGridUnits}}.

For a step-by-step tutorial on how to automatically detect checkerboard corners in an image see \href{http://stereomorph.blogspot.com/2014/04/auto-detecting-checkerboard-corners.html}{Auto-detecting checkerboard corners}.
}
\value{an array of three or four dimensions.}
\author{Aaron Olsen}
\seealso{
\code{\link{readLandmarksToArray}}, \code{\link{readLandmarksToList}}, \code{\link{readLandmarksToMatrix}}
}
\examples{
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET NUMBER OF ROWS AND COLUMNS
## THESE ARE THE NUMBER OF INTERNAL CORNERS, NOT THE NUMBER OF SQUARES
nx <- 4
ny <- 3

## SET FILE PATHS
file <- matrix(c(paste0(fdir, "rcta_a", 1:3, "_v1.txt"), 
                 paste0(fdir, "rcta_a", 1:3, "_v2.txt")), ncol=2)

## READ MATRIX OF FILES ##
## REVERSE COLUMNS IN FIRST COLUMN OF FILE MATRIX
## REVERSE ROWS IN ALL FILES
readCheckerboardsToArray(file, nx, ny, col.reverse=c(TRUE, FALSE), row.reverse=TRUE)
}
\keyword{ read functions }