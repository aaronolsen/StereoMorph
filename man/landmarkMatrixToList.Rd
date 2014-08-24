\name{landmarkMatrixToList}
\alias{landmarkMatrixToList}
\title{Converts a landmark matrix to a landmark list}
\description{Converts a landmark matrix to a landmark list.}
\usage{
landmarkMatrixToList(lm.matrix, semilandmark.pattern='[0-9]+$', k=ncol(lm.matrix))
}
\arguments{
  \item{lm.matrix}{a landmark matrix. See \code{\link{readLandmarksToMatrix}}.}
  \item{semilandmark.pattern}{a regular expression pattern passed to \code{sub()} for identifying and grouping curve points. The default is landmark names ending in one or more numbers. To disable grouping, set to code{''}.}
  \item{k}{the number of dimensions of the landmark data.}
}
\value{a landmark list.}
\author{Aaron Olsen}
\seealso{\code{\link{landmarkListToMatrix}}, \code{\link{readLandmarksToList}}, \code{\link{readLandmarksToMatrix}}}
\examples{

## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILES TO LOAD - TWO DIFFERENT 3D POINT SETS
file <- paste0(fdir, "lm_2d_a1_v", 1:2, ".txt")

## READ LANDMARKS INTO A LIST
lm.matrix <- readLandmarksToMatrix(file=file, row.names=1)

## CONVERT LANDMARK LIST TO LANDMARK MATRIX
lm.list <- landmarkMatrixToList(lm.matrix, k=2)

## CAN BE CONVERTED BACK INTO MATRIX
## RECOVERING THE SAME MATRIX AS THE ORIGINAL
lm.matrix <- landmarkListToMatrix(lm.list)
}