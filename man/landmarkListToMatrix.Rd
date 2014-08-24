\name{landmarkListToMatrix}
\alias{landmarkListToMatrix}
\title{Converts a landmark list to a landmark matrix}
\description{Converts a landmark list to a landmark matrix. The landmark matrix is identical to the matrix that would be returned if the landmark files were sent directly to \code{\link{readLandmarksToMatrix}}.
}
\usage{
landmarkListToMatrix(lm.list)
}
\arguments{
  \item{lm.list}{a landmark list. See \code{\link{readLandmarksToList}}.}
}
\value{a landmark matrix.}
\author{Aaron Olsen}
\seealso{\code{\link{readLandmarksToList}}, \code{\link{readLandmarksToMatrix}}}
\examples{
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILES TO LOAD - TWO DIFFERENT 3D POINT SETS
file <- paste0(fdir, "lm_3d_even_a", 1:2, ".txt")

## READ LANDMARKS INTO A LIST
lm.list <- readLandmarksToList(file=file, row.names=1)

## CONVERT LANDMARK LIST TO LANDMARK MATRIX
lm.matrix <- landmarkListToMatrix(lm.list)

lm.matrix
}