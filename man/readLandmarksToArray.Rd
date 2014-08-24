\name{readLandmarksToArray}
\alias{readLandmarksToArray}
\title{Reads landmark file(s) into an array}
\description{
Reads landmarks from one or more files into an array. The files can be input as a vector or matrix and will be grouped into the returned array according to their input grouping. In every case, the first dimension corresponds to the number of landmarks and the second dimension corresponds to the number of landmark dimensions (2 for 2D landmarks, 3 for 3D landmarks, etc.).
}
\usage{
readLandmarksToArray(file, na.omit = FALSE, ...)
}
\arguments{
  \item{file}{a vector or matrix of file paths to be read. Each file should contain a single landmark matrix.}
  \item{na.omit}{whether landmarks with NA values in any file should be omitted.}
  \item{...}{further arguments to be passed to \code{read.table()}.}
}
\details{
This function will read landmark matrices from one or more files and use the row names in each matrix to match up corresponding landmarks into a single array, filling in missing landmarks with \code{NA}. The landmark files are read by \code{read.file()} and should thus conform to all requirements of \code{read.file()}. Arguments for \code{read.file()} can be passed through \code{readLandmarksToList()} (e.g. \code{header}, \code{row.names}, etc.). If the landmark matrices do not have row names, this function assumes that landmarks in the same rows correspond and the number of rows in each landmark file should be the same.

Since stereo camera setups involve at least two camera views and usually more than one orientation per view, \code{readLandmarksToArray()} can be used to create a single array with multiple orientations and views of the same or overlapping landmark sets. If a vector of file paths is input, \code{readLandmarksToArray()} returns a three-dimensional array in which the first two dimensions are the landmark matrices and the last dimension is the index in the input file vector. If a matrix of file paths is input, \code{readLandmarksToArray()} returns a four-dimensional array in which the first two dimensions are the landmark matrices and the last two dimensions are the indices in the input file matrix (see "Examples").

This function is called by \code{\link{readCheckerboardsToArray}} to read in checkerboard corners with additional arguments available through \code{\link{readCheckerboardsToArray}} for manipulating the corner point order.
}
\value{a landmark array of three or four dimensions.}
\author{Aaron Olsen}
\seealso{
\code{\link{readCheckerboardsToArray}}, \code{\link{readLandmarksToList}}, \code{\link{readLandmarksToMatrix}}
}
\examples{
## READING IN LANDMARKS WITH ROW NAMES ##
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILES TO LOAD
file <- paste0(fdir, "lm_3d_even_a", 1:3, ".txt")

## LOAD FILES INTO AN ARRAY
lm.array <- readLandmarksToArray(file=file, header=FALSE, row.names=1)

## VIEW THE FIRST FIVE LANDMARKS
lm.array[1:5, , ]

## LOAD FILES INTO AN ARRAY OMITTING ALL NA
lm.array <- readLandmarksToArray(file=file, header=FALSE, row.names=1, na.omit=TRUE)

## VIEW ARRAY
## NOTE THERE ARE ONLY THREE LANDMARKS SHARED AMONG FILES THAT ARE NOT NA
lm.array


## READING SINGLE VS. VECTOR VS. MATRIX FILE INPUTS ##
## SET FILE PATHS
file <- matrix(c(paste0(fdir, "rcta_a", 1:3, "_v1.txt"), 
  paste0(fdir, "rcta_a", 1:3, "_v2.txt")), ncol=2)

## READ A SINGLE FILE PATH
## TREATED AS A VECTOR OF LENGTH ONE
readLandmarksToArray(file=file[1, 1])

## READ A FILE PATH VECTOR
readLandmarksToArray(file=file[, 1])

## READ A FILE PATH MATRIX
readLandmarksToArray(file=file)
}
\keyword{ landmarks }
\keyword{ read functions }