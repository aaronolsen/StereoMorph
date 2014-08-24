\name{readLandmarksToMatrix}
\alias{readLandmarksToMatrix}
\title{Reads a landmark file or files into a matrix}
\description{
Reads landmarks from one or more files into a matrix. A single file or vector of files can be input. If more than one file is input, each matrix will be appended to the previous one with matching landmarks in the same row.
}
\usage{
readLandmarksToMatrix(file, na.omit = FALSE, ...)
}
\arguments{
  \item{file}{a single landmark file or vector of landmark files to be read. Each file should contain a single landmark matrix.}
  \item{na.omit}{whether landmarks with NA values in any file should be omitted.}
  \item{...}{further arguments to be passed to \code{read.table()}.}
}
\details{
This function will read a landmark matrix from one or more files and use the row names in each matrix to match corresponding landmarks into a single matrix, filling in missing landmarks with \code{NA}. The rows correspond to landmarks and the columns correspond to the number of landmark dimensions (2 for 2D landmarks, 3 for 3D landmarks, etc.). Each landmark matrix is appended as new columns onto the existing matrix. So, if three, 2D landmark files are input the resulting matrix would have six columns.

The landmark files are read by \code{read.file()} and should thus conform to all requirements of \code{read.file()}. Arguments for \code{read.file()} can be passed through \code{readLandmarksToList()} (e.g. \code{header}, \code{row.names}, etc.). All landmark matrices must have row names.
}
\value{a landmark matrix}
\author{Aaron Olsen}
\seealso{
\code{\link{readLandmarksToList}}, \code{\link{readLandmarksToArray}}, \code{\link{readCheckerboardsToArray}}
}
\examples{
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILES TO LOAD
file <- paste0(fdir, "lm_2d_a3_v", 1:2, "_wna.txt")

## LOAD FILES INTO A MATRIX
readLandmarksToMatrix(file=file, row.names=1)

## LOAD FILES INTO A MATRIX OMITTING NAS
readLandmarksToMatrix(file=file, row.names=1, na.omit=TRUE)
}
\keyword{ landmarks }
\keyword{ read functions }