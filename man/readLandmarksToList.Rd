\name{readLandmarksToList}
\alias{readLandmarksToList}
\title{Reads landmark file(s) into a list}
\description{
Reads landmarks from one or more files into a list. This function is useful when dealing with curves (semilandmarks) since curve points can be grouped by curve name for other operations.
}
\usage{
readLandmarksToList(file, semilandmark.pattern = "[0-9]+$", ...)
}
\arguments{
  \item{file}{a single landmark file or vector of landmark files to be read. Each file should contain a single landmark matrix with row names.}
  \item{semilandmark.pattern}{a regular expression pattern passed to \code{sub()} for identifying and grouping curve points. The default is landmark names ending in one or more numbers.}
  \item{...}{further arguments to be passed to \code{read.table()}.}
}
\details{
This function will read a landmark matrix from one or more files and use the row names in each matrix to match corresponding landmarks into list elements, ordered first by the landmark name and then numbered by the index of the file (in \code{file}) from which the landmark was read. Landmark lists are the required input format for \code{\link{dltMatchCurvePoints}}. Landmark lists are also one of three possible input formats for \code{\link{dltReconstruct}} and allow for curve points to be easily pulled out of a landmark set for curve fitting.

\code{semilandmark.pattern} is a regular expression passed to \code{sub()} to identify semilandmarks (curve points). By default, the regular expression \code{"[0-9]+$"} identifies row names that end in more than one digit (e.g. 'tomium_R004') as curve points. \code{sub()} removes the part of the string identified by \code{semilandmark.pattern} in order to group all curve points under one curve name (e.g. 'tomium_R004' would be grouped under 'tomium_R'). Curve grouping can be turned off by setting \code{semilandmark.pattern} to \code{""}. Once grouped, curve points are sorted only by the numeric portion of their row name (identified by \code{semilandmark.pattern} using \code{regexpr}). Preceding zeros are not necessary. For example, after sorting, the order of the following curve points would be: tomium_R1, tomium_R02, tomium_R9, tomium_R10. Note that if these were sorted simply by row name, the order would be: tomium_R02, tomium_R1, tomium_R10, tomium_R9. Landmarks missing from one or more files are given the value \code{NULL}.

The landmark files are read by \code{read.file()} and should thus conform to all requirements of \code{read.file()}. Arguments for \code{read.file()} can be passed through \code{readLandmarksToList()} (e.g. \code{header}, \code{row.names}, etc.).
}
\value{a landmark list.}
\author{Aaron Olsen}
\seealso{
\code{\link{readLandmarksToArray}}, \code{\link{readLandmarksToMatrix}}, \code{\link{readCheckerboardsToArray}}, 

\code{\link{dltMatchCurvePoints}}
}
\examples{
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILES TO LOAD - TWO DIFFERENT 3D POINT SETS
file <- paste0(fdir, "lm_3d_even_a", 1:2, ".txt")

## READ LANDMARKS INTO A LIST
lm.list <- readLandmarksToList(file=file, row.names=1)

## CURVE POINTS
## CURVE POINTS ARE ABSENT FROM FIRST POINT SET
lm.list[['tomium_R']]

## LANDMARKS PRESENT IN BOTH POINT SETS
lm.list[['quadrate_jugal_R']]

## LANDMARK MISSING FROM SECOND POINT SET
lm.list[['foramen_magnum_inf']]
}
\keyword{ landmarks }
\keyword{ read functions }