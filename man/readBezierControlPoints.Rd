\name{readBezierControlPoints}
\alias{readBezierControlPoints}
\title{Reads a file of Bezier control points}
\description{
Reads Bezier control points from a file or files into a list grouped first by curve name and then by the index of the file from which they were read. A separate function from the standard read functions is necessary since the number of control points may differ for each Bezier curve or spline and, thus, the number of values may differ by row.
}
\usage{
readBezierControlPoints(file, ndim = 2, ...)
}
\arguments{
  \item{file}{file(s) to be read.}
  \item{ndim}{the number of dimensions of the Bezier curve points}
  \item{...}{further arguments to be passed to \code{readLines()}.}
}
\details{
The rows of each file must start with the name of the curve or spline followed by the control points, all separated by tabs. The control points are listed first by dimension and then by point (\code{x1\\ty1\\tx2\\ty2} etc.). For example, three Bezier points starting with [100, 200] would be on one line as follows, with \code{\\t} replaced by tabs.

\code{tomium_R\\t100\\t200\\t300\\t100\\t400\\t300}

Each Bezier curve or spline is first grouped into a list by curve name (e.g. \code{list$tomium_R}) and then by the index of the file from which it was read (e.g. \code{list$tomium_R[[1]]} from the first file). The control points are made into a matrix where the number of columns corresponds to \code{ndim}. The Bezier list structure is similar to the landmark list structure created by \code{\link{readLandmarksToList}} and can be used to generate points along a Bezier curve or spline. See the R package \href{https://cran.r-project.org/package=bezier}{bezier} for more details.
}
\value{
a list of Bezier control points grouped by name and file number.}
\author{Aaron Olsen}
\seealso{
\code{\link{readLandmarksToArray}}, \code{\link{readCheckerboardsToArray}}, \code{\link{readLandmarksToList}},

\code{\link{readLandmarksToMatrix}}
}
\examples{
## GET FILE DIRECTORY FOR PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## FILE TO READ
file <- paste0(fdir, "bezier_control_points_a2_v", 1:2, ".txt")

## FILE TO READ
bcp <- readBezierControlPoints(file=file)
}
\keyword{ read functions }