\name{measureCheckerboardSize}
\alias{measureCheckerboardSize}
\alias{summary.measureCheckerboardSize}
\alias{print.summary.measureCheckerboardSize}
\title{Estimates checkerboard square size}
\description{
This function estimates the square size of a checkerboard, optionally scaling this to real-world units (e.g. millimeters).
}
\usage{
measureCheckerboardSize(corner.file, nx, ruler.file=NULL, ruler.pt.size=NULL)

\method{summary}{measureCheckerboardSize}(object, ...)
}
\arguments{
  \item{corner.file}{a file path to text file containing a matrix of internal corners from a checkerboard pattern (a point grid) or the matrix itself. The text file must not have row names or a header.}
  \item{nx}{the number of internal corners in the first dimension along which the checkerboard points are ordered.}
  \item{ruler.file}{a file path to a text file containing a matrix of evenly spaced points digitized along a ruler (or comparable standard) or the matrix itself. The text file must have row names but no header or column names.}
  \item{ruler.pt.size}{the size of the spacing between points in the \code{ruler.file} matrix in real world units. This can be numeric or alphanumeric including the unit (see "Details").}
  \item{object}{a list of class \code{"measureCheckerboardSize"}.}
  \item{...}{further arguments passed to other methods.}
}
\details{
\code{corner.file} can be a file path to a text file containing a matrix of internal corners from a checkerboard pattern (ie points in a regular grid pattern) or the matrix itself. These can be automatically detected from a JPEG image using the function \code{\link{findCheckerboardCorners}}. The function first fits a camera perspective model to the corner points to robustly compare the opposing side lengths of the grid (see \code{\link{resampleGridImagePoints}}). These are returned as \code{side.lengths} and are displayed when calling the summary method. Opposing sides that differ greatly in length indicate that the grid was not completely flat relative to the image plane when it was photographed.

\code{measureCheckerboardSize()} then estimates the checkerboard or grid square size by fitting a simple grid model to the points (see \code{\link{gridPointsFit}}). The best fitting parameters are used to estimate the square size. Model fitting is more robust to noise in the grid point coordinates than taking the mean inter-point distance, for instance. The model goodness of fit can be assessed by the returned elements \code{dist.corner.fit.mean} and \code{dist.corner.fit.sd}.

\code{ruler.file} can be a file path to a text file containing a matrix of points at equal intervals along a ruler or the matrix itself. These ruler points can be digitized from an image using the function \code{\link{digitizeImage}}. If \code{ruler.file} is \code{NULL}, then only the checkerboard square size (in the input units) is returned. All other return values are \code{NULL}. If \code{ruler.file} is non-\code{NULL}, the distance between consecutive ruler points (the ruler point interval) is estimated by fitting a model of points at a regular interval along a line (see \code{\link{gridPointsFit}}). The goodness of fit for the ruler point model can be assessed by the returned elements \code{dist.ruler.fit.mean} and \code{dist.ruler.fit.sd}. The estimated ruler point interval is used to scale the checkerboard square size to the units of \code{ruler.pt.size}.

\code{ruler.pt.size} can be numeric or alphanumeric (including the units). For example, \code{'1'}, \code{'1 mm'} and \code{'1.0 mm'} are all possible inputs to \code{ruler.pt.size}. The units are automatically extracted and only used in the summary function to help interpret the function results. \code{measureCheckerboardSize()} also returns the estimated real-world size of a pixel. This represents the resolution of the camera at the surface of the checkerboard pattern.

See \href{http://stereomorph.blogspot.com/2014/04/measuring-checkerboard-square-size_4.html}{Measuring checkerboard square size} for a tutorial on this function.

See \href{http://stereomorph.blogspot.com/2014/04/auto-detecting-checkerboard-corners.html}{Auto-detecting checkerboard corners} for a tutorial on how to automatically detect checkerboard corners from an image.
}
\value{
a list of class \code{"measureCheckerboardSize"} with the following elements:
  \item{side.lengths}{the lengths of the four sides of the grid estimated by camera perspective model fitting.}
  \item{dist.corner.fit.mean}{the mean difference between the corner points \code{corner.file} and those generated assuming the best-fit simple grid model.}
  \item{dist.corner.fit.sd}{the standard deviation in the difference between the corner points \code{corner.file} and those generated assuming the best-fit model.}
  \item{square.size.px}{the best-fit estimate of the checkerboard square size in pixels.}
  \item{square.size.rwu}{the best-fit estimate of the checkerboard square size in real-world units. \code{NULL} if \code{ruler.file} is \code{NULL}.}
  \item{dist.ruler.fit.mean}{the mean difference between the \code{ruler.file} matrix and those generated assuming the best-fit model. \code{NULL} if \code{ruler.file} is \code{NULL}.}
  \item{dist.ruler.fit.sd}{the standard deviation in the difference between the \code{ruler.file} matrix and those generated assuming the best-fit model. \code{NULL} if \code{ruler.file} is \code{NULL}.}
  \item{ruler.size.px}{the best-fit estimate of the distance between consecutive points on the ruler (in pixels) in the plane of the imaged grid. \code{NULL} if \code{ruler.file} is \code{NULL}.}
  \item{rwu.per.px}{the real-world size of a pixel in the image (the length of one side of the pixel) in the plane of the imaged grid. \code{NULL} if \code{ruler.file} is \code{NULL}.}
  \item{unit}{if \code{ruler.pt.size} includes a unit, the unit. \code{NULL} if \code{ruler.file} is \code{NULL}.}
}
\author{Aaron Olsen}
\seealso{
\code{\link{drawCheckerboard}}, \code{\link{resampleGridImagePoints}}, \code{\link{gridPointsFit}}, \code{\link{digitizeImage}}
}
\examples{
## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CHECKERBOARD POINTS FILE
corner_file <- paste0(fdir, "checker_21_14_200(9).txt")

## NUMBER OF INTERNAL CORNERS IN THE HORIZONTAL DIMENSION
nx <- 21

## NUMBER OF INTERNAL CORNERS IN THE VERTICAL DIMENSION
ny <- 14

## SET FILE PATH TO RULER POINTS FILE
ruler_file <- paste0(fdir, "ruler_21_14_200(9).txt")

## ESTIMATE SQUARE SIZE
square_size <- measureCheckerboardSize(corner.file=corner_file, nx=nx)

## PRINT SUMMARY
summary(square_size)



## ESTIMATE SQUARE SIZE AND SCALE WITH RULER POINTS
square_size_scale <- measureCheckerboardSize(corner.file=corner_file, nx=nx,
    ruler.file=ruler_file, ruler.pt.size='1 mm')

## PRINT SUMMARY
summary(square_size_scale)



## INPUT MATRICES DIRECTLY
## READ POINTS INTO MATRICES
corner_pts <- as.matrix(read.table(corner_file))
ruler_pts <- as.matrix(read.table(ruler_file, row.names=1))

## ESTIMATE SQUARE SIZE AND SCALE WITH RULER POINTS
square_size_scale <- measureCheckerboardSize(corner.file=corner_pts, nx=nx,
    ruler.file=ruler_pts, ruler.pt.size='1 mm')

}
\keyword{ grid functions }
\keyword{ calibration }