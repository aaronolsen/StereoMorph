\name{resampleGridImagePoints}
\alias{resampleGridImagePoints}
\title{Resamples imaged grid points}
\description{
This function fits a 12-parameter image perspective model to imaged grid points and uses the model parameters to produce a grid with the same transformations but consisting of fewer points, effectively "resampling" the number of grid points. In this way, fewer points (but representing the same amount of information) can be used in more computationally intensive steps such as camera calibration. This function is called by \code{\link{dltCalibrateCameras}}.}
\usage{
resampleGridImagePoints(pts, nx, rx, ry, fit.min.break=1, 
                        print.progress = FALSE)
}
\arguments{
  \item{pts}{a matrix of grid points from an image, such as the internal corners of a checkerboard image.}
  \item{nx}{the number of points along the first dimension (e.g. this would be the number of points in each row if points in \code{pts} are listed first by row).}
  \item{rx}{the re-sampled number of points along the first dimension (corresponding to \code{nx}).}
  \item{ry}{the re-sampled number of points along the second dimension (e.g. if \code{nx} is the number of points per row, this is the new number of points per column).}
  \item{fit.min.break}{a minimum returned by \code{nlminb()} at which \code{resampleGridImagePoints()} will stop iterating to find a better fit.}
  \item{print.progress}{whether the model fit error should be printed. Error is in the same units as \code{pts}.}
}
\value{
a list with the following elements:
  \item{pts}{a matrix of resampled grid points.}
  \item{error}{the error (in the same units as \code{pts}) between the input \code{pts} and the model fit grid points of the same dimensions.}
}
\author{Aaron Olsen}
\seealso{
\code{\link{imagePlaneGridTransform}}, \code{\link{readCheckerboardsToArray}},  \code{\link{imagePlaneGridTransformError}}, 

\code{\link{dltCalibrateCameras}}
}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## GET GRID POINTS
file <- paste0(fdir, "cal_a1_v2.txt")

## SET NUMBER OF GRID ROWS AND COLUMNS
nx <- 21
ny <- 14

## READ THE GRID POINTS INTO A MATRIX
## OUTPUT OF FUNCTION IS AN ARRAY SO WE TAKE THE FIRST ENTRY TO GET MATRIX
coor.2d <- as.matrix(read.table(file))

## RESAMPLE THE GRID WITH THE SAME NUMBER OF POINTS AS IN ORIGINAL
coor_2d_same <- resampleGridImagePoints(pts=coor.2d, nx=nx, rx=21, ry=14, 
 print.progress=TRUE)

## RESAMPLE THE GRID WITH A REDUCED NUMBER OF POINTS (4 X 4)
coor_2d_red <- resampleGridImagePoints(pts=coor.2d, nx=nx, rx=4, ry=4, 
 fit.min.break=1, print.progress=TRUE)

## PLOT THE ORIGINAL IMAGED POINTS
plot(coor.2d)

## PLOT THE MODELED GRID POINTS WITHIN THE ORIGINAL POINTS
## THE MODEL GOODNESS-OF-FIT CAN BE EVALUATED VISUALLY
points(coor_2d_same$pts, col='red', cex=0.75)

## PLOT THE REDUCED NUMBER OF GRID POINTS
points(coor_2d_red$pts, col='green', lwd=2, cex=1.25)

## PLOT A HISTOGRAM OF THE FIT ERROR
## HERE UNITS ARE PIXELS - MOST POINTS ARE FIT WITHIN 2 PIXELS
hist(coor_2d_same$error)
}