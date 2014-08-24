\name{dltCoefficients}
\alias{dltCoefficients}
\alias{summary.dltCoefficients}
\alias{print.summary.dltCoefficients}
\title{Computes DLT coefficients for a stereo camera setup}
\description{
This function takes 3D coordinates and their corresponding 2D coordinates in one or more camera views and returns DLT calibration coefficients. The DLT coefficients can then be used in 3D reconstruction and calculation of epipolar lines.
}
\usage{
dltCoefficients(coor.3d, coor.2d)
}
\arguments{
  \item{coor.3d}{a three-column matrix of 3D coordinates.}
  \item{coor.2d}{an three-dimensional array of 2D pixel coordinates.}
}
\details{
This function takes 3D coordinates and their corresponding 2D coordinates in one or more camera views and returns DLT calibration coefficients. Note that to find the calibration coefficient for a particular camera view, only the pixel coordinates in that camera view and their corresponding 3D coordinates are used. Thus, it is possible to derive calibration coefficients for several cameras without any overlap among the views in the points used to derive the calibration coefficients. Any missing values (either in \code{coor.3d} or pixel coordinates missing in a particular view in \code{coor.2d}) can be input as \code{NA}; they will be ignored.

The requirements for the structure of the \code{coor.2d} array are as follows. The first dimension of \code{coor.2d} is the number of points used in calculating the DLT coefficients. The number of elements in the first dimension of \code{coor.2d} must match the number of rows in \code{coor.3d} and these must be corresponding points (though some may be \code{NA}). The second dimension of \code{coor.2d} should be two as these are x,y-coordinates. The third dimension of \code{coor.2d} is the number of camera views. This will correspond to the number of columns in the returned calibration coefficient matrix.

For more information on stereo camera calibration, see \href{http://stereomorph.blogspot.com/2014/04/calibrating-stereo-cameras.html}{Calibrating stereo cameras}.
}
\value{
a list of class \code{"dltCoefficients"} with the following elements:
  \item{cal.coeff}{the calibration coefficient matrix.}
  \item{rmse}{the root-mean-square error for each camera view.}
}
\references{
Abdel-Aziz, Y.I., Karara, H.M. (1971) Direct linear transformation into object space coordinates in close-range photogrammetry. \emph{Proc. Symp. on Close-Range Photogrammetry} (University of Illinois at Urbana-Champaign).

Hedrick, T.L. (2008) Software techniques for two- and three-dimensional kinematic measurements of biological and biomimetic systems. \emph{Bioinspiration & Biomimetics}, \bold{3} (034001).

For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\note{This function was modified by A Olsen from the Matlab function \code{dlt_reconstruct()} written by T Hedrick.
}
\seealso{
\code{\link{dltCalibrateCameras}}, \code{\link{findCheckerboardCorners}}
}
\examples{
## SET NUMBER OF INTERNAL CORNERS AND ROWS
nx <- 21
ny <- 14

## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## GET FILE PATHS FOR PIXEL COORDINATES FROM CALIBRATION IMAGES 1 AND 5
file2d <- matrix(c(paste0(fdir, "cal_a", c(1, 5), "_v1.txt"), 
  paste0(fdir, "cal_a", c(1, 5), "_v2.txt")), ncol=2)

## READ INTO ARRAY
## THESE ARE THE 2D COORDINATES USED IN THE CALIBRATION
coor.2d <- readCheckerboardsToArray(file=file2d, nx=nx, ny=ny, col.reverse=TRUE)

## REDUCE ARRAY DIMENSIONS TO THREE
## THIS STACKS MULTIPLE VIEWS ON TOP OF EACH OTHER INTO THE SAME MATRIX
coor.2d <- apply(coor.2d, c(2, 4), matrix, byrow=FALSE)

## GET FILE PATH FOR CORRESPONDING 3D COORDINATES
file3d <- paste0(fdir, "cal_3d_coor.txt")

## READ 3D POINTS INTO MATRIX
coor.3d <- as.matrix(read.table(file=file3d))

## FIND THE DLT COEFFICIENTS
dlt_coeffcients <- dltCoefficients(coor.3d=coor.3d, coor.2d=coor.2d)

## PRINT THE SUMMARY
summary(dlt_coeffcients)

## NOTE THAT EACH CAMERA VIEW IS CALIBRATED SEPARATELY
## GIVES THE EXACT SAME RESULT
dltCoefficients(coor.3d=coor.3d, coor.2d=coor.2d[, , 1])
}
\keyword{ DLT }