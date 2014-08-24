\name{dltInverse}
\alias{dltInverse}
\title{Returns ideal pixel coordinates of 3D point(s) in a stereo camera setup}
\description{
This function takes 3D coordinates and the DLT calibration coefficients corresponding to one camera view and returns the ideal pixel coordinates of the 3D points in that camera view.
}
\usage{
dltInverse(cal.coeff, coor.3d)
}
\arguments{
  \item{cal.coeff}{a single column matrix of DLT calibration coefficients for one camera view.}
  \item{coor.3d}{a three-column matrix of 3D coordinates.}
}
\details{
When \code{\link{dltReconstruct}} is used to reconstruct points in 3D based on pixel coordinates from two or more camera views, these 3D points can be projected back into any camera view at their "ideal" pixel coordinates (the "inverse" of reconstruction). The "ideal" pixel coordinates represent the pixel coordinates in each view if there were no error (i.e. all pixel coordinates in every view correspond to the exact same point in 3D). In any real-world system there is some error and these ideal pixel coordinates are compared to the original pixel coordinates used in the reconstruction to assess reconstruction error. \code{dltInverse()} is called by \code{\link{dltCoefficients}} and \code{\link{dltEpipolarLine}}.

Since \code{dltInverse()} only projects the 3D coordinates into a single camera view, only one column of the DLT coefficients should be input. Currently, \code{dltInverse()} only works with the 11-parameter DLT model.
}
\value{
a two-column matrix of pixel coordinates of all points in \code{coor.3d} in the camera view corresponding to \code{cal.coeff}.
}
\references{
Abdel-Aziz, Y.I., Karara, H.M. (1971) Direct linear transformation into object space coordinates in close-range photogrammetry. \emph{Proc. Symp. on Close-Range Photogrammetry} (University of Illinois at Urbana-Champaign).

Hedrick, T.L. (2008) Software techniques for two- and three-dimensional kinematic measurements of biological and biomimetic systems. \emph{Bioinspiration & Biomimetics}, \bold{3} (034001).

For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\note{This function was modified by A Olsen from the Matlab function \code{dlt_inverse()} written by T Hedrick.
}
\seealso{
\code{\link{dltCalibrateCameras}},
\code{\link{dltReconstruct}}
}
\examples{
## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CALIBRATION COEFFICIENTS IN TWO CAMERA STEREO SETUP
cc_file <- paste0(fdir, "cal_coeffs.txt")

## LOAD COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=cc_file))

## READ LANDMARKS INTO MATRIX
lm.matrix <- readLandmarksToMatrix(file=paste0(fdir, "lm_3d_a1.txt"), row.names=1)

## GET IDEAL 2D COORDINATES OF 3D POINTS IN FIRST CAMERA VIEW
dltInverse(cal.coeff[, 1], lm.matrix)

## GET IDEAL 2D COORDINATES OF 3D POINTS IN SECOND CAMERA VIEW
dltInverse(cal.coeff[, 2], lm.matrix)
}
\keyword{ DLT }
