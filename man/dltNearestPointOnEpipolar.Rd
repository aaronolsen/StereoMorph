\name{dltNearestPointOnEpipolar}
\alias{dltNearestPointOnEpipolar}
\title{Returns the closest point on a epipolar line to a point or points}
\description{
Given the same point in two camera views, this function finds the nearest point on the epipolar line of the point in the first view to a point or points in the second view.}
\usage{
dltNearestPointOnEpipolar(p1, p2, cal.coeff)
}
\arguments{
  \item{p1}{vector of x,y pixel coordinates for a point in the camera view corresponding to the first column of \code{cal.coeff}. This point will be used to generate an epipolar line in the second view.}
  \item{p2}{an x,y vector or two-column matrix of a point or points in a second camera view, corresponding to the second column of \code{cal.coeff}. The nearest point on the epipolar line will be an orthogonal projection from a point in \code{p2}.}
  \item{cal.coeff}{a two-column matrix of DLT calibration coefficients, where each column corresponds to the views from which \code{p1} and \code{p2} were taken, respectively.}
}
\details{
In a stereo camera setup, a point in one camera view must fall somewhere along a line in a second camera view. This line is called its epipolar line. Due to error in manually selecting the same point in two camera views and error in the calibration, the epipolar line of the point in the first view will not intersect exactly with the same point in the second view. The nearest point on the epipolar line is a point at a minimum distance from the point in the second view. This is equivalent to the orthogonal projection (\code{\link{orthogonalProjectionToLine}}) of the point in the second view onto the epipolar line of the point in the first view. The length of this line is the epipolar distance (\code{\link{dltEpipolarDistance}}).

\code{dltNearestPointOnEpipolar()} first finds the epipolar line of \code{p1}, a point in the first camera view, and then finds the point on this epipolar line nearest to point(s) \code{p2} in the second camera view. If \code{p2} is a single point, \code{dltNearestPointOnEpipolar()} finds the point on the epipolar line closest to \code{p2}. If \code{p2} is a matrix of points, the point in \code{p2} closest to the epipolar line is first identified and then the point on the epipolar line closest to this point is determined.
}
\value{
a list with the following elements:
  \item{matching.pt}{an x,y vector of the point on the epipolar line of \code{p1} closest to point(s) \code{p2}.}
  \item{min.idx}{the index in \code{p2} of the nearest point to the epipolar line of \code{p1}. If \code{p2} is a single point (vector), \code{min.idx} will be 1.}
  \item{p2.dist}{the epipolar distance between \code{matching.pt} and the point in \code{p2} at the \code{min.idx}.}
}
\references{
For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\seealso{
\code{\link{dltCalibrateCameras}}, \code{\link{dltEpipolarDistance}}, \code{\link{dltEpipolarLine}}, \code{\link{dltMatchCurvePoints}}
}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CALIBRATION COEFFICIENTS IN TWO CAMERA STEREO SETUP
cc_file <- paste0(fdir, "cal_coeffs.txt")

## LOAD COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=cc_file))

## GET LANDMARKS IN FIRST CAMERA VIEW
lm_files <- paste0(fdir, c("lm_2d_a1_v1.txt", "lm_2d_a1_v2.txt"))

## READ LANDMARKS INTO MATRIX
lm.array <- readLandmarksToArray(file=lm_files, row.names=1)

## FIND THE NEAREST POINT ON THE EPIPOLAR LINE OF P1
dltNearestPointOnEpipolar(p1=lm.array[3, , 1], p2=lm.array[3, , 2], cal.coeff=cal.coeff)

## FIND THE NEAREST POINT ON THE EPIPOLAR LINE OF P1
## THIS TIME USING ALL LANDMARKS IN THAT VIEW
## FUNCTION IDENTIFIES THE CORRECT LANDMARK IN THE SECOND VIEW AS THE SAME LANDMARK
dltNearestPointOnEpipolar(p1=lm.array[3, , 1], p2=lm.array[, , 2], cal.coeff=cal.coeff)

}
\keyword{ DLT }
\keyword{ epipolar functions }