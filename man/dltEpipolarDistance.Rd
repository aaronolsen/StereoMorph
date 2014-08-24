\name{dltEpipolarDistance}
\alias{dltEpipolarDistance}
\title{Finds the distance between a point and a self-epipolar line}
\description{
Given the same point in two camera views, this function finds the distance between the point in the second camera view and a epipolar line in the second view, as determined from the point in the first view. The option is also available to return the mean reciprocal epipolar distance.
}
\usage{
dltEpipolarDistance(p1, p2, cal.coeff, reciprocal = FALSE)
}
\arguments{
  \item{p1}{an x,y vector or two-column matrix of a point or points in the camera view corresponding to the first column of \code{cal.coeff}. This point will be used to generate an epipolar line in the second view.}
  \item{p2}{an x,y vector or two-column matrix of a point or points in a second camera view, corresponding to the second column of \code{cal.coeff}. The distance will be measured from this point to the epipolar line of \code{p1}.}
  \item{cal.coeff}{a two-column matrix of DLT calibration coefficients, where each column corresponds to the views from which \code{p1} and \code{p2} were taken, respectively.}
  \item{reciprocal}{a logical indicating whether epipolar distance should be calculated reciprocally and then averaged.}
}
\details{
In a stereo camera setup, a point in one camera view must fall somewhere along a line in a second camera view. This line is called its epipolar line. Due to error in manually selecting the same point in two camera views and error in the calibration, the epipolar line of the point in the first view will not intersect exactly with the same point in the second view. This distance between a point and the epipolar line of the same point in another view is the epipolar distance (or error).

The epipolar distance can be calculated between the point in the second view and the epipolar line of the point in the first view or between the point in the first view and the epipolar line of the point in the second view; the choice is arbitrary. This function performs the former. If a user would like to perform the latter, simply switch \code{p1} with \code{p2} \emph{and} reverse the column order of \code{cal.coeff} (see "Examples"). Another possibility is to perform both distance calculations and return an average (mean reciprocal epipolar distance). This can be done by setting \code{reciprocal} to \code{TRUE}.

Although a stereo camera system may consist of more than two cameras, the coefficients of only two cameras should be input to \code{dltEpipolarDistance()}. Only the coefficients of the two camera views for which epipolar distances are being calculated are relevant. Currently, this function only works with the 11-parameter DLT model.

A few options for input of \code{p1} and \code{p2} are available. If a single point is input for both, the epipolar distance is calculated for these two points. If a matrix of points is input for both (of the same dimensions), the epipolar distance is calculated pair-wise - points in the same row are treated as the same point. Lastly, if a single point is input as \code{p1} and a matrix is input as \code{p2}, the epipolar distance is calculated for \code{p1} relative to all points in \code{p2} (see "Examples").
}
\value{
a vector of the epipolar distance(s).
}
\references{
For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\seealso{
\code{\link{dltCalibrateCameras}}, \code{\link{dltEpipolarLine}}, \code{\link{dltNearestPointOnEpipolar}}, 

\code{\link{dltMatchCurvePoints}}
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

## FIND EPIPOLAR DISTANCE BETWEEN TWO SINGLE LANDMARKS
## EPIPOLAR DISTANCE (ERROR) IS AROUND 7 PIXELS
## IDENTIFYING THE EXACT SAME POINT IN TWO VIEWS MANUALLY IS CHALLENGING...
dltEpipolarDistance(p1=lm.array[1, , 1], p2=lm.array[1, , 2], cal.coeff=cal.coeff)

## FIND EPIPOLAR DISTANCE USING EPIPOLAR FROM SECOND VIEW INSTEAD
dltEpipolarDistance(p1=lm.array[1, , 2], p2=lm.array[1, , 1], cal.coeff=cal.coeff[, 2:1])

## FIND MEAN RECIPROCAL EPIPOLAR DISTANCE BETWEEN TWO SINGLE LANDMARKS
## THIS IS THE AVERAGE OF THE PREVIOUS TWO DISTANCES
dltEpipolarDistance(p1=lm.array[1, , 1], p2=lm.array[1, , 2], cal.coeff=cal.coeff, 
  reciprocal=TRUE)

## FIND EPIPOLAR DISTANCES BETWEEN ALL LANDMARKS
## PROCEEDS PAIRWISE BECAUSE p1 AND p2 HAVE THE SAME DIMENSIONS
dltEpipolarDistance(p1=lm.array[, , 1], p2=lm.array[, , 2], cal.coeff=cal.coeff)

## FIND EPIPOLAR DISTANCES BETWEEN FIRST LANDMARK AND ALL LANDMARKS
## HERE THE EPIPOLAR DISTANCES ARE HIGH BECAUSE ONLY THE FIRST LANDMARK
##  CORRESPONDS
## THE REMAINING POINTS ARE NOT THE SAME LANDMARK
dltEpipolarDistance(p1=lm.array[1, , 1], p2=lm.array[, , 2], cal.coeff=cal.coeff)

}
\keyword{ DLT }
\keyword{ epipolar functions }