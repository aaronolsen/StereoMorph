\name{dltTestCalibration}
\alias{dltTestCalibration}
\alias{summary.dltTestCalibration}
\alias{print.summary.dltTestCalibration}
\title{Tests the accuracy of a stereo camera calibration}
\description{
This function uses a set of grid points, ideally other than those used in stereo camera calibration, to test calibration accuracy. Results of both distance-based and position-based accuracy tests are returned.
}
\usage{
dltTestCalibration(cal.coeff, coor.2d, nx, sq.size, 
                   reciprocal = TRUE, align.princomp = FALSE)

\method{summary}{dltTestCalibration}(object, print.tab = '', ...)
}
\arguments{
  \item{cal.coeff}{a matrix of DLT calibration coefficients. The columns correspond to each camera view and the column order should match the camera view order in the fourth dimension of the \code{coor.2d} array.}
  \item{coor.2d}{a four-dimensional array of grid points. The first two dimensions correspond to each matrix of grid points, the third corresponds to each grid position/orientation and the fourth corresponds to each camera view. These can be read in from file by \code{\link{readCheckerboardsToArray}}.}
  \item{nx}{the number of points along the first dimension (e.g. this would be the number of points in each row if points in \code{coor.2d} are listed first by row). The number of points along the second dimension is calculated based on the total number of points per view and orientation.}
  \item{sq.size}{the size of the grid squares in real-world units (e.g. millimeters).}
  \item{reciprocal}{a logical indicating whether epipolar distance should be calculated reciprocally and then averaged.}
  \item{align.princomp}{a logical indicating whether checkerboard corners should be aligned along principal coordinate axes prior to error testing (serves to describe error along axes that may be more physically meaningful than the initial, arbitrary coordinate system).}
  \item{object}{a list of class \code{"dltTestCalibration"} (the output of \code{dltTestCalibration()}).}
  \item{print.tab}{Tabs preceding lines printed to console.}
 \item{...}{further arguments passed to or from other methods.}
}
\details{
Although the RMS errors reported by \code{\link{dltCalibrateCameras}} can be used to assess the accuracy of a stereo camera setup, these represent how well the DLT parameters fit the calibration point set and not the reconstruction accuracy per se. It has been argued that in order to obtain a true estimation of reconstruction accuracy, an independent assessment criterion is required (Challis & Kerwin 1992). With the StereoMorph package, this is best accomplished by photographing a grid not used in the calibration and of a different square size (to test for proper scaling). These images are taken and the internal corners extracted just as in the calibration step (see \code{\link{dltCalibrateCameras}}), again ensuring that the test images fully sample the calibration volume and that the extracted point orders correspond between the two views. The input format of \code{coor.2d} to \code{dltTestCalibration()} is the same format as the \code{coor.2d} input to \code{\link{dltCalibrateCameras}}.

\code{dltTestCalibration()} measures the calibration accuracy using two approaches: a distance-based approach and a position-based approach. For the distance-based approach (e.g. Tashman & Anderst 2003; Brainerd et al. 2010), random pairs of grid points are chosen (without resampling), reconstructed and the distance between the reconstructed points is compared with the actual distance. The deviations from the true distance (interpoint distance error or IPD error) for each pair of points are returned in the \code{ipd.error} vector. \code{dltTestCalibration()} also measures IPD error of only adjacent points, returned in the vector \code{adj.pair.ipd.error}. With a sufficient number of grid points, adjacent points are close enough that one can test how IPD error varies as a function of the distance from the approximate center of the calibrated volume (\code{adj.pair.centroid.dist}) or along a particular dimension (\code{adj.pair.mean.pos}).

One challenge in interpreting the IPD error, however, is that each deviation represents error in the x, y and z position of two points. This makes it difficult to assess the accuracy of a particular point or along a particular dimension. Since we do not know the 3D coordinates of a test grid placed at an arbitrary orientation in the calibration volume, we must find the best fit 3D position in order to assess positional accuracy. For the position-based approach, \code{dltTestCalibration()} takes an ideal grid of the same square size and dimensions and optimally aligns it with the reconstructed test points using \code{\link{findOptimalPointAlignment}}. The reconstructed test points can then be compared with their corresponding reference points. These errors are returned in the matrix \code{aitr.error} (aligned ideal to reconstructed point position). This approach has the disadvantage that best fit alignment will tend to align the reference grid where the error is highest so as to minimize differences. This can decrease error where it is in actuality relatively high and vice versa.
}
\value{
a list of class \code{"dltTestCalibration"} with the following elements:
  \item{num.grids}{the number of test calibration grids used in accuracy assessment.}
  \item{epipolar.error}{the epipolar error (distance) for every test calibration point. This is the reciprocal epipolar distance if \code{reciprocal} is \code{TRUE}. See \code{\link{dltEpipolarDistance}}.}
  \item{epipolar.rmse}{the root-mean-square error of \code{epipolar.error}.}
  \item{ipd.error}{a vector of the deviations from the true distance between random pairs of points (without resampling).}
  \item{pair.dist}{a vector of the true distances between the random pairs of points in \code{ipd.error}.}
  \item{ipd.rmse}{the root-mean-square error of \code{ipd.error}.}
  \item{adj.pair.ipd.error}{a vector of the deviations from the true distance between random pairs of adjacent points (without resampling).}
  \item{adj.pair.mean.pos}{a three-column matrix of the mean position (midpoint) of the adjacent pairs of points in \code{adj.pair.ipd.error}.}
  \item{adj.pair.centroid.dist}{a vector of the distances from each point in \code{adj.pair.mean.pos} to the centroid of all \code{adj.pair.mean.pos}.}
  \item{aitr.error}{a three-column matrix of the x, y and z position errors for the reconstructed test calibration points relative to optimally aligned ideal grid points.}
  \item{aitr.dist.error}{a vector of the distances between the reconstructed test calibration points and the optimally aligned ideal grid points. Note that ideally this distance should be zero so all values in this vector are positive.}
  \item{aitr.dist.rmse}{the RMS error (or deviation) of \code{aitr.dist.error}.}
  \item{aitr.rmse}{a vector of the RMS error (or deviation) of \code{aitr.error} along each dimension. This is very similar to the standard deviation of \code{aitr.error} along each dimension.}
  \item{aitr.pos}{a three-column matrix of the ideal grid points after best fit alignment to the reconstructed grid points.}
  \item{aitr.centroid.dist}{a vector of the distances between each AITR point and the centroid of all AITR points.}
}
\references{
Challis, J.H. and Kerwin, D.G. (1992). Accuracy assessment and control point configuration when using the DLT for photogrammetry. \emph{Journal of Biomechanics}, \bold{25} (9), 1053--1058.

Tashman, S. and Anderst, W. (2003). \emph{In Vivo} Measurement of Dynamic Joint Motion Using High Speed Biplane Radiography and CT: Application to Canine ACL Deficiency. \emph{Transactions of the ASME}, \bold{125}, 238--245.

Brainerd, E.L., Baier, D.B., Gatesy, S.M., Hedrick, T.L., Metzger, K.A., Gilbert, S.L and Crisco, J.J. (2010). X-ray reconstruction of moving morphology (XROMM): Precision, accuracy and applications in comparative biomechanics research. \emph{Journal of Experimental Zoology}, \bold{313A}, 262--279.

For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\seealso{
\code{\link{dltCalibrateCameras}},
\code{\link{dltCoefficients}},
\code{\link{readCheckerboardsToArray}},
\code{\link{dltEpipolarDistance}},
\code{\link{findCheckerboardCorners}}
}
\examples{
## SET NUMBER OF INTERNAL ROWS AND COLUMNS
nx <- 21
ny <- 14

## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CHECKERBOARD CORNERS FROM TEST CALIBRATION IMAGE SET
file <- matrix(c(paste0(fdir, "test_cal_a", 1:11, "_v1.txt"), 
  paste0(fdir, "test_cal_a", 1:11, "_v2.txt")), ncol=2)

## READ IN CHECKERBOARD CORNERS
coor.2d <- readCheckerboardsToArray(file=file, nx=nx, ny=ny, col.reverse=FALSE)

## SET GRID SIZE OF TEST CHECKERBOARDS (IN MM)
sq.size <- 4.2218

## LOAD CALIBRATION COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=paste0(fdir, "cal_coeffs.txt")))

## TEST CALIBRATION ACCURACY
## USE ONLY A SUBSET (FIVE) OF TEST CALIBRATION IMAGES
## IN THE TUTORIAL POINTS, UNITS NOT IN PIXELS ARE MILLIMETERS
dlt_test <- dltTestCalibration(cal.coeff=cal.coeff, coor.2d=coor.2d[, , 1:5, ], nx=nx, 
  sq.size=sq.size)

## RUN TEST ON ALL TEST CALIBRATION IMAGES
\dontrun{
dlt_test <- dltTestCalibration(cal.coeff=cal.coeff, coor.2d=coor.2d, nx=nx, 
  sq.size=sq.size)
}

## PRINT SUMMARY
summary(dlt_test)

## PLOT A HISTOGRAM OF THE INTERPOINT DISTANCE ERROR
hist(dlt_test$ipd.error)

## PLOT ADJACENT POINT DISTANCE ERROR AS A FUNCTION OF POSITION ALONG THE Y-AXIS
dev.new()
plot(dlt_test$adj.pair.ipd.error, abs(dlt_test$adj.pair.mean.pos[, 2, ]))

## PLOT POSITION-BASED ERROR AS A FUNCTION OF POSITION ALONG THE X-AXIS
dev.new()
plot(dlt_test$aitr.pos[, 1, ], abs(dlt_test$aitr.error[, 1, ]))
}
\keyword{ DLT }
\keyword{ calibration }