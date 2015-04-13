\name{dltMatchCurvePoints}
\alias{dltMatchCurvePoints}
\alias{summary.dltMatchCurvePoints}
\alias{print.summary.dltMatchCurvePoints}
\title{Matches curve points between two camera views}
\description{This function uses DLT calibration coefficients to find corresponding points along a curve viewed from two different cameras in stereo camera setup.
}
\usage{
dltMatchCurvePoints(lm.list, cal.coeff, ref.view = "max", window.size = 30, 
                    min.tangency.angle = 0.1, min.dist.adj.slope = 0.1, 
                    plot.match = FALSE, fill.missing=TRUE)

\method{summary}{dltMatchCurvePoints}(object, ...)
}
\arguments{
  \item{lm.list}{a list of curve points from two camera views (see \code{\link{readLandmarksToList}}). \code{lm.list} can include landmarks; these will be returned unchanged.}
  \item{cal.coeff}{a two-column matrix of DLT calibration coefficients, where each column corresponds to the views from which points in \code{lm.list} were taken.}
  \item{ref.view}{which of the two views to use as a reference. Possible values are \code{1}, \code{2}, \code{"max"} and \code{"min"}.}
  \item{window.size}{the number of curve points to consider in each corresponding point search.}
  \item{min.tangency.angle}{a threshold for which parts of the curve should not be matched because they are nearly parallel to the epipolar line. See "Details".}
  \item{min.dist.adj.slope}{a threshold for which parts of the curve should not be matched because a point is not clearly the best match in comparison with neighboring points.}
  \item{plot.match}{a logical indicating whether a plot should be drawn showing the point matches between two curves. This is not optimized for visualization and might not always provide the best view of the matching results.}
  \item{fill.missing}{a logical whether missing segments should be filled.}
  \item{object}{a list of class \code{"dltMatchCurvePoints"} (the output of \code{dltMatchCurvePoints()}).}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
Point reconstruction with a stereo camera setup requires pixel coordinates of the same point in two or more camera views. Curves can also be reconstructed in a stereo camera setup if reconstructed as a series of single points. This, however, poses an additional challenge: that of identifying the same point on a curve viewed from two different perspectives. A point half-way along the curve in one view will not necessarily correspond to a point half-way along the same curve in another view. \code{dltMatchCurvePoints()} solves this challenge by using epipolar geometry informed by the DLT calibration coefficients (Yekutieli et al. 2007).

In a stereo camera setup, a point in one camera view must fall somewhere along a line in a second camera view. This line is called its epipolar line. If the same curve has been digitized in two camera views, the epipolar line of a point on the first curve should intersect the curve in the second camera view. The point at which the epipolar line intersects the curve in the second view represents the corresponding point on the second curve. \code{dltMatchCurvePoints()} iterates through all the points on one curve and uses epipolar geometry to identify the corresponding point on a second curve. The corresponding point is identified as a point on the epipolar line that is closest to the curve in the second view (rather than finding the intersection, per se). For more details on the use of epipolar geometry to solve for corresponding points see Yekutieli et al. (2007).

Two different types of curve point input to \code{dltMatchCurvePoints()} are possible. The first type is a list with two elements (\code{list[[1]]} and \code{list[[2]]}), containing the curve points of the first and second camera view, respectively. The second type is a list of the same form as the landmark list described in \code{\link{readLandmarksToList}}. The main elements of the landmark list are the landmarks and curves (\code{list[['landmark1']]}, \code{list[['curve1']]}, etc.). Each main element then has two elements (e.g. \code{list[['curve1']][[1]]}, \code{list[['curve1']][[2]]}) corresponding to the first and second camera views, respectively. The curve points themselves should be densely sampled pixel coordinates (e.g. single pixel spacing) in order to improve matching accuracy.

\code{dltMatchCurvePoints()} returns the landmark list as the element \code{match.lm.list} in the same format as the input, except that all curve points will be corresponding points. Note that list input is used, rather than a matrix, because the number of curve points may differ between the two views. Once the corresponding curve points are identified, however, the number of curve points in each view will be equal. Landmarks and curves containing less than three points are ignored and returned just as input. In this way, all landmarks and curve points can be passed through \code{dltMatchCurvePoints()} without having to be processed separately.

Although a stereo camera system may consist of more than two cameras, the coefficients of only two cameras should be input to \code{dltMatchCurvePoints()}. Only the coefficients of the two camera views for which corresponding curve points are being identified are relevant. Currently, this function can only match curve points between two camera views using the 11-parameter DLT model.

The curve points chosen as the reference are used to generate epipolar lines in a second camera view. The results will differ slightly depending on which view is chosen as a reference. By default, \code{dltMatchCurvePoints()} uses the curve with the maximum number of points as a reference. Users can specify which view is to be used as reference through \code{ref.view}. Setting \code{ref.view} to "min" will use the curve with the minimum number of points as a reference. Setting \code{ref.view} to 1 or 2 will use the first view or second view as a reference, respectively.

As \code{dltMatchCurvePoints()} steps through each point on the reference curve, it searches for the closest point on the epipolar line to the second curve. Rather than search for the closest point among all of the second curve points, \code{dltMatchCurvePoints()} only searches over a sliding window of points. \code{window.size} is the number of curve points considered at each iteration in identifying the corresponding point. A lower \code{window.size} will decrease the run-time but will potentially cause the function to miss corresponding points. If \code{curve.pt.dist} values are low, the current \code{window.size} is probably appropriate. \code{window.size} can be increased if \code{curve.pt.dist} values are high (over several pixels).

When the epipolar line is nearly parallel to the curve in the non-reference view, several points are equally likely to be the corresponding point and determining the actual corresponding point is impossible without additional information. The angle between the epipolar line and the points on the non-reference curve is referred to here as the tangency angle. When the tangency angle for points on the non-reference curve is less than \code{min.tangency.angle}, the current reference point is skipped. Additionally, when the points near a point on the non-reference curve are also very close to the epipolar line, a wrong match is more likely. Within the window of candidate points, the distance from each point to the epipolar line is calculated. The slope of these distances away from the point closest to the epipolar line (the minimum distance) is referred to here as the adjacent point distance slope. When the adjacent point distance slope is lower, the confidence that the minimum distance point is the correct match decreases. When this adjacent point distance slope is less than \code{min.dist.adj.slope}, the current reference point is skipped. The \code{min.tangency.angle} and \code{min.dist.adj.slope} are similar criteria, however the \code{min.dist.adj.slope} might provide more robust results with more irregular curves. Users might need to increase one or both of these values to obtain satisfactory results.

When reference points are skipped, these are filled in at the end with straight lines extending between defined points to either side of the skipped regions. Straight lines are used because these regions are likely to be nearly linear, owing to their minimal deviation from the epipolar line.

In addition to returning \code{match.lm.list}, \code{dltMatchCurvePoints()} also returns two vectors (or lists of vectors, depending on the format of \code{lm.list}) that can be used to assess the accuracy of the curve point matching. \code{epipolar.dist} is the epipolar distance between the epipolar line of the reference point and the corresponding point in the non-reference view. The first and last point are assumed to correspond, so there will be some epipolar error for these points. The remaining points are chosen on the epipolar line of the reference point, so their epipolar error will be zero. Future implementations may allow users to specify that corresponding points be on the curve in the second view and not necessarily on the epipolar line, in which case \code{epipolar.dist} will become more relevant. \code{curve.pt.dist} is the distance between the epipolar line and the nearest point on the curve in the second view for each curve point. If the exact same curve has been digitized in the two views, \code{curve.pt.dist} should be low (within a pixel or less).

For a tutorial on how to use \code{dltMatchCurvePoints()} see \href{http://stereomorph.blogspot.com/2014/04/reconstructing-2d-points-and-curves.html}{Reconstructing 2D points and curves into 3D}.
}
\value{
a list of class \code{"dltMatchCurvePoints"} with the following elements:
  \item{match.lm.list}{a landmark list of matched curve points (and landmarks if also input).}
  \item{epipolar.dist}{a list or vector of the epipolar distance between the epipolar line of the reference points and the corresponding non-reference point. In current implementation, all values will be zero except the start and end points.}
  \item{curve.pt.dist}{a list or vector of the distances from the chosen corresponding points and the nearest point on the non-reference curve.}
}
\references{
Yekutieli, Y.,  Mitelman, R., Hochner, B. and Flash, T. (2007). Analyzing Octopus Movements Using Three-Dimensional Reconstruction. \emph{Journal of Neurophysiology}, \bold{98}, 1775--1790.

For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\note{This function was written by A Olsen based on the methodology described in Yekutieli et al. 2007.}
\seealso{
\code{\link{readLandmarksToList}},
\code{\link{dltEpipolarLine}}, 
\code{\link{dltEpipolarDistance}}, 
\code{\link{dltNearestPointOnEpipolar}}
}
\examples{
## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO LANDMARK DATA
file <- paste0(fdir, "lm_2d_a2_v", 1:2, ".txt")

## LOAD COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=paste0(fdir, "cal_coeffs.txt")))

## READ LANDMARKS INTO LIST
lm.list <- readLandmarksToList(file=file, row.names=1)

## MATCH CURVE POINTS FOR ONE CURVE
## FIRST TYPE OF LANDMARK INPUT
## RETURNS LIST OF MATCHING POINTS WITHOUT CURVE NAME
dlt_match <- dltMatchCurvePoints(lm.list$pterygoid_crest_R, cal.coeff)

## PRINT SUMMARY
summary(dlt_match)

## SET A DIFFERENT REFERENCE VIEW
## SECOND VIEW HAS 80 FEWER POINTS
## dlt_match <- dltMatchCurvePoints(lm.list$pterygoid_crest_R, cal.coeff, ref.view=2)

## MATCH CURVE POINTS FOR ALL CURVES IN LIST
## SECOND TYPE OF LANDMARK INPUT
## RETURNS LIST OF ALL LANDMARKS AND MATCHED CURVE POINTS WITH CURVE NAMES
## dlt_match <- dltMatchCurvePoints(lm.list, cal.coeff)
}
\keyword{ DLT }