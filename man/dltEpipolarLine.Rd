\name{dltEpipolarLine}
\alias{dltEpipolarLine}
\title{Finds a epipolar or self-epipolar line}
\description{
This function takes a point in one camera view and returns either its epipolar line in another camera view or its epipolar line in that same camera view (self-epipolar line).
}
\usage{
dltEpipolarLine(p, cal.coeff1, cal.coeff2 = NULL, self = FALSE)
}
\arguments{
  \item{p}{vector of x,y pixel coordinates for a point in an image.}
  \item{cal.coeff1}{DLT calibration coefficients corresponding to the camera view from which \code{p} is taken or a two-column matrix of calibration coefficients in which the first column corresonds to the camera view from which \code{p} is taken and the second column corresponds to an additional camera view.}
  \item{cal.coeff2}{in the case that \code{cal.coeff1} is a single column matrix, these are the DLT calibration coefficients corresponding to a camera view in a stereo camera setup other than that from which \code{p} is taken.}
  \item{self}{a logical indicating whether the epipolar line returned should be a self-epipolar line.}
}
\details{
In a stereo camera setup, a point in one camera view must fall somewhere along a line in a second camera view. This line is called its epipolar line. If a second point is taken anywhere along this epipolar line in the second camera and its epipolar line is found in the first camera, the original point must fall along this line. The epipolar line in the first camera view, along which the original point falls, is called its self-epipolar line (Yakutieli et al. 2007). \code{dltEpipolarLine()} uses DLT calibration coefficients (see \code{\link{dltCalibrateCameras}}) to find the epipolar or self-epipolar line for a given point in a stereo camera setup.

Although a stereo camera system may consist of more than two cameras, the coefficients of only two cameras should be input to \code{dltEpipolarLine()}. Only the coefficients of the two cameras between which epipolar lines are being calculated are relevant. These two columns of coefficients can be input as one matrix (to \code{cal.coeff1}) or as two separate, one-column matrices (to \code{cal.coeff1} and \code{cal.coeff2}).

Currently, \code{dltEpipolarLine()} only works with the 11-parameter DLT model.
}
\value{
\code{dltEpipolarLine()} outputs the resulting epipolar line in two forms: slope-intercept coefficients (\code{m} and \code{b}) and two points on the line (\code{l1} and \code{l2}). These are stored in a list as follows:
  \item{m}{the slope of the epipolar line.}
  \item{b}{the y-intercept of the epipolar line.}
  \item{l1}{one point on the epipolar line.}
  \item{l2}{a second point on the epipolar line.}
}
\references{
Abdel-Aziz, Y.I., Karara, H.M. (1971) Direct linear transformation into object space coordinates in close-range photogrammetry. \emph{Proc. Symp. on Close-Range Photogrammetry} (University of Illinois at Urbana-Champaign).

Yekutieli, Y.,  Mitelman, R., Hochner, B. & Flash, T. (2007). Analyzing Octopus Movements Using Three-Dimensional Reconstruction. \emph{Journal of Neurophysiology}, \bold{98}, 1775--1790.

Hedrick, T.L. (2008) Software techniques for two- and three-dimensional kinematic measurements of biological and biomimetic systems. \emph{Bioinspiration & Biomimetics}, \bold{3} (034001).

For a general overview of DLT: \url{http://kwon3d.com/theory/dlt/dlt.html}
}
\author{Aaron Olsen}
\note{This function was modified by A Olsen from the Matlab function \code{partialdlt()} written by T Hedrick. A Olsen added the self-epipolar functionality after Yekutieli et al. 2007.
}
\seealso{
\code{\link{dltCalibrateCameras}}, \code{\link{dltEpipolarDistance}}, \code{\link{dltNearestPointOnEpipolar}}
}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CALIBRATION COEFFICIENTS IN TWO CAMERA STEREO SETUP
cc_file <- paste0(fdir, "cal_coeffs.txt")

## LOAD COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=cc_file))

## GET LANDMARKS IN FIRST CAMERA VIEW
lm_file <- paste0(fdir, "lm_2d_a1_v1.txt")

## READ LANDMARKS INTO MATRIX
lm.matrix <- readLandmarksToMatrix(file=lm_file, row.names=1)

## FIND EPIPOLAR LINE IN SECOND CAMERA VIEW
epipolar <- dltEpipolarLine(p=lm.matrix['occipital_condyle', ], cal.coeff1=cal.coeff)

## FIND SELF-EPIPOLAR LINE (IN FIRST CAMERA VIEW)
self_epipolar <- dltEpipolarLine(p=lm.matrix['occipital_condyle', ], cal.coeff1=cal.coeff, 
  self=TRUE)

## CONFIRM THAT DISTANCE FROM ORIGINAL POINT TO SELF-EPIPOLAR LINE IS ZERO
distancePointToLine(p=lm.matrix['occipital_condyle', ], l1=self_epipolar)
}
\keyword{ DLT }
\keyword{ epipolar functions }