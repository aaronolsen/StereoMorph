\name{dltReconstruct}
\alias{dltReconstruct}
\alias{summary.dltReconstruct}
\alias{print.summary.dltReconstruct}
\title{Reconstructs the 3D position of points in two or more camera views}
\description{
This function takes 2D pixel coordinates of a point or points from two more camera views and uses DLT coefficients to reconstruct their position in 3D.
}
\usage{
dltReconstruct(cal.coeff, coor.2d, min.views = 2)

\method{summary}{dltReconstruct}(object, ...)
}
\arguments{
  \item{cal.coeff}{a matrix of DLT calibration coefficients. The columns correspond to each camera view and the column order should match the camera view order of the landmarks in \code{coor.2d}.}
  \item{coor.2d}{2D pixel coordinates from two or more camera views. Format can be either a landmark matrix, list or array.}
  \item{min.views}{the minimum number of views required for a point to be reconstructed in 3D.}
  \item{object}{a list of class \code{"dltReconstruct"} (the output of \code{dltReconstruct()}).}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
This function uses DLT coefficients (calculated using \code{\link{dltCalibrateCameras}}, for example) to reconstruct the 3D position of points, based on their 2D position in two or more camera views. 2D pixel coordinates can be input as a landmark matrix (\code{\link{readLandmarksToMatrix}}), as a list (\code{\link{readLandmarksToList}}) or as an array (\code{\link{readLandmarksToArray}}).

A minimum of two views is required for 3D reconstruction although additional camera views can be used, potentially improving reconstruction accuracy. Points that are present in fewer views than specified by \code{min.views} will be assigned NA values in the returned 3D matrix (\code{coor.3d}).

After 3D reconstruction, \code{dltReconstruct()} performs the inverse operation, taking the reconstructed, 3D coordinates and solving for the 2D position of the points in each camera view. These inverse 2D coordinates are compared with the original coordinates and their difference is returned as the root-mean-square (RMS) reconstruction error (\code{list$rmse}). This error is similar to the epipolar distance (\code{\link{dltEpipolarDistance}}). The \code{summary()} function can be used to view the error by landmark.

Currently, \code{dltReconstruct()} only works with the 11-parameter DLT model.

For a step-by-step tutorial on how to use \code{dltReconstruct()} see \href{http://stereomorph.blogspot.com/2014/04/reconstructing-2d-points-and-curves.html}{Reconstructing 2D points and curves into 3D}.
}
\value{
a list of class \code{"dltReconstruct"} with the following elements:
  \item{coor.3d}{a 2D or 3D landmark matrix.}
  \item{rmse}{the root-mean-square reconstruction error.}
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
\code{\link{dltCalibrateCameras}},
\code{\link{readLandmarksToMatrix}},
\code{\link{readLandmarksToList}},

\code{\link{readLandmarksToArray}},
\code{\link{dltEpipolarDistance}}
}
\examples{
## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET FILE PATH TO CALIBRATION COEFFICIENTS IN TWO CAMERA STEREO SETUP
cc_file <- paste0(fdir, "cal_coeffs.txt")

## LOAD COEFFICIENTS
cal.coeff <- as.matrix(read.table(file=cc_file))

## GET LANDMARKS IN FIRST CAMERA VIEW
lm_files <- paste0(fdir, c("lm_2d_a1_v1.txt", "lm_2d_a1_v2.txt"))

## READ LANDMARKS INTO MATRIX
lm.matrix <- readLandmarksToMatrix(file=lm_files, row.names=1)

## RECONSTRUCT LANDMARKS IN 3D (MATRIX INPUT)
dlt_recon <- dltReconstruct(cal.coeff=cal.coeff, coor.2d=lm.matrix)


## OTHER POSSIBLE LANDMARK FORMAT INPUTS ##
## READ LANDMARKS INTO LIST
lm.list <- readLandmarksToList(file=lm_files, row.names=1)

## RECONSTRUCT LANDMARKS IN 3D (LIST INPUT)
dlt_recon <- dltReconstruct(cal.coeff=cal.coeff, coor.2d=lm.list)

## READ LANDMARKS INTO ARRAY
lm.array <- readLandmarksToArray(file=lm_files, row.names=1)

## RECONSTRUCT LANDMARKS IN 3D (ARRAY INPUT)
dlt_recon <- dltReconstruct(cal.coeff=cal.coeff, coor.2d=lm.array)
}
\keyword{ DLT }