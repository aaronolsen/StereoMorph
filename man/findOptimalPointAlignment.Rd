\name{findOptimalPointAlignment}
\alias{findOptimalPointAlignment}
\title{Optimally aligns one point set to another}
\description{
This function translates and rotates one point set, optimally aligning it with another point set.
}
\usage{
findOptimalPointAlignment(m1, m2)
}
\arguments{
  \item{m1}{a point set matrix}
  \item{m2}{a second point set matrix of the same dimensions as \code{m1}}
}
\details{
This function optimally aligns point set \code{m2} with point set \code{m1}. \code{m1} and \code{m2} must contain the exact same landmarks or points in the same order. Points present in \code{m2} but not \code{m1} should be \code{NA} in \code{m1}. They do not need to be \code{NA} in \code{m2}; all translations and rotations will be applied to all points in \code{m2} even though only shared points will be used in the alignment.

The function first centers the centroid \code{m2} about the centroid of \code{m1}. The function \code{svd()} is then used to find the 3D rotation matrix that optimally aligns \code{m2} to \code{m1} based on common points. The positions of points in \code{m2} relative to one another are unchanged. Thus, optimal rotation is constrained to already translated point sets. Depending on the point sets, a better alignment may be possible by allowing translation and rotation to be optimized simultaneously.

This function is called by \code{\link{unifyLandmarks}} to align landmark sets and by \code{\link{dltTestCalibration}} to test accuracy in reconstructed calibration grids.
}
\value{\code{m2} after alignment.}
\references{
Rohlf, F.J. (1990) "Chapter 10. Rotational fit (Procrustes) Methods." \emph{Proceedings of the Michigan Morphometrics Workshop}. Ed. F. James Rohlf and Fred L. Bookstein. The University of Michigan Museum of Zoology, 1990. 227--236. \href{http://deepblue.lib.umich.edu/handle/2027.42/49535}{Info page at lib.umich.edu}
}
\author{Annat Haber, Aaron Olsen}
\note{Modified from \code{unifyVD()} by Annat Haber.}
\seealso{\code{\link{unifyLandmarks}}}
\examples{
## MAKE MATRIX OF 3D POINTS
m1 <- matrix(c(0,0,0, 1,3,2, 4,2,1, 5,5,3, 1,4,2, 3,6,4), nrow=6, ncol=3)

## COPY TO M2
m2 <- m1

## MAKE MISSING POINT IN M1
## ALTHOUGH NOT USED IN THE ALIGNMENT THE CORRESPONDING POINT
##  IN M2 IS STILL RETURNED AFTER ALIGNMENT
m1[3, ] <- NA

## CENTER M2 ABOUT CE
m2 <- m2 \%*\% rotationMatrixZYX(pi/6, -pi/3, pi/8)

## TRANSLATE M2
m2 <- m2 + matrix(c(2,3,4), nrow=6, ncol=3, byrow=TRUE)

## ALIGN M2 TO M1
m3 <- findOptimalPointAlignment(m1, m2)

## NOTE THAT RETURNED MATRIX IS IDENTICAL TO M1
## OF COURSE REAL WORLD DATA WILL HAVE SOME ERROR
m1
m3
}