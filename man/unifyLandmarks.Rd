\name{unifyLandmarks}
\alias{unifyLandmarks}
\alias{summary.unifyLandmarks}
\alias{print.summary.unifyLandmarks}
\title{Optimally align a set of partial landmark sets}
\description{
This function aligns two or more landmark sets using shared points. Corresponding landmarks are identified by matching row names. The function selects a sequence of alignments that minimizes the step-wise alignment error.
}
\usage{
unifyLandmarks(lm.array, min.common = dim(lm.array)[2], return.on.error = FALSE)

\method{summary}{unifyLandmarks}(object, print.tab = '', verbose = TRUE, ...)
}
\arguments{
  \item{lm.array}{an array of 2D or 3D landmark matrices. These can be read in from a file or files using \code{\link{readLandmarksToArray}}.}
  \item{min.common}{a minimum number of landmarks to use in the alignment. Must be greater than \code{dim(lm.array)[2]}.}
  \item{return.on.error}{Logical whether to return \code{NULL} if there are an insufficient common points for unification.}
  \item{object}{a list of class \code{"unifyLandmarks"} (the output of \code{unifyLandmarks()}).}
  \item{print.tab}{Tabs preceding lines printed to console.}
  \item{verbose}{Logical whether to print full error report.}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
The input \code{lm.array} should be an array of 2D or 3D landmark matrices with row names, such as created by \code{\link{readLandmarksToArray}}. The first two dimensions of \code{lm.array} correspond to the rows and columns of each matrix, respectively. The last dimension of \code{lm.array} corresponds to each separate landmark matrix.

\code{unifyLandmarks()} first aligns all pair combinations of landmark sets that share the minimum number of points specified by \code{min.common}. The two sets that align with the lowest root-mean-square (RMS) error are aligned and the mean positions of all points saved. If there are additional landmark sets, \code{unifyLandmarks()} aligns each of these with the combined matrix, again identifying the set that aligns with the least RMS error. The alignment with the least error is saved as the new combined landmark matrix. This is repeated for each remaining landmark set, sequentially aligning remaining landmark sets to the combined landmark matrix.

To align two 2D landmark sets, the sets must share at least two landmarks and to align two 3D landmark sets, the sets must share at least three landmarks. These are the default minimum number of points for alignment. A greater number of common points can be specified using the \code{min.common} parameter. Additionally, in the 3D case, these landmarks must not be collinear. If \code{lm.array} contains more than two landmark matrices, it is not necessarily required that each landmark set share these minimum number of points with every other landmark set. For example, it may be that two landmark sets do not each separately share the minimum number of landmarks required for alignment with a third landmark set. But if these two landmark sets are combined, they may then share the required number of landmarks with the third set. During each alignment step, \code{unifyLandmarks()} skips pairs of matrices that do not share the required number of landmarks. As long as there is some combination of alignments that provide a sufficient number of shared landmarks, all landmark sets can be combined into a single matrix.

If an array consisting of only one landmark matrix is input, the matrix is returned without an alignment operation.
}
\value{
a list of class \code{"unifyLandmarks"} with the following elements:
  \item{lm.matrix}{a 2D or 3D landmark matrix.}
  \item{unify.seq}{a vector of the order in which landmark sets were aligned.}
  \item{unify.error}{a matrix of the alignment error for each shared landmark for each alignment (the number of sets minus one).}
  \item{unify.rmse}{a vector of the root-mean-square error of each alignment (the number of sets minus one).}
}
\references{
Rohlf, F.J. (1990) "Chapter 10. Rotational fit (Procrustes) Methods." \emph{Proceedings of the Michigan Morphometrics Workshop}. Ed. F. James Rohlf and Fred L. Bookstein. The University of Michigan Museum of Zoology, 1990. 227--236. \href{http://deepblue.lib.umich.edu/handle/2027.42/49535}{Info page at lib.umich.edu}
}
\author{Annat Haber, Aaron Olsen}
\note{This function was modified by A Olsen from the R function \code{unifyVD()} written by A Haber.}
\seealso{
\code{\link{findOptimalPointAlignment}}
}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## SET LANDMARK FILES
file <- paste0(fdir, "lm_3d_even_a", 1:3, ".txt")

## READ LANDMARKS INTO ARRAY
lm.array <- readLandmarksToArray(file, row.names=1)

## UNIFY LANDMARKS
unify_lm <- unifyLandmarks(lm.array)

## PRINT UNIFICATION SUMMARY
print(summary(unify_lm))
}
\keyword{ landmark alignment }