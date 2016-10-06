\name{alignLandmarksToMidline}
\alias{alignLandmarksToMidline}
\alias{summary.alignLandmarksToMidline}
\alias{print.summary.alignLandmarksToMidline}
\title{Aligns bilateral landmarks to the midline plane}
\description{
This function aligns a set of bilateral landmarks to the midline plane. Midline landmarks and the mean position of bilateral landmarks are both used to define the midline plane.
}
\usage{	
alignLandmarksToMidline(lm.matrix, left = '(_l[_]?|_left[_]?)([0-9]*$)', 
                        right = '(_r[_]?|_right[_]?)([0-9]*$)',
                        left.remove = '\\\2', right.remove = '\\\2', 
                        average = FALSE)

\method{summary}{alignLandmarksToMidline}(object, ...)
}
\arguments{
  \item{lm.matrix}{a 2D or 3D matrix with landmark names as row names.}
  \item{left}{a regular expression to identify left landmarks in the row names of \code{lm.matrix}.}
  \item{right}{a regular expression to identify right landmarks in the row names of \code{lm.matrix}.}
  \item{left.remove}{an expression for input to the \code{gsub()} function indicating which element of \code{left} in parentheses should be removed to create a landmark name that is not side-specific (see "Details").}
  \item{right.remove}{an expression for input to the \code{gsub()} function indicating which element of \code{right} in parentheses should be removed to create a landmark name that is not side-specific (see "Details").}
  \item{average}{Whether to average left and right landmarks after alignment. Can be left false if landmarks previously reflected with \code{average} equal to \code{TRUE} (see \code{\link{reflectMissingLandmarks}}).}
  \item{object}{a list of class \code{"alignLandmarksToMidline"} (output of this function).}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
Currently, the function only accepts left/right designations by matching a regular expression to the landmark name. This is preferable in that it allows for easier match up between bilateral landmarks (based on their common name without a side annotation). The default regular expression identifies left landmarks by a name ending in "_L", "_l", "_left" or "_LEFT", optionally followed by numbers. For example, "hamulus_left", "hamulus_L" and "zymgomatic_arch_l012" would all be identified as landmarks on the left side. Similarly, "hamulus_right", "hamulus_R" and "zymgomatic_arch_r012" would all be identified as landmarks on the right side. Landmarks not identified as left or right are assumed to fall on the midline.

In order to find corresponding left and right landmarks, the function requires the \code{left.remove} and \code{right.remove} arguments. The \code{left.remove} and \code{right.remove} arguments are passed to the base function \code{gsub()} as the \code{replacement} argument. This is used to generate a landmark name that is not side-specific. For example, "hamulus_left" and "zymgomatic_arch_l012" would become "hamulus" and "zymgomatic_arch012". These will be reverted to their original names at return.

Once corresponding right and left landmarks have been identified, the function finds the mean positions of all bilateral landmarks and the positions of all midline landmarks. These points are used to define the midline. After alignment, the specimen will have the midline axis as the last column (z in 3D, y in 2D), the longest non-midline axis as the first column (x in 3D), and the second non-midline axis as y for 3D. No further rotation and reflection is done, therefore the specimen may be facing any direction along each of the axes.

This function returns the aligned landmarks and an error vector, \code{midline.error}. This is a vector of the squared z-coordinate of the midline landmarks (the distance between each midline landmark and the midline plane). If \code{\link{reflectMissingLandmarks}} was called on the landmarks prior to \code{alignLandmarksToMidline()} with \code{average} equal to \code{TRUE}, then all of the midline points will fall exactly along the midline. Thus, the error vector will consist entirely of zeros (or near-zero values).
}
\value{
a list of class \code{"alignLandmarksToMidline"} with the following elements:
  \item{lm.matrix}{a 2D or 3D matrix of landmarks aligned to the midline.}
  \item{midline.error}{a vector of the errors (distances) between each midline landmark and the midline plane.}
}
\author{Annat Haber, Aaron Olsen}
\note{This function was modified by A Olsen from the R function \code{AMP()} written by A Haber.}
\seealso{\code{\link{readLandmarksToMatrix}}}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## GET LANDMARKS
file <- paste0(fdir, "lm_3d_unify.txt")

## LOAD FILES INTO A MATRIX
lm.matrix <- readLandmarksToMatrix(file=file, row.names=1)

## ALIGN TO MIDLINE
align_landmarks <- alignLandmarksToMidline(lm.matrix=lm.matrix)

## PRINT SUMMARY OF ERRORS
print(summary(align_landmarks))
}