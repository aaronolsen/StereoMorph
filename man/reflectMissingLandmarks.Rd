\name{reflectMissingLandmarks}
\alias{reflectMissingLandmarks}
\alias{summary.reflectMissingLandmarks}
\alias{print.summary.reflectMissingLandmarks}
\title{Reflects missing landmarks across the plane of symmetry}
\description{
This function reflects missing bilateral landmarks across the plane of symmetry, optionally averaging left and right landmarks.}
\usage{	
reflectMissingLandmarks(lm.matrix, left = '(_L|_l|_left|_LEFT)([0-9]*$)', 
                        right = '(_R|_r|_right|_RIGHT)([0-9]*$)',
                        left.remove = '\\\2', right.remove = '\\\2', 
                        left.replace = '_R\\\2', right.replace = '_L\\\2', 
                        average = FALSE)

\method{summary}{reflectMissingLandmarks}(object, ...)
}
\arguments{
  \item{lm.matrix}{a 2D or 3D matrix with landmark names as row names.}
  \item{left}{a regular expression to identify left landmarks in the row names of \code{lm.matrix}.}
  \item{right}{a regular expression to identify right landmarks in the row names of \code{lm.matrix}.}
  \item{left.remove}{an expression for input to the \code{gsub()} function indicating which element of \code{left} in parentheses should be removed to create a landmark name that is not side-specific (see "Details").}
  \item{right.remove}{an expression for input to the \code{gsub()} function indicating which element of \code{right} in parentheses should be removed to create a landmark name that is not side-specific (see "Details").}
  \item{left.replace}{an expression for input to the \code{gsub()} function indicating a replacement string for \code{left} that will turn a left landmark name into a right landmark name (see "Details").}
  \item{right.replace}{an expression for input to the \code{gsub()} function indicating a replacement string for \code{right} that will turn a right landmark name into a left landmark name (see "Details").}
  \item{average}{a logical indicating whether bilateral landmarks should be averaged.}
  \item{object}{a list of class \code{"reflectMissingLandmarks"} (output of this function).}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
Currently, the function only accepts left/right designations by matching a regular expression to the row names of \code{lm.matrix}. This is preferable since it allows for easier match up between bilateral landmarks. The default regular expression identifies left landmarks by a name ending in "_L", "_l", "_left" or "_LEFT", optionally followed by numbers. For example, "hamulus_left", "hamulus_L" and "zymgomatic_arch_l012" would all be identified as landmarks on the left side. Similarly, "hamulus_right", "hamulus_R" and "zymgomatic_arch_r012" would all be identified as landmarks on the right side. Landmarks not identified as left or right are assumed to fall on the midline.

In order to find corresponding left and right landmarks, the function requires the \code{left.remove} and \code{right.remove} arguments. The \code{left.remove} and \code{right.remove} arguments are passed to the base function \code{gsub()} as the \code{replacement} argument. This is used to generate a landmark name that is not side-specific. For example, "hamulus_left" and "zymgomatic_arch_l012" would become "hamulus" and "zymgomatic_arch012". These will be reverted to their original names at return.

If only a left or right landmark is present in \code{lm.matrix}, \code{reflectMissingLandmarks()} will create new a new row in \code{lm.matrix} for the missing, contralateral landmark. Thus, the output matrix could be longer than the input matrix. The arguments \code{left.replace} and \code{right.replace} are used to create these new rownames by converting landmark names from left to right or vice versa. By default, the function replaces the existing side designation with "_L" and "_R". For instance, "hamulus_left" and "zymgomatic_arch_L012" would become "hamulus_R" and "zymgomatic_arch_R012", respectively. None of the names of existing landmarks will be modified. Users wanting a different left/right scheme can either change the \code{left.replace} and \code{right.replace} arguments or make sure that all the bilateral landmarks in \code{lm.matrix} are represented by both a left and right landmark (missing values being \code{NA}). In this case, \code{left.replace} and \code{right.replace} will be ignored and no new landmark names will be created.

Once corresponding right and left landmarks have been identified, the plane of object symmetry is found as described by Klingenberg et al. (2002). This includes creating two landmark sets, reflecting one set across the xy-plane, swapping left and right landmark names in one set and performing Procrustes alignment on the two sets. The user then has the option of averaging across the plane of symmetry. This will cause all bilateral landmarks to be mirror images across the midline plane and midline landmarks to lie directly in the midline plane. The input orientation of \code{lm.matrix} is maintained. So if \code{average} is \code{FALSE}, landmarks that were not missing will be unchanged at output (the new landmarks having been filled in around them). If \code{average} is \code{TRUE}, the positions of the non-missing landmarks will have changed due to averaging but will only be shifted slightly from the original position.

\code{reflectMissingLandmarks()} returns an alignment error vector. This is the error (distance) between a left or right landmark and its contralateral landmark (if present) when reflected across the midline plane. This is equivalent to the Procrustes alignment error.

Users with landmark names in alternative formats might find it easier to simply add '_L' and '_R' to the end of left and right landmark names, respectively, rather than re-specifying the regular expression arguments.

For a step-by-step tutorial on how to use \code{reflectMissingLandmarks()} see \href{http://stereomorph.blogspot.com/2014/04/unifying-reflecting-and-aligning.html}{Unifying, reflecting and aligning landmarks}.
}
\value{
a list of class \code{"reflectMissingLandmarks"} with the following elements:
  \item{lm.matrix}{a 2D or 3D matrix of landmarks with missing landmarks reflected. This matrix could be longer than the input landmark matrix.}
  \item{align.error}{a vector of the error (distance) between between a left or right landmark and its contralateral landmark (if present) when reflected across the plane of symmetry.}
}
\references{
Klingenberg, C.P., Barluengua, M., Meyer, A. (2002) Shape analysis of symmetric structures: Quantifying variation among individuals and asymmetry. \emph{Evolution}, \bold{56} (10), 1909--1920.
}
\author{Annat Haber, Aaron Olsen}
\note{This function was modified by A Olsen from the R function \code{OSymm()} written by A Haber.}
\seealso{\code{\link{readLandmarksToMatrix}}, \code{\link{alignLandmarksToMidline}}}
\examples{
## FIND THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## GET LANDMARKS
file <- paste0(fdir, "lm_3d_unify.txt")

## LOAD FILES INTO A MATRIX
lm.matrix <- readLandmarksToMatrix(file=file, row.names=1)

## ALIGN TO MIDLINE
reflect_missing <- reflectMissingLandmarks(lm.matrix=lm.matrix, average=TRUE)

## PRINT SUMMARY OF ERRORS
print(summary(reflect_missing))
}