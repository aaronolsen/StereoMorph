\name{findCheckerboardCorners}
\alias{findCheckerboardCorners}
\title{Finds internal corners of a checkerboard pattern}
\description{
This function finds the internal corners of a checkerboard pattern in an image.
}
\usage{
findCheckerboardCorners(image.file, nx, ny, corner.file=NULL, verify.file=NULL, 
                        perim.min = 140, perim.max = NULL, dilations.min = 0,
                        dilations.max = 7, quad.approx.thresh = NULL,
                        print.progress=TRUE, verbose=FALSE, debug = FALSE)
}
\arguments{
  \item{image.file}{File path(s) to image(s) or to folder(s) containing image(s) (and only images). The image(s) should be a JPEG and include a checkerboard pattern. Can be a vector or matrix. Many different inputs accepted, see "Examples".}
  \item{nx}{The number of internal corners in the checkerboard along one dimension. Note that this is not the number of squares (see "Details").}
  \item{ny}{The number of internal corners in the checkerboard along a second dimension.}
  \item{corner.file}{File path(s) to text file(s) or to folder(s) where the corners should be saved. Can be a vector or matrix. If \code{NULL}, corners are not saved to a text file. Many different inputs accepted, see "Examples".}
  \item{verify.file}{File path(s) to JPEG image(s) or to folder(s) where verification images should be saved. Can be a vector or matrix. If \code{NULL}, verification images are not created. Many different inputs accepted, see "Examples".}
  \item{perim.min}{The minimum expected perimeter of a black square in the checkerboard pattern (in pixels).}
  \item{perim.max}{The maximum expected perimeter of a black square in the checkerboard pattern (in pixels).}
  \item{dilations.min}{The initial number of dilations to perform on the image. See "Details".}
  \item{dilations.max}{The maximum number of dilations to perform on the image. If equal to \code{dilations.min}, the function will only perform one dilation. See "details".}
  \item{quad.approx.thresh}{A threshold for the perimeter of black squares in which method to use to approximate the shape as a quadrangle.}
  \item{print.progress}{Logical indicating whether the function progress should be printed to the console. See \code{verbose}.}
  \item{verbose}{Logical indicating whether more detailed progress reports to the console. If \code{verbose} is \code{FALSE}, only the image name and whether the corners were found successfully are printed. If \code{verbose} is \code{TRUE}, the outcome of the corner search at the conclusion of each dilation is also printed.}
  \item{debug}{Logical indicating whether images should be created at each of several steps in the corner search. These will be written to the same location as the images written to \code{verify.file}. If \code{debug} is \code{TRUE}, \code{verify.file} must be defined. Additionally, \code{dilations.min} and \code{dilations.max} should be identical since debugging images are created at each dilation and will be overwritten if a range of dilations is input.}
}
\details{
This function automatically detects checkerboard corners in an image and returns the pixel coordinates of the internal corners (where the corners of the black squares contact other black squares) to subpixel resolution. The function uses several C++ functions for image processing written by the author and compiled with the StereoMorph package but hidden until documentation can be written for more general use. Currently the function only works with JPEG images (.jpg or .jpeg); this is the most common digital camera image format output. For large images (10-20 MB), the function can take from 5-15 seconds per image.

\code{image.file} input to the function can be of several different forms. First, it can be file paths to particular images or file paths to a folder or folders containing images. Secondly, it can be in a vector or matrix format. The format of \code{image.file} will dictate the structure of the value returned by the function. If a single image file is input, a two-colum matrix of corners (where the two columns correspond to the x, y pixel coordinates) is returned. If the input is a vector of file paths or folders containing images, a three-dimensional array is returned; the first two dimensions are the rows and columns of each corner matrix and the third dimension is the order of the corresponding image files in \code{image.file}. If the input is a matrix of file paths or folders containing images, a four-dimensional array is returned; the first two dimensions are the rows and columns of each corner matrix and the third and fourth dimensions are the positions of the corresponding image files in the \code{image.file} matrix. If \code{image.file} is a folder or folders containing images, the folders cannot contain any other files.

The inputs \code{corner.file} and \code{verify.file} are optional but if they are non-\code{NULL}, they should be of the same format as \code{image.file}. If \code{image.file} is a folder or folders containing images, folders can also be input for \code{corner.file} and \code{verify.file}. In this case, the function will automatically name the corner files and verify image files with the same names as the images and as text files and JPEG files, respectively. The corners are saved to a text file as a two column matrix without a header or row names.

For every input image, the function begins by reading in the image (using \code{readJPEG()} of the 'jpeg' package). For large images this is one of the most time-consuming steps. The image is converted to grayscale using the internal function \code{rgbToGray()}. The image is thresholded to create a binary image (black and white) based on an adaptive threshold. The threshold is created using the internal function \code{meanBlurImage()} and the image thresholded with the internal function \code{thresholdImageMatrix()}. Morphological closing is performed to reduce noise using the internal functions \code{dilateImage()} and \code{erodeImage()}.

The function then proceeds to dilate the image (expand white areas and consolidate black areas) using a 3x3 square kernel for the range specified by \code{dilations.min} and \code{dilations.max}. This separates the black squares from each other so that their perimeters can be detected as separate contours. For each dilation, all edge points are identified (black pixels with a neighboring white pixel and vice versa) using the internal function \code{findBoundaryPoints()}. Contours (connected edge points) are identified by the internal function \code{generateQuads()}, retaining only contours that are quadrangles. The midpoints between adjoining corners of all the quads are found using the internal function \code{intCornersFromQuads()}; among these will be the full set of internal corners.

If the initial set of internal corners exceeds the expectation, the internal corners are filtered, fitting a line to the internal corner set and removing the points at the furthest difference from the line of best fit until the number of corners matches the expectation. The filtered internal corner set is then ordered using the internal function \code{orderCorners()} so that first corner is the top left most corner in the pattern and the sequence of internal corners proceeds along \code{nx} first and \code{ny} second. Lastly, the function finds the internal corner positions to subpixel resolution (using the internal function \code{findCornerSubPix()}) by sampling a 23x23 pixel window around the approximate location of the internal corners to find a point optimally positioned at the intersection of diagonally opposing white and black squares. It is the sampling of this large image region (529 pixels) that allows the function to return the corner position to subpixel resolution.

If \code{verify.file} is non-\code{NULL}, the internal corners are overlayed on the input image to verify that the correct corners have been found and in the correct order. The first corner is circled in red, a green line interconnects all the intermediate corners in sequence and the last corner is circled in blue (the order of colors then being RGB).
}
\value{An array of the pixel coordinates of internal corners to subpixel resolution in an array of two (one checkerboard input), three (if \code{image.file} is a vector) or four dimensions (if \code{image.file} is a matrix). For images in which the expected number of internal corners were not found, an \code{NA} matrix is returned for those particular images. The corners are returned along the \code{nx} dimension first and the \code{ny} dimension second.}
\references{
This function was written based on the methodology described in 'Learning OpenCV' for the automated detection of internal checkerboard corners (Bradski and Kaehler 2008).
}
\author{Aaron Olsen}
\seealso{
\code{\link{readCheckerboardsToArray}}, \code{\link{measureCheckerboardSize}}
}
\examples{

## GET THE FILE DIRECTORY FOR EXTRA R PACKAGE FILES
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

## FIND 5 X 3 INTERNAL CORNERS IN A SINGLE IMAGE
corners <- findCheckerboardCorners(image.file=paste0(fdir, 
     "Checkerboards/RUlna.JPG"), nx=5, ny=3)

## FIND 5 X 3 INTERNAL CORNERS IN ALL IMAGES IN A FOLDER (HERE 3)
corners <- findCheckerboardCorners(image.file=paste0(fdir, 
     "Checkerboards"), nx=5, ny=3)

## WHICH DIMENSIONS ARE ASSIGNED TO NX AND NY IS ARBITRARY BUT REVERSING
## THESE WILL CHANGE THE SEQUENCE IN WHICH THE CORNERS ARE RETURNED
corners <- findCheckerboardCorners(image.file=paste0(fdir, 
     "Checkerboards/RUlna.JPG"), nx=3, ny=5)
}
\keyword{ grid functions }
\keyword{ calibration }