\name{drawCheckerboard}
\alias{drawCheckerboard}
\title{Creates a checkerboard image}
\description{
Creates a checkerboard image of specified dimensions and saves to an input file path. The dimensions of the checkerboard are specified by the number of internal corners (the number of squares minus one).
}
\usage{
drawCheckerboard(nx, ny, square.size, filename,
                 margin.x = c(round(square.size/2), round(square.size/2)),
                 margin.y = c(round(square.size/2), round(square.size/2)),
                 ...)
}
\arguments{
  \item{nx}{the number of internal corners in the horizontal direction (the number of squares in each row minus one).}
  \item{ny}{the number of internal corners in the vertical direction (the number of squares in each column minus one).}
  \item{square.size}{the square size in pixels.}
  \item{filename}{the file path and name to which the image should be saved. The filename must be a valid image filename. Acceptable extensions are: jpg, jpeg, bmp, png and tiff.}
  \item{margin.x}{the margin in pixels on the left and right sides of the checkerboard pattern.}
  \item{margin.y}{the margin in pixels on the top and bottom of the checkerboard pattern.}
  \item{...}{further arguments to be passed to the image function corresponding to the extension in \code{filename} (e.g. \code{compression}, \code{quality}, etc.).}
}
\details{
This function requires the \code{grid} package. The image type is determined automatically from the filename and the corresponding image writing function is called.

For a step-by-step tutorial on how to use \code{drawCheckerboard()}, see \href{http://stereomorph.blogspot.com/2014/04/creating-checkerboard-pattern.html}{Creating a checkerboard pattern}.
}
\value{returns null device}
\author{Aaron Olsen}
\seealso{
\code{\link{readCheckerboardsToArray}}
}
\examples{
## NUMBER OF INTERNAL CORNERS IN THE HORIZONTAL DIMENSION
## NUMBER OF ROWS OF SQUARES MINUS ONE
nx <- 21

## NUMBER OF INTERNAL CORNERS IN THE VERTICAL DIMENSION
## NUMBER OF COLUMNS OF SQUARES MINUS ONE
ny <- 14

## SQUARE SIZE IN PIXELS
square.size <- 200

## WHERE TO SAVE THE FILE
filename <- paste0("checkerboard_", nx, "_", ny, "_", square.size, ".jpeg")

\dontrun{
## DRAW CHECKERBOARD
## FILE WILL BE CREATED IN CURRENT WORKING DIRECTORY
drawCheckerboard(nx=nx, ny=ny, square.size=square.size, filename=filename)
}
}
\keyword{ grid functions }