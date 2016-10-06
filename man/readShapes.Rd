\name{readShapes}
\alias{readShapes}
\alias{print.shapes}
\title{Reads a StereoMorph shape file}
\description{
This function reads digitized shape and scaling data from a StereoMorph shape file or files into a list structure.
}
\usage{
readShapes(file, fields=NULL)
}
\arguments{
  \item{file}{A shape file, a vector of shape files or a folder containing shape files to be read.}
  \item{fields}{Objects to be returned from the shape file. If \code{NULL}, all objects in the file will be returned.}
}
\details{
The \code{\link{digitizeImage}} function makes it possible to save shape and scaling data into a single shape file. This shape file has an XML-like format that allows the \code{readShapes()} function to read multiple object types (including vectors, matrices and lists) into a list structure using a generalized routine. All these objects are saved to a list as several elements. The particular elements in the output list will depend on which objects are present in the file. If the object is not present in the file, a call to that object will return NULL. The contents will also differ if multiple files are input. For instance, if one file is input \code{landmarks.pixel} will be a matrix but if multiple files are input it will be an array.

The output of \code{print()} on the entire output list is formatted for readability given the potentially large matrices contained within the list.
}
\value{
a list of class \code{"shapes"} containing any number of the following elements:
  \item{image.name}{A vector of image names.}
  \item{image.id}{A vector of image IDs.}
  \item{scaling}{A vector of the scaling (real-world units per pixel) of the image.}
  \item{scaling.units}{A vector of the units of \code{scaling}.}
  \item{ruler.pixel}{A vector of the interval of the digitized ruler points in pixels.}
  \item{ruler.interval}{A vector of the interval of the digitized ruler points in real-world units.}
  \item{checkerboard.nx}{A vector of the number of internal corners of a checkerboard pattern along one dimension.}
  \item{checkerboard.ny}{A vector of the number of internal corners of a checkerboard pattern along the other dimension.}
  \item{square.pixel}{A vector of the best-fit checkerboard square size in pixels.}
  \item{square.size}{A vector of the best-fit checkerboard square size in real-world units.}
  \item{landmarks.pixel}{A matrix or array of landmark coordinates in pixels.}
  \item{landmarks.scaled}{A matrix or array of scaled landmark coordinates.}
  \item{ruler.points}{A matrix or array of ruler points in pixels.}
  \item{checker.pixel}{A matrix or array of checkerboard points in pixels.}
  \item{curves.control}{A list of Bezier curve control points in pixels.}
  \item{curves.pixel}{A list of Bezier curve points in pixels.}
  \item{curves.scaled}{A list of scaled Bezier curve points.}

 If any of the above objects are absent from the shape file they will be \code{NULL}.
}
\author{Aaron Olsen}
\seealso{
\code{\link{digitizeImage}}
}