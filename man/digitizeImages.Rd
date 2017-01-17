\name{digitizeImages}
\alias{digitizeImages}
\title{Opens the StereoMorph Digitizing App}
\description{This function opens an application in the user's default web browser for manually digitizing landmarks and Bezier curves from photographs.}
\usage{	
digitizeImages(image.file, shapes.file=NULL, landmarks.file=NULL, 
     control.points.file=NULL, curve.points.file=NULL, 
     cal.file=NULL, landmarks.ref=NULL, curves.ref=NULL, image.id=NULL, 
     landmark.color.blur = 'blue', landmark.color.focus = 'green', 
     curve.color.blur = 'purple', control.point.color.blur = 'purple', 
     control.point.color.focus = 'red', landmark.radius = 4, 
     control.point.radius = 4, marker.stroke.width = 2, app.dir=NULL)
}
\arguments{
  \item{image.file}{file path to the image or images to be digitized. This can be a folder containing one or more images or a vector of file paths of one or more images.}
  \item{shapes.file}{file path or folder indicating where shape files should be saved. This is a new format that is currently only intended to be used when collecting 2D data. The other input types (\code{landmarks.file}, \code{control.points.file}, \code{curve.points.file}) will be phased out in future updates and replaced with this format.}
  \item{landmarks.file}{same input as \code{landmarks.file} in \code{\link{digitizeImage}}. Included for backward compatibility. This will eventually be phased out.}
  \item{control.points.file}{same input as \code{control.points.file} in \code{\link{digitizeImage}}. Included for backward compatibility. This will eventually be phased out.}
  \item{curve.points.file}{same input as \code{curve.points.file} in \code{\link{digitizeImage}}. Included for backward compatibility. This will eventually be phased out.}
  \item{cal.file}{file path to calibration file created by \code{\link{calibrateCameras}}.}
  \item{landmarks.ref}{landmarks to be digitized. This can either be a file path to a .txt file containing the landmarks (listed in a single column, each on a separate line) or a vector of landmark names.}
  \item{curves.ref}{curves to be digitized. For each curve, the name of the curve, the starting point and the ending point must be specified. \code{curves.ref} can either be a three-column matrix (with the curve name in the first column, the starting point in the second and the end point in the third) or a file path to a .txt file containing a three-column curve reference matrix. If a file path, the file should have no header and tab-separated row values.}
  \item{image.id}{image IDs to be saved with each image. These will be used to reference shape data in the output of \code{\link{readShapes}}. If \code{NULL}, the filenames of the images will be used (without the file extension).}
  \item{landmark.color.blur}{color of an unselected landmark. It might be necessary to change if the background color is close to the default. Colors must be valid SVG color names or codes (e.g. "hotpink", "#4B0082", etc.). A web-search for "SVG color codes" will indicate several possible options.}
  \item{landmark.color.focus}{color of a selected landmark. See \code{landmark.color.blur}.}
  \item{curve.color.blur}{color of digitized curves. A different color for a selected curve is not yet supported. See \code{landmark.color.blur}.}
  \item{control.point.color.blur}{color of an unselected control point. See \code{landmark.color.blur}.}
  \item{control.point.color.focus}{color of a selected control point. See \code{landmark.color.blur}.}
  \item{landmark.radius}{radius of digitized landmarks.}
  \item{control.point.radius}{radius of the Bezier control points.}
  \item{marker.stroke.width}{thickness of the lines used to draw the landmarks and control points.}
  \item{app.dir}{changes the shiny app directory for debugging.}
}
\details{
This function opens a digitizing app in the user's default browswer and allows for the digitization of landmarks and Bezier curves from photographs. Although the app runs in a web browser, the user does not have to be connected to the internet as the app runs on a local server. The R package 'shiny' handles the communication between the browser and the R console. Safari, Chrome and Opera all provide full compatibility with the apps's features. Please see \href{https://aaronolsen.github.io/software/stereomorph.html}{StereoMorph tutorials} for step-by-step tutorials on how to use \strong{StereoMorph} for 2D or 3D shape data collection.
}
\value{
NULL
}
\author{Aaron Olsen}
\seealso{
\code{\link{readShapes}}
}
\keyword{ digitizing }