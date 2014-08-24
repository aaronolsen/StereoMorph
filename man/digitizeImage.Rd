\name{digitizeImage}
\alias{digitizeImage}
\title{Opens the StereoMorph Digitizing App}
\description{This function opens an application in the user's default web browser for manually digitizing landmarks and Bezier curves from photographs.}
\usage{	
digitizeImage(image.file, landmarks.file=NULL, control.points.file=NULL,
              curve.points.file=NULL, landmarks.ref=NULL, curves.ref=NULL, 
              auto.advance=TRUE, landmark.color.blur = 'blue', 
              landmark.color.focus = 'green', curve.color.blur = 'purple', 
              control.point.color.blur = 'purple', control.point.color.focus = 'red', 
              landmark.radius = 4, control.point.radius = 4, 
              marker.stroke.width = 1)
}
\arguments{
  \item{image.file}{a file path to the image or images to be digitized. This can be a folder containing one or more images or a vector of file paths of one or more images.}
  \item{landmarks.file}{a file path indicating where the landmarks should be loaded from and/or saved to. If \code{landmark.file} is a folder, then landmark files are assumed to have the same name as the images (with a .txt extension). Alternatively, this can be a vector of file paths to .txt files. If \code{landmarks.file} is a vector of file paths, the number of paths must equal the number of images to be digitized. If not collecting landmarks, this argument can be omitted.}
  \item{control.points.file}{a file path indicating where the Bezier control points should be loaded from and/or saved to. If \code{control.points.file} is a folder, then control point files are assumed to have the same name as the images (with a .txt extension). Alternatively, this can be a vector of file paths to .txt files. If \code{control.points.file} is a vector of file paths, the number of paths must equal the number of images to be digitized. If not collecting curves, this argument can be omitted.}
  \item{curve.points.file}{a file path indicating where the Bezier curve points should be saved to. If \code{curve.points.file} is a folder, then curve point files are assumed to have the same name as the images (with a .txt extension). Alternatively, this can be a vector of file paths to .txt files. If \code{curve.points.file} is a vector of file paths, the number of paths must equal the number of images to be digitized. If not collecting curve points, this argument can be omitted.}
  \item{landmarks.ref}{landmarks to be digitized. This can either be a file path to a .txt file containing the landmarks (listed in a single column, each on a separate line) or a vector of landmark names.}
  \item{curves.ref}{curves to be digitized. For each curve, the name of the curve, the starting point and the ending point must be specified. \code{curves.ref} can either be a three-column matrix (with the curve name in the first column, the starting point in the second and the end point in the third) or a file path to a .txt file containing a three-column curve reference matrix. If a file path, the file should have no header and tab-separated row values.}
  \item{auto.advance}{A logical indicating whether after digitizing a landmark or control point the app should automatically advance to the next marker. This can be turned on/off once the app is open by holding down "shift" and pressing "a".}
  \item{landmark.color.blur}{The color of an unselected landmark. It might be necessary to change if the background color is close to the default. Colors must be valid SVG color names or codes (e.g. "hotpink", "#4B0082", etc.). A web-search for "SVG color codes" will indicate several possible options.}
  \item{landmark.color.focus}{The color of a selected landmark. See \code{landmark.color.blur}.}
  \item{curve.color.blur}{The color of digitized curves. A different color for a selected curve is not yet supported. See \code{landmark.color.blur}.}
  \item{control.point.color.blur}{The color of an unselected control point. See \code{landmark.color.blur}.}
  \item{control.point.color.focus}{The color of a selected control point. See \code{landmark.color.blur}.}
  \item{landmark.radius}{The radius of digitized landmarks.}
  \item{control.point.radius}{The radius of the Bezier control points.}
  \item{marker.stroke.width}{The thickness of the lines used to draw the landmarks and control points.}
}
\details{
This function opens a digitizing app in the user's default browswer and allows for the digitization of landmarks and Bezier curves from photographs. Although the app runs in a web browser, the user does not have to be connected to the internet as the app runs on a local server. The R package 'shiny' handles the communication between the browser and the R console. Safari, Chrome and Opera all provide full compatibility with the apps's features.

One or more images can be uploaded to the app. If more than one image is uploaded (via \code{image.file}), users can switch between images within the app by clicking the "Previous Image" and "Next Image" buttons in the app control panel. The app can be used to digitize landmarks only, curves only or both landmarks and curves. To only digitize landmarks omit the arguments \code{control.points.file}, \code{curve.points.file} and \code{curves.ref}. To only digitize curves, omit the arguments \code{landmarks.file} and \code{landmarks.ref}. If the files input via \code{landmarks.file} and \code{control.points.file} already contain landmarks or control points, these will be loaded in the app and can be modified and re-saved.

The app can save both Bezier control points and Bezier curve points. The former are the points that can be added and moved by the user while digitizing; these are used to change the shape of the curve but are not the curve points themselves. The latter are hundreds of points at single-pixel spacing that actually describe the curve and are appropriate for subsequent shape analysis. These are generated by the \code{pointsOnBezier()} function in the R package 'bezier'. While it is necessary to specify \code{control.points.file} when digitizing curves, \code{curve.points.file} is optional.

For a step-by-step tutorial on how to open and use the StereoMorph Digitizing App and download the StereoMorph Tutorial folder, see \href{http://stereomorph.blogspot.com/2014/08/digitizing-photographs.html}{Digitizing with StereoMorph}. Additional instructions can be found by clicking on 'Help' in the right upper corner of the app (or viewing the help file \href{http://home.uchicago.edu/~aolsen/StereoMorph/help.html}{here}).
}
\value{
NULL
}
\author{Aaron Olsen}
\examples{
\dontrun{
## THE FOLLOWING EXAMPLES CAN BE RUN
## BUT USERS MUST FIRST DOWNLOAD THE STEREOMORPH TUTORIAL FOLDER
## SEE DETAILS ABOVE

## TO DIGITIZE ONLY LANDMARKS IN A SINGLE IMAGE
digitizeImage(
   image.file = "Object images/obj1_a1_v1.JPG",
   landmarks.file = "Landmarks 2D/obj1_a1_v1.txt",
   landmarks.ref = "landmarks_ref.txt")

## TO DIGITIZE ONLY LANDMARKS IN MULTIPLE IMAGES
digitizeImage(
   image.file = c("Object images/obj1_a1_v1.JPG", "Object images/obj1_a1_v1.JPG"),
   landmarks.file = c("Landmarks 2D/obj1_a1_v1.txt", "Landmarks 2D/obj1_a1_v1.txt"),
   landmarks.ref = "landmarks_ref.txt")

## OR ALTERNATIVELY, IF "OBJECT IMAGES" IS A FOLDER CONTAINING IMAGES
digitizeImage(
   image.file = "Object images",
   landmarks.file = "Landmarks 2D",
   landmarks.ref = "landmarks_ref.txt")

## DIGITIZING CURVES
digitizeImage(
   image.file = "Object images",
   control.points.file = "Control points",
   curve.points.file = "Curve points 2D",
   curves.ref = "curves_ref.txt")

## DIGITIZING LANDMARKS AND CURVES
digitizeImage(
   image.file = "Object images",
   landmarks.file = "Landmarks 2D",
   control.points.file = "Control points",
   curve.points.file = "Curve points 2D",
   landmarks.ref = "landmarks_ref.txt",
   curves.ref = "curves_ref.txt")
}
}
\keyword{ digitizing }