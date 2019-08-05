\name{TPSToShapes}
\alias{TPSToShapes}
\title{Converts TPS file to shape file}
\description{
Converts shape data in the TPS format into the StereoMorph shape file format, primarily for use with the StereoMorph digitizing application
}
\usage{
TPSToShapes(tps.file, shapes.file, image.file, landmark.names,
        spec.names = c("IMAGE"), scaling.units = NULL, flip.y = TRUE)
}
\arguments{
  \item{tps.file}{A TPS file.}
  \item{shapes.file}{A folder where the shape files will be saved. If the TPS file contains landmark sets for more than one specimen, the landmarks for each specimen will be written to a separate shape file.}
  \item{image.file}{A folder containing images corresponding to each of the specimens in the TPS file. The image filenames must match the text in the \code{spec.names} field within the TPS file.}
  \item{landmark.names}{The names corresponding to the landmarks in the TPS file, in the same order in which they are listed in the TPS file. This can be either a vector of landmark names or a '.txt' file with each of the landmark names listed on a separate line.}
  \item{spec.names}{The label in the TPS file indicating the specimen name.}
  \item{scaling.units}{The scaling units for the TPS landmarks (e.g 'cm', 'mm').}
  \item{flip.y}{A logical indicating whether the y-coordinates of the TPS landmarks should be flipped.}
}
\details{
TPS is a common file format used in morphometrics. This function reads landmarks from a TPS file and converts these into the StereoMorph shape file format. This function is intended for users who have previously collected shape data in the TPS format that they would like to import into StereoMorph. The resulting shape files can be opened with the StereoMorph digitizing application (see \code{\link{digitizeImages}}).

In most cases, \code{flip.y} should be \code{TRUE} (the default). The StereoMorph digitizing application follows the opposite convention from TPS with regard to which part of the image corresponds to the 0 y-coordinate (i.e. top versus bottom). By flipping the y-coordinates the landmarks are properly rendered on top of the image in the digitizing application. Flipping is performed using the height of the corresponding image. For this reason it is necessary to include the \code{image.file} parameter to determine the height of the corresponding image.
}
\value{
NULL
}
\author{Aaron Olsen}
\seealso{
\code{\link{readShapes}}, 
\code{\link{writeLMToTPS}}
}
\examples{
\dontrun{
# Convert TPS file to a series of StereoMorph shape files
TPSToShapes(tps.file='tps_file.TPS', shapes.file='Shapes', image.file='Images', 
    landmark.names='landmarks.txt', scaling.units='mm') 
}
}