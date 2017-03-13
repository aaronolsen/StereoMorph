\name{writeLMToTPS}
\alias{writeLMToTPS}
\title{Writes landmarks as TPS file}
\description{
Reads landmarks from a StereoMorph shape file and writes them in the TPS file format
}
\usage{
writeLMToTPS(shapes.file, tps.file, in.pixels = TRUE, 
        flip.y = TRUE, flip.x = FALSE)
}
\arguments{
  \item{shapes.file}{A single shape file or folder containing multiple shape files. If this is a folder containing multiple files then all of the landmarks from each file will be written into a single TPS file as separate specimens.}
  \item{tps.file}{A TPS file where landmarks will be saved.}
  \item{in.pixels}{A logical indicating whether function should write pixel or scaled landmark coordinates to the TPS file.}
  \item{flip.y}{A logical indicating whether to flip the y-coordinates of the landmarks. This may be necessary depending on how another program renders the image.}
  \item{flip.x}{A logical indicating whether to flip the x-coordinates of the landmarks. This may be necessary depending on how another program renders the image.}
}
\details{
TPS is a common file format used in morphometrics. This function converts landmarks from the StereoMorph shape file format into the TPS format. This is intended for users who would like to input landmark data collected using StereoMorph into a program that reads TPS files.
}
\value{
NULL
}
\author{Aaron Olsen}
\examples{
# Get the path to package example files
fdir <- paste0(path.package("StereoMorph"), "/extdata/")

# Write 2D landmark pixel coordinates from a single shape file to TPS
writeLMToTPS(paste0(fdir, 'Shapes_2D/mug_003.txt'), 'Mug_2D.tps')

# Write 3D landmark coordinates from a single shape file to TPS
writeLMToTPS(paste0(fdir, 'Shapes_3D/bubo_virginianus_FMNH488595.txt'), 'Owl_3D.tps')

# Write 2D landmark pixel coordinates from multiple shape file to TPS
writeLMToTPS(paste0(fdir, 'Shapes_2D'), 'Shapes_2D.tps')

# Write 3D landmark coordinates from multiple shape file to TPS
writeLMToTPS(paste0(fdir, 'Shapes_3D'), 'Shapes_3D.tps', in.pixels=FALSE)
}