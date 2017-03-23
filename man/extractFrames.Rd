\name{extractFrames}
\alias{extractFrames}
\title{Extracts frames from video}
\description{
Extracts frames from a video saving them as a series of images
}
\usage{
extractFrames(file = NULL, save.to = NULL, frames = NULL, names = NULL, 
              ext = 'jpeg', qscale = 2, frame.start = 0, video.i = NULL, 
              warn.min = 100)
}
\arguments{
  \item{file}{Video file from which frames are to be extracted.}
  \item{save.to}{Where to save the extracted frames.}
  \item{frames}{The frames to be extracted, starting with 0.}
  \item{names}{Names to be given to the extracted frames. If \code{NULL} the function will automatically name them with the corresponding frame number, preceded by enough zeros to maintain a constant filename width.}
  \item{ext}{The image type/extension to be added to each extract frame.}
  \item{qscale}{Integer indicating the image quality of the extracted frames. This is an input parameter passed direclty to ffmpeg.}
  \item{frame.start}{The time (in msec) corresponding to the frame immediately before the first frame change.}
  \item{video.i}{Video metadata passed to the function. This parameter is only intended for internal use.}
  \item{warn.min}{The minimum number of extracted frames for which the user is prompted and has to respond 'y' prior to frame extraction. This is intended to prevent the user from mistakenly extracting thousands of frames.}
}
\details{
In order to use this function you must separately install the ffmpeg video codec library. For instructions please refer to the 'Extracting video frames' section of the most \href{https://aaronolsen.github.io/software/stereomorph.html}{recent StereoMorph user guide here}. This function can be used interactively with prompts by calling \code{extractFrames()}. The user will then be prompted for all necessary input parameters. This is useful because the function will report the number of frames in the video before prompting which frames the user would like to extract.
}
\value{
NULL
}
\author{Aaron Olsen}
\examples{
\dontrun{
# Use extractFrames() with interactive prompts
extractFrames()

# Extract the first 20 frames from a video
extractFrames(file='Example_video.mov', save.to='Frames', frames=0:20)
}
}