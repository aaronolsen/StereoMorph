\name{avectors}
\alias{avectors}
\title{Computes the angle between two vectors}
\description{This function returns the angle (in radians) between two vectors. The vectors can be of any dimension.}
\usage{avectors(u, v)}
\arguments{
  \item{u}{a vector}
  \item{v}{a vector}
}
\value{the angle (in radians) between the two input vectors.}
\author{Aaron Olsen}
\examples{
## THE ANGLE BETWEEN TWO 2D, ORTHOGONAL VECTORS
## VALUE IS EQUAL TO asin(1/sqrt(2))
u <- c(0, 1)
v <- c(1, 0)
avectors(u, v)

## THE ANGLE BETWEEN TWO 3D VECTORS
## VALUE IS EQUAL TO asin(sqrt(2)/sqrt(3))
u <- c(1, 1, 1)
v <- c(0, 1, 0)
avectors(u, v)
}