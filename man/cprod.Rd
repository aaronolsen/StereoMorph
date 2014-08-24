\name{cprod}
\alias{cprod}
\title{Computes the cross product of two vectors}
\description{Returns the cross product of two vectors using either the right-hand or left-hand convention.}
\usage{
cprod(u, v, h = "right")
}
\arguments{
  \item{u}{a vector of length 3}
  \item{v}{a vector of length 3}
  \item{h}{whether the right-hand, "right", or left-hand, "left", convention is to be used. The right-hand convention is default.}
}
\details{The cross product vector is a vector orthogonal to the two input vectors.}
\value{the cross product vector (also of length 3)}
\author{Aaron Olsen}
\examples{

## DEFINE TWO 3D VECTORS
u <- c(1, 0, 0)
v <- c(0, 1, 0)

## FIND THE CROSS PRODUCT
cprod(u, v)
}