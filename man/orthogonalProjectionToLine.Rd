\name{orthogonalProjectionToLine}
\alias{orthogonalProjectionToLine}
\title{Finds the orthogonal projection of a point onto a line}
\description{
Given a 2D or 3D input point \code{p} and a 2D or 3D line, this function finds a point on the line at a minimum distance from point \code{p}. This is equivalent to the orthogonal projection of point \code{p} onto the line.
}
\usage{
orthogonalProjectionToLine(p, l1 = NULL, l2 = NULL)
}
\arguments{
  \item{p}{a vector of a single point or a matrix of multiple points}
  \item{l1}{a vector describing a point on a line or a list with line constants}
  \item{l2}{if \code{l1} is a point, a second point on a line}
}
\details{
If \code{p} is a vector, the function returns a point as a vector of the same dimension. If \code{p} is a matrix, each row is treated as a point and the orthogonal projection is returned for each. These points are returned as a matrix (of the same dimension), each row being the orthogonal projection of the corresponding row in \code{p}.

The line input can be defined using one of three standard ways: two points on the line, 'm' and 'b' constants (slope and y-intercept) and direction numbers 'abc' (a vector parallel to a line through the origin). If \code{l1} is a vector, this is taken as one point on the line and \code{l2} must be a second point on the line. If \code{l1} is a list, the named objects must correspond to one of these three line definitions. Two points on the line are defined as \code{l1$l1} and \code{l1$l2}. 'm' and 'b' are defined as \code{l1$m} and \code{l1$b}. And the direction numbers 'abc' are defined as \code{l1$a}, \code{l1$b} and \code{l1$c}.
}
\value{
a vector if \code{p} is a vector and a matrix if \code{p} is a matrix. The returned vector or matrix will be of the same dimensions as \code{p}.
}
\references{
\url{http://paulbourke.net/geometry/pointlineplane/}
}
\author{Aaron Olsen}
\seealso{\code{\link{distancePointToLine}}}
\examples{
## POINT INPUT: 2D VECTOR
## LINE INPUT: l1, l2
## LINE THROUGH THE ORIGIN WITH SLOPE OF ONE
orthogonalProjectionToLine(p=c(0, 5), l1=c(0, 0), l2=c(3, 3))

## POINT INPUT: 2D VECTOR
## LINE INPUT: LIST WITH l1, l2
orthogonalProjectionToLine(p=c(0, 5), l1=list(l1=c(0, 0), l2=c(3, 3)))

## POINT INPUT: 2D VECTOR
## LINE INPUT: LIST WITH m, b
## LINE WITH Y-INTERCEPT AT ONE AND SLOPE OF ONE
orthogonalProjectionToLine(p=c(0, 5), l1=list(m=1, b=0))

## POINT INPUT: 2D VECTOR
## LINE INPUT: LIST WITH VECTOR PARALLEL TO LINE THROUGH THE ORIGIN
## LINE THROUGH THE ORIGIN WITH SLOPE OF ONE
orthogonalProjectionToLine(p=c(0, 5), l1=list(a=1, b=-1, c=0))

## POINT INPUT: 2D VECTOR
## LINE INPUT: SAME AS PREVIOUS BUT WITH Z-AXIS COMPONENT
orthogonalProjectionToLine(p=c(0, 5), l1=list(a=1, b=-1, c=1))

## POINT INPUT: 3D VECTOR
## LINE INPUT: l1, l2
orthogonalProjectionToLine(p=c(0, 5, 0), l1=list(l1=c(0, 0, 0), l2=c(3, 3, 3)))

## POINT INPUT: 2D MATRIX
## LINE INPUT: l1, l2
p <- matrix(c(0,5, 0,10), nrow=2, byrow=TRUE)
orthogonalProjectionToLine(p=p, l1=list(l1=c(0, 0), l2=c(3, 3)))

## POINT INPUT: 3D MATRIX
## LINE INPUT: l1, l2
p <- matrix(c(0,5,0, 0,10,0), nrow=2, byrow=TRUE)
orthogonalProjectionToLine(p=p, l1=list(l1=c(0, 0, 0), l2=c(3, 3, 3)))
}
\keyword{ lines }