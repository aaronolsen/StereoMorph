\name{distancePointToLine}
\alias{distancePointToLine}
\title{Finds the minimum distance(s) between point(s) and a line}
\description{Finds the minimum distance between a point and a line or multiple points and a line in two or three dimensions.}
\usage{
distancePointToLine(p, l1, l2 = NULL)
}
\arguments{
  \item{p}{a vector of a single point or a matrix of multiple points}
  \item{l1}{a vector describing a point on a line or a list with line constants}
  \item{l2}{if \code{l1} is a point, a second point on a line}
}
\details{
If \code{p} is a vector, the function returns the distance between a point and the line input. If \code{p} is a matrix, the function returns the distance between each point in the matrix (defined by each row) and the line input. If \code{p} is a vector, the length must be 2 or 3 (2D or 3D, respectively). If \code{p} is a matrix, the number of columns must be 2 or 3 (2D or 3D, respectively).

The line input can be defined using one of three standard ways: two points on the line, 'm' and 'b' constants and direction numbers (a vector parallel to the line). If \code{l1} is a vector, this is taken as one point on the line and \code{l2} must be a second point on the line. If \code{l1} is a list, the named objects must correspond to one of these three line definitions. Two points on the line are defined as \code{l1$l1} and \code{l1$l2}. 'm' and 'b' are defined as \code{l1$m} and \code{l1$b}. And the direction numbers 'abc' are defined as \code{l1$a}, \code{l1$b} and \code{l1$c}.
}
\value{a vector of distance(s)}
\author{Aaron Olsen}
\seealso{
\code{\link{orthogonalProjectionToLine}}
}
\examples{
## FIND THE DISTANCE BETWEEN A 2D POINT AND A LINE DEFINED BY A SLOPE AND Y-INTERCEPT
distancePointToLine(p=c(0, 2), l1=list(m=0, b=1))

## FIND THE DISTANCE BETWEEN A 2D POINT AND A LINE DEFINED BY TWO POINTS ON THE LINE
distancePointToLine(p=c(0, 5), l1=list(l1=c(2, 4), l2=c(2, 1)))

## FIND THE DISTANCE BETWEEN MULTIPLE 2D POINTS AND A LINE DEFINED BY A SLOPE AND Y-INTERCEPT
p <- matrix(c(0, 0, 1, 1, 2, 2), nrow=3, ncol=2, byrow=TRUE)
distancePointToLine(p=p, l1=list(m=0, b=1))

## FIND THE DISTANCE BETWEEN MULTIPLE 2D POINTS AND A LINE DEFINED BY DIRECTION NUMBERS
p <- matrix(c(0, -1.5, 1, -2, 2, 2), nrow=3, ncol=2, byrow=TRUE)
distancePointToLine(p=p, l1=list(a=1, b=2, c=3))

## FIND THE DISTANCE BETWEEN A 3D POINT AND A LINE DEFINED BY TWO POINTS ON THE LINE
## HERE THE DISTANCE IS EQUAL TO sqrt(2)
distancePointToLine(p=c(1, 1, 1), l1=c(0, 0, 0), l2=c(1, 0, 0))

## FIND THE DISTANCE BETWEEN MULTIPLE 3D POINTS AND A LINE DEFINED BY TWO POINTS ON THE LINE
p <- matrix(c(0, 0, 0, 1, 1, 1, 2, 2, 2), nrow=3, ncol=3, byrow=TRUE)
distancePointToLine(p=p, l1=list(l1=c(0, 0, 0), l2=c(1, 0, 0)))
}
\keyword{ lines }