\name{distancePointToPoint}
\alias{distancePointToPoint}
\title{Finds the distance between two points or sets of points}
\description{Finds the distance betweeen two single points, the distances between one point and a set of points, or the distances between two point sets. Points can be of any number of dimensions.}
\usage{
distancePointToPoint(p1, p2 = NULL)
}
\arguments{
  \item{p1}{a vector of a single point or a matrix of one or multiple points}
  \item{p2}{a vector of a single point or a matrix of one or multiple points. If \code{NULL}, the function either returns the distance of \code{p1} from the origin or the distances between subsequent values of \code{p1}.}
}
\details{If \code{p1} is a single point and \code{p2} is a single point then the function returns the distance between these two points. If either \code{p1} or \code{p2} is a single point and the other is a matrix of multiple points then the function returns a vector of the distances between the single point and each of the multiple points. If both \code{p1} and \code{p2} are matrices of multiple points, then the function returns a vector of the distances between the points in each corresponding row. If \code{p1} and \code{p2} are both matrices, the matrix dimensions must match.
	
If \code{p2} is \code{NULL}, then \code{distancePointToPoint()} returns the distance between consecutive points in \code{p1}. If \code{p1} is a vector, the function returns the absolute difference between consecutive values of \code{p1} (interpoint distances along a single dimension). If \code{p1} is a matrix, then the function returns the distance between the point in each row of \code{p1} and its subsequent row. This can be used to return the interpoint distances along a curve defined as a matrix of points.
}
\value{a vector of distance(s)}
\author{Aaron Olsen}
\seealso{
\code{\link{distancePointToLine}}
}
\examples{

## FIND THE DISTANCE BETWEEN TWO, 2D POINTS
## VALUE IS sqrt(2)
distancePointToPoint(p1=c(0, 0), p2=c(1, 1))

## FIND THE DISTANCE BETWEEN A 2D POINT AND MULTIPLE 2D POINTS
p1 <- c(0, 0)
p2 <- matrix(c(1, 1, 2, 2, 3, 3), nrow=3, ncol=2, byrow=TRUE)
distancePointToPoint(p1=p1, p2=p2)

## FIND THE DISTANCE BETWEEN TWO SETS OF 2D POINTS
p1 <- matrix(c(0, 0, 1, 1, 2, 2), nrow=3, ncol=2, byrow=TRUE)
p2 <- matrix(c(1, 1, 2, 2, 3, 3), nrow=3, ncol=2, byrow=TRUE)
distancePointToPoint(p1=p1, p2=p2)

## FIND THE DISTANCE BETWEEN A 3D POINT AND MULTIPLE 3D POINTS
p1 <- c(0, 0, 0)
p2 <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)
distancePointToPoint(p1=p1, p2=p2)

## FIND THE DISTANCE BETWEEN CONSECUTIVE VALUES IN A VECTOR
distancePointToPoint(p1=c(1, 2, 4, 7))

## FIND THE DISTANCE BETWEEN CONSECUTIVE 2D POINTS IN A MATRIX
## HERE, WE FIND THE DISTANCE BETWEEN THE POINT c(0, 0) AND c(1, 1), WHICH IS sqrt(2)
distancePointToPoint(p1=matrix(c(0, 0, 1, 1), nrow=2, ncol=2, byrow=TRUE))

## FIND THE DISTANCE BETWEEN CONSECUTIVE 2D POINTS IN A MATRIX, WITH MORE POINTS
## HERE, WE ADD TWO MORE POINTS TO THE PREVIOUS EXAMPLE: c(2, 2) AND c(3, 3)
## THE DISTANCE BETWEEN EACH CONSECUTIVE PAIR OF POINTS IS sqrt(2)
distancePointToPoint(p1=matrix(c(0, 0, 1, 1, 2, 2, 3, 3), nrow=4, ncol=2, byrow=TRUE))

## FIND THE DISTANCE BETWEEN CONSECUTIVE 3D POINTS IN A MATRIX
distancePointToPoint(p1=matrix(c(0, 0, 0, 1, 1, 1), nrow=2, ncol=3, byrow=TRUE))

## FIND THE DISTANCE BETWEEN CONSECUTIVE 4D POINTS IN A MATRIX
distancePointToPoint(p1=matrix(c(0, 0, 0, 0, 1, 1, 1, 1), nrow=2, ncol=4, byrow=TRUE))
}