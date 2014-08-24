\name{quadraticPointsOnInterval}
\alias{quadraticPointsOnInterval}
\title{Generates points along an interval with quadratic parameterization}
\description{
Generates a specified number of points on an interval, applying a quadratic function to interpoint spacing. This function is called internally by \code{\link{imagePlaneGridTransform}}.
}
\usage{
quadraticPointsOnInterval(t1, t2, n, a)
}
\arguments{
  \item{t1}{the starting value of the returned points.}
  \item{t2}{the final value of the returned points.}
  \item{n}{the number of points.}
  \item{a}{a quadratic parameter describing how interpoint spacing changes over the interval.}
}
\details{
The parameter \code{a} describes how strong of a skew to place on the interpoint distances over the interval specified by \code{t1} and \code{t2}. When \code{a=0}, the points are spaced uniformly across the interval. When \code{a>0} or \code{a<0}, points become further apart or closer together along the interval, respectively, at the rate of a quadratic function (see "Examples").
}
\value{a vector of points.}
\author{Aaron Olsen}
\seealso{
\code{\link{imagePlaneGridTransform}}, \code{\link{resampleGridImagePoints}}, \code{\link{imagePlaneGridTransformError}}
}
\examples{
## GENERATE EVENLY SPACED POINTS ON INTERVAL
q0 <- quadraticPointsOnInterval(t1=0, t2=1, n=10, a=0)

## MAKE POINTS PROGRESSIVELY FURTHER APART ALONG INTERVAL
qgt0 <- quadraticPointsOnInterval(t1=0, t2=1, n=10, a=1)

## MAKE POINTS PROGRESSIVELY CLOSER TOGETHER ALONG INTERVAL
qlt0 <- quadraticPointsOnInterval(t1=0, t2=1, n=10, a=-1)

## PLOT POINTS ON THREE SEPARATE LINES
plot(q0, rep(0, 10))
points(qgt0, rep(0.5, 10), col='green')
points(qlt0, rep(-0.5, 10), col='blue')
}