\name{gridPointsFit}
\alias{gridPointsFit}
\title{Fits regularly spaced points to a sample line or grid}
\description{
This function is used to fit a model of points at a regular interval to a sample of points in one dimension. The function is used by \code{\link{measureCheckerboardSize}} to estimate the solution to the inter-point distance of points along a line or in a grid.
}
\usage{	
gridPointsFit(p, nx, ny=NULL)
}
\arguments{
  \item{p}{The parameters defining the regular point distribution. When \code{nx} is \code{NULL}, \code{p} is of length 2. When \code{nx} is non-\code{NULL}, \code{p} is of length 3.}
  \item{nx}{The number of points to be created at regular spacing along one dimension.}
  \item{ny}{The number of points to be created at regular spacing along a second dimension.}
}
\details{
This function is used to fit a model of points at a regular interval to a sample of points in one dimension. The function is used by \code{\link{measureCheckerboardSize}} to estimate the solution to the inter-point distance of points along a line or in a grid. To fit a model to points along lines and grids in two dimensions, each dimension is fit separately. A best fit estimate of the true interval between points can then be calculated from the optimized parameters. See the examples below for how to use \code{gridPointsFit()} to estimate the inter-point intervals of line and grid points.
}
\value{
a vector of length \code{nx*ny}.
}
\author{Aaron Olsen}
\seealso{\code{\link{measureCheckerboardSize}}}
\examples{

## ESTIMATE LINE INTER-POINT INTERVAL
# GENERATE POINTS AT A REGULAR INTERVAL WITH NORMAL, RANDOM VARIATION
pts <- cbind((1:500) + rnorm(500, sd=1), (1:500) + rnorm(500, sd=1))

# FIND THE MEAN SUCCESSIVE POINT-TO-DISTANCE
# NOTE THAT THIS CONSISTENTLY OVERESTIMATES THE TRUE INTERVAL
mean(sqrt(rowSums((pts[2:nrow(pts), ] - pts[1:(nrow(pts)-1), ])^2)))

# FIT A REGULARLY SPACED POINTS MODEL TO EACH DIMENSION OF THE POINTS MATRIX
fit_x <- nlminb(start=c(pts[1, 1], pts[2, 1]-pts[1, 1]), 
    objective=gridPointsFitError, nx=nrow(pts), points=pts[, 1])
fit_y <- nlminb(start=c(pts[1, 2], pts[2, 2]-pts[1, 2]), 
    objective=gridPointsFitError, nx=nrow(pts), points=pts[, 2])

# FIND THE BEST FIT INTER-POINT DISTANCE
# MORE ACCURATELY RECOVERS TRUE INTERVAL
sqrt(fit_x$par[2]^2 + fit_y$par[2]^2)


## ESTIMATE REGULAR GRID SQUARE SIZE
# GENERATE A REGULAR GRID WITH NORMAL, RANDOM VARIATION
corners <- cbind(
    rep(1:20, 20) + rnorm(20^2, sd=0.1), 
    c(t(matrix(1:20, nrow=20, ncol=20))) + rnorm(20^2, sd=0.1))

# FIT A REGULARLY SPACED POINTS MODEL TO EACH DIMENSION OF THE POINTS MATRIX
fit_x <- nlminb(
    start=c(corners[1, 1], corners[2, 1]-corners[1, 1], 0),
    objective=gridPointsFitError, points=corners[, 1], nx=20, ny=20)
fit_y <- nlminb(
    start=c(corners[1, 2], corners[2, 2]-corners[1, 2], 0),
    objective=gridPointsFitError, points=corners[, 2], nx=20, ny=20)

# FIND THE BEST FIT INTER-POINT DISTANCE (SQUARE SIZE)
sqrt(fit_x$par[2]^2 + fit_y$par[2]^2)
}