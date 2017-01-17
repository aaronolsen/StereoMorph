epipolarBezier <- function(p, cal.coeff, undistort.coeff = NULL, distort.coeff = NULL, 
	img.size = NULL, views = c(1,2), self = FALSE){

	# Takes a point and returns the epipolar line in the second view (by default) as a 
	# Bezier function. The second view is determined as the second column in the cal.coeff 
	# input matrix and the second row of the undistort/distort.coeff matrix. Bezier is 
	# needed because of possible distortion

	# Set views, second value is view in which epipolar curve is drawn (from, to)
	cal.coeff <- cal.coeff[, views]
	undistort.coeff <- undistort.coeff[views, ]
	distort.coeff <- distort.coeff[views, ]
	img.size <- img.size[views, ]

	# If no distortion get simple line
	if(is.null(undistort.coeff)){
		
		# Get epipolar line
		eline <- dltEpipolarLine(p=p, cal.coeff1=cal.coeff, self=self)

		# Get points spanning x-range of image for sampling of points along epipolar line
		xpts <- seq(0-img.size[2,1]*0.1, img.size[2,1]*1.1, length=4)

		# Generate points along epipolar line
		epoints <- cbind(xpts, xpts*eline$m + eline$b)

		# Return as 4-point Bezier (control points all on line)
		return(list('m'=eline$m, 'b'=eline$b, 'p'=epoints))

	}else{

		# Undistort point
		up <- undistort(p, img.size[1, ], center=undistort.coeff[1, 1:2], 
			k=undistort.coeff[1, 3:5], p=undistort.coeff[1, 6:7])

		# Get epipolar line of undistorted point
		elineRu <- dltEpipolarLine(p=up, cal.coeff1=cal.coeff)

		# Get points spanning x-range of image for sampling of points along epipolar line
		xpts <- seq(0-img.size[2,1]*0.1, img.size[2,1]*1.1, length=50)

		# Generate points along undistorted epipolar line
		epointsRu <- cbind(xpts, xpts*elineRu$m + elineRu$b)

		# Distort epipolar line points
		epointsRd <- undistort(epointsRu, img.size[2, ], center=distort.coeff[2, 1:2], 
			k=distort.coeff[2, 3:5], p=distort.coeff[2, 6:7])
		
		if(self) return(epipolarBezier(p=epointsRd[1,], cal.coeff, undistort.coeff, distort.coeff, 
			img.size, views=2:1))

		# Fit Bezier curve to points
		bezier_fit <- bezierCurveFit(epointsRd, fix.start.end=FALSE, min.control.points=4, 
			max.control.points=4)
		
		#print(bezier_fit$rse)

		return(list('p'=bezier_fit$p))
	}
}