distortionError <- function(p, ucoor, dcoor, img.size){

	# Replace NA values with 0
	p[is.na(p)] <- 0

	# Apply distortion parameters
	dcoor_p <- undistort(ucoor, image.size=img.size, center=c(p[1], p[2]), 
		k=c(p[3], p[4], p[5]), p=c(p[6], p[7]))

	# Find error between input distorted coordinates and coordinates from distortion parameters
	errors <- sqrt(rowSums((dcoor - dcoor_p)^2))
	
	mean(errors)
}
