polyArea <- function(xy){

	# If start and end point are not the same, add start to end
	if(sum(abs(xy[1, ] - xy[nrow(xy), ])) > 0) xy <- rbind(xy, xy[1, ])
	
	# Get number of points
	n <- nrow(xy)
	
	# Find area
	0.5*abs(sum(xy[1:(n-1), 1]*xy[2:n, 2] - xy[2:n, 1]*xy[1:(n-1), 2]))
}