undistortionError <- function(p, coor.img, coor.obj, image.size){


	# Replace NA values with 0
	if (length(p) == 5){
	  # Apply distortion parameters
	  coor_img_u  <- undistort(ucoor, image.size=img.size, center=c(p[1], p[2]), 
		k=c(p[3], p[4], p[5]), p=c(0,0))
        }
	if (length(p) == 7){
	  # Apply distortion parameters
	  coor_img_u <- undistort(ucoor, image.size=img.size, center=c(p[1], p[2]), 
		k=c(p[3], p[4], p[5]), p=c(p[6], p[7]))
	}
	
	# FIND HOMOGRAPHY ERROR
	find_hom <- findHomography(coor_img_u, coor.obj)

	mean(find_hom$error)
}
