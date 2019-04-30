undistortShapes <- function(shapes, cal.file, view = NULL){

	# Get calibration information
	if(is.list(cal.file)){
		cal_list <- cal.file
	}else{
		if(!file.exists(cal.file)) stop(paste0("The calibration file ('", cal.file, "') was not found."))
		cal_list <- XML4R2list(cal.file)$calibration
	}

	# If no undistortion parameters, return original shapes
	if(is.null(cal_list$undistort.params)) return(shapes)
	
	# Undistort landmarks
	if(!is.null(shapes$landmarks.pixel)){
	
		if(length(dim(shapes$landmarks.pixel)) == 3){
		
			for(view in dimnames(shapes$landmarks.pixel)[[3]]){

				# Check that view is found in undistortion parameters
				if(!view %in% dimnames(cal_list$undistort.params)[[1]]){
					stop(paste0("View name from the digitized shapes ('", view, "') not found in the view names from the undistortion calibration ('", paste0(dimnames(cal_list$undistort.params)[[1]], collapse="', '"), "')"))
				}
			
				# Undistort
				shapes$landmarks.pixel[, , view] <- undistort(shapes$landmarks.pixel[, , view], 
					image.size=cal_list$img.size[view, ], center=cal_list$undistort.params[view, c('cx', 'cy')], 
					k=cal_list$undistort.params[view, c('k1', 'k2', 'k3')], 
					p=cal_list$undistort.params[view, c('p1', 'p2')])
			}
		}else{
		
			stop('A single view of shapes was input without specifying the view.')

			# Undistort
			shapes$landmarks.pixel <- undistort(shapes$landmarks.pixel, 
				image.size=cal_list$img.size[view, ], center=cal_list$undistort.params[view, c('cx', 'cy')], 
				k=cal_list$undistort.params[view, c('k1', 'k2', 'k3')], 
				p=cal_list$undistort.params[view, c('p1', 'p2')])
		}
	}

	if(!is.null(shapes$curves.pixel)){
		warning('Undistortion for curve points not yet supported.')
	}

	return(shapes)
}