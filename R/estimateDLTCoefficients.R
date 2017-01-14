estimateDLTCoefficients <- function(cal.list, sample.est, num.sample.est, cal_corners, aspect_non_na, min_views, 
	cal_views_found_num, estimate_cal_coeffs, 
	num.sample.sets, num.aspects.sample, max.sample.optim, num_views, 
	img_sub_dir, non_na_by_view, print.progress, nx, ny, sq.size.num, fit.min.break, objective.min, 
	objective.min.break, nlm.calls.max, img_sub_dir_salign, unify_view_pairs, unify_sub, ...){

	# IF NAMES OF ASPECTS TO BE SAMPLED ARE EXPLICITLY DEFINED, MAKE SURE LENGTH OF NUM SAMPLE MATCHES NUMBER OF ASPECTS
	if(!is.null(sample.est)) num.sample.est <- length(sample.est)

	if(is.null(sample.est)){

		# REMOVE IMAGE PAIRS WHERE AT LEAST ONE IS NA (NUMBER OF NON-NA IMAGE PAIRS ALREADY DETERMINED)
		cal_corners_trim <- cal_corners[, , aspect_non_na >= min_views, ]

	}else{

		# sample.est IS USED FOR DE-BUGGING
		# OF THE PROVIDED SAMPLE ASPECTS FIND WHICH ARE NOT NA IN ANY VIEW
		sample_est_non_na <- sample.est[!sample.est %in% names(aspect_non_na)[which(!aspect_non_na >= min_views)]]

		# CHECK THAT NONE OF DEFINED ASPECTS ARE NA
		if(length(sample_est_non_na) < length(sample.est))
			stop(paste0("The following aspects were not found in at least one view and will not be included in the calibration estimation: ", paste(sample.est[!sample.est %in% sample_est_non_na], collapse=',')))

		# USE DEFINED ASPECTS
		cal_corners_trim <- cal_corners[, , sample_est_non_na, ]
	}

	# PRINT WARNING FOR 3 ASPECTS
	if(cal_views_found_num <= 3){
		if(estimate_cal_coeffs){
			response <- readline(prompt=paste0("\n\t\tCorners were only detected in 3 or fewer aspects. More than 3 aspects are generally required for an accurate calibration. Do you still wish to continue with the calibration? (y/n) : "))
			if(tolower(response) %in% c('no', 'n')) return(1)
		}else{
			warning(paste0("Corners were only detected in 3 or fewer aspects. More than 3 aspects are generally required for an accurate calibration."))
		}
	}

	# SET DEFAULTS FOR COEFFICIENT ESTIMATION SAMPLING PARAMETERS
	if(num.sample.est == 'auto'){
		if(cal_views_found_num < 15){
			num.sample.est <- cal_views_found_num
		}else if(cal_views_found_num >= 15 && cal_views_found_num < 20){
			num.sample.est <- 10
		}else if(cal_views_found_num >= 20 && cal_views_found_num < 25){
			num.sample.est <- 15
		}else if(cal_views_found_num >= 25){
			num.sample.est <- 20
		}
	}else{

		# IF NUMBER OF FOUND ASPECTS IS LESS THAN NUMBER OF ASPECTS TO USE FOR ESTIMATION, MAKE SAMPLE NUMBER OF FOUND ASPECTS
		if(cal_views_found_num < num.sample.est) num.sample.est <- cal_views_found_num
	}

	if(num.sample.sets == 'auto'){
		if(cal_views_found_num <= 5){
			num.sample.sets <- 1
		}else if(cal_views_found_num >= 6 && cal_views_found_num <= 7){
			num.sample.sets <- 2
		}else if (cal_views_found_num >= 8){
			num.sample.sets <- 3
		}
	}		

	if(num.aspects.sample == 'auto'){
		if(cal_views_found_num <= 5){
			num.aspects.sample <- cal_views_found_num
		}else if(cal_views_found_num >= 6 && cal_views_found_num <= 7){
			num.aspects.sample <- 5
		}else if (cal_views_found_num >= 8){
			num.aspects.sample <- 6
		}
	}else{

		# CHECK THAT NUMBER OF ASPECTS TO SAMPLE DOES NOT EXCEED TOTAL NUMBER OF FOUND ASPECTS
		if(num.aspects.sample > num.sample.est) stop(paste0("'num.aspects.sample' (", num.aspects.sample, ") must be less than 'num.sample.est' (", num.sample.est, ")."))
	}

	# GET ESTIMATION SUBSAMPLE INDICES
	sample_trim_est <- floor(seq(from=1, to=dim(cal_corners_trim)[3], length=num.sample.est))
	
	# GET ESTIMATION SUBSAMPLE
	cal_corners_trim_est <- cal_corners_trim[, , sample_trim_est, ]

	# GET OPTIMIZATION SUBSAMPLE - IF AT LEAST 10 ASPECTS NOT USED IN CALIBRATION, USE THOSE FOR OPTIMIZATION
	cal_corners_trim_optim <- NULL
	optim_with_cal <- TRUE
	if(cal_views_found_num - num.sample.est >= 10){

		# INDICES
		sample_trim_optim <- (1:dim(cal_corners_trim)[3])[!(1:dim(cal_corners_trim)[3] %in% sample_trim_est)]
		
		# CAP AT MAX
		sample_trim_optim <- sample_trim_optim[floor(seq(from=1, to=length(sample_trim_optim), length=min(length(sample_trim_optim), max.sample.optim)))]

		# SUBSAMPLE
		cal_corners_trim_optim <- cal_corners_trim[, , sample_trim_optim, ]
		
		optim_with_cal <- FALSE
	}
	
	# IF MORE THAN TWO VIEWS, CHECK THAT ONE VIEW ISNT STRANDED WITHOUT COMMON CORNERS WITH OTHER VIEWS
	if(num_views > 2){
		common_by_view <- setNames(rep(NA, num_views), img_sub_dir)
		for(view in 1:num_views){
			common_by_view[view] <- sum((non_na_by_view[, view] > 0)*(rowSums(non_na_by_view[, -view]) >= min_views-1))
			if(common_by_view[view] < round((num.aspects.sample+max.sample.optim) / 2)){
				stop(paste0("There are only ", common_by_view[view], " aspects in view '", img_sub_dir[view], "' for which corners were detected in at least ", min_views-1, " other view(s). Try increasing 'num.aspects.read'."))
			}
		}
	}

	if(num.sample.sets == 1){

		set.seed(42);
		cal_sample_sets <- list(sort(sample(1:num.sample.est, num.aspects.sample)))

	}else{

		# CREATE RANDOM SAMPLE SETS
		cal_sample_sets <- list()
		for(i in 1:max(num.sample.sets, 40)){

			# GET SAMPLE SET
			set.seed(i);
			sample_set <- sample(1:num.sample.est, num.aspects.sample);

			if(num_views > 2){

				# FIND NUMBER OF NON-NA VIEWS FOR EACH ASPECT AND VIEW
				non_na_by_view <- apply(!is.na(cal_corners_trim_est[, , sample_set, ]), c(3, 4), 'sum') > 0

				common_by_view <- setNames(rep(NA, num_views), img_sub_dir)
				for(view in 1:num_views) common_by_view[view] <- sum((non_na_by_view[, view] > 0)*(rowSums(non_na_by_view[, -view]) >= min_views-1))

				# CHECK THAT NO VIEW IS UNDERREPRESENTED
				if(sum(common_by_view < floor(num.aspects.sample / 2)) > 0) next
			}
			
			# MAKE SURE THAT SAMPLE IS NOT THE SAME AS ANY OTHER SET
			if(length(cal_sample_sets) > 0){
				go_next <- FALSE
				for(j in 1:length(cal_sample_sets)) if(sum(!sample_set %in% cal_sample_sets[[j]]) == 0){go_next <- TRUE;break}
				if(go_next) next
			}

			# SAVE SAMPLE SET
			cal_sample_sets[[length(cal_sample_sets)+1]] <- sort(sample_set)
			
			# IF NUMBER OF SAMPLE SETS HAS BEEN REACHED, BREAK
			if(length(cal_sample_sets) == num.sample.sets) break
		}
	}

	if(print.progress){
		cat(paste0("\t\tNumber of aspects from total that will be sampled for calibration coefficient estimation (num.sample.est): ", num.sample.est, "\n"))
		cat(paste0("\t\tNumber of unique sets of aspects to try (num.sample.sets): ", num.sample.sets, "\n"))
		cat(paste0("\t\tNumber of aspects to sample for each set (num.aspects.sample): ", num.aspects.sample, "\n"))
		if(num.sample.sets > 1){
			cat(paste0("\t\tAspects in each set:\n"))
			for(i in 1:length(cal_sample_sets)) cat(paste0("\t\t\t", i, ": ", paste(dimnames(cal_corners_trim_est)[[3]][cal_sample_sets[[i]]], collapse=", "), "\n"))
		}
		cat(paste0("\t\tUse calibration aspects to determine the best calibration set?: ", optim_with_cal, "\n"))
		if(!optim_with_cal) cat(paste0("\t\tNumber of aspects in optimization set: ", dim(cal_corners_trim_optim)[3], "\n"))
	}

	# ESTIMATE DLT CALIBRATION COEFFICIENTS
	if(estimate_cal_coeffs){
		
		# SAVE ALL 2D COORDINATES FOR ESTIMATION
		coor_2d <- cal_corners_trim_est

		# REDUCE CORNER NUMBER FOR ALL GRIDS BY FITTING PERSPECTIVE GRID
		# SET REDUCED GRID DIMENSIONS
		rx <- ry <- 3
		
		# SET REDUCED GRID SIZES
		sx <- ((nx-1)*sq.size.num) / (rx-1)
		sy <- ((ny-1)*sq.size.num) / (ry-1)

		# EMPTY REDUCED GRID DIMENSION ARRAY
		coor_2d_red <- array(NA, dim=c(rx*ry, 2, dim(coor_2d)[3], dim(coor_2d)[4]), dimnames=list(NULL, c('x', 'y'), dimnames(coor_2d)[[3]], dimnames(coor_2d)[[4]]))

		if(print.progress) cat('\n\t\tReduce number of corners (', dim(coor_2d)[3]*dim(coor_2d)[4], ' checkerboards total)\n', sep='')

		for(i in 1:dim(coor_2d)[3]){
			for(j in 1:dim(coor_2d)[4]){

				if(print.progress) cat('\t\t\t', (i-1)*dim(coor_2d)[4] + j, ') Aspect: ', dimnames(coor_2d)[[3]][i], '; View: ', dimnames(coor_2d)[[4]][j], '; ', sep='')

				# DOWNSAMPLE THE NUMBER OF CORNERS
				coor_2d_red[, , i, j] <- resampleGridImagePoints(pts=coor_2d[, , i, j], 
					nx=nx, rx=rx, ry=ry, fit.min.break=fit.min.break, print.progress=print.progress)$pts
				
				# fit.min.break is mean error
			}
		}

		# EMPTY LIST TO STORE CALIBRATION RESULTS
		dlt_cal_cam_list <- list()

		# CALIBRATE CAMERAS WITH EACH SAMPLE SET
		cal_optim <- rep(NA, num.sample.sets)
		for(i in 1:num.sample.sets){

			if(print.progress) cat('\n\t\tAspects used in estimating coefficients: ', paste(dimnames(coor_2d_red)[[3]][cal_sample_sets[[i]]], collapse=", "), '\n', sep='')

			# ESTIMATE DLT CALIBRATION COEFFICIENTS FOR ALL VIEWS
			dlt_cal_cam <- dltCalibrateCameras(coor.2d=coor_2d_red[, , cal_sample_sets[[i]], ], nx=3, 
				grid.size=c(sx, sy), print.progress=print.progress, print.tab='\t\t', 
				reduce.grid.dim=FALSE, objective.min=objective.min, objective.min.break=objective.min.break, 
				min.views=min_views, grid.incl.min=2, nlm.calls.max=nlm.calls.max, ...)
			
			if(print.progress) cat('\n')

			dlt_cal_cam_list[[i]] <- list()

			# SET COLUMN NAMES FOR COEFFICIENTS
			colnames(dlt_cal_cam$cal.coeff) <- img_sub_dir

			# ADD COEFFICIENT ESTIMATION RESULTS
			dlt_cal_cam_list[[i]][['cal.set.num']] <- i
			dlt_cal_cam_list[[i]][['cal.coeff']] <- dlt_cal_cam$cal.coeff
			dlt_cal_cam_list[[i]][['mean.reconstruct.rmse']] <- dlt_cal_cam$mean.reconstruct.rmse
			dlt_cal_cam_list[[i]][['coefficient.rmse']] <- dlt_cal_cam$coefficient.rmse
			dlt_cal_cam_list[[i]][['cal.sample.aspects']] <- dimnames(coor_2d_red)[[3]][cal_sample_sets[[i]]]

			if(!is.null(cal_corners_trim_optim)){

				# TEST CALIBRATION ACCURACY AGAINST OPTIM SET
				test_calibration <- dltTestCalibration(dlt_cal_cam$cal.coeff, cal_corners_trim_optim, nx, sq.size.num)
			
				# SAVE IPD ERROR FOR CHOOSING BEST CALIBRATION
				cal_optim[i] <- mean(test_calibration$ipd.rmse)

				# SAVE OTHER CALIBRATION ACCURACY TEST RESULTS
				dlt_cal_cam_list[[i]][['cal.ipd.rmse']] <- test_calibration$ipd.rmse
				dlt_cal_cam_list[[i]][['cal.ipd.abs.mean']] <- mean(abs(test_calibration$ipd.error))
				dlt_cal_cam_list[[i]][['cal.ipd.abs.max']] <- max(abs(test_calibration$ipd.error))
				dlt_cal_cam_list[[i]][['cal.ipd.abs.sd']] <- sd(abs(test_calibration$ipd.error))
				dlt_cal_cam_list[[i]][['cal.ipd.mean']] <- mean(test_calibration$ipd.error)
				dlt_cal_cam_list[[i]][['cal.ee.rms']] <- test_calibration$epipolar.rmse
				dlt_cal_cam_list[[i]][['cal.ee.mean']] <- mean(test_calibration$epipolar.error)
				dlt_cal_cam_list[[i]][['cal.ee.max']] <- max(test_calibration$epipolar.error)
				dlt_cal_cam_list[[i]][['cal.ee.sd']] <- sd(test_calibration$epipolar.error)

			}else{
		
				# SAVE RECONSTRUCT ERROR FOR CHOOSING BEST CALIBRATION
				cal_optim[i] <- dlt_cal_cam$mean.reconstruct.rmse
			}
		}

		# GET SAMPLE SET WITH MINIMUM ERROR
		min_error_set <- which.min(cal_optim)
	
		# COPY ELEMENTS FROM MINIMUM ERROR SET
		for(ename in names(dlt_cal_cam_list[[min_error_set]])) cal.list[[ename]] <- dlt_cal_cam_list[[min_error_set]][[ename]]

		if(print.progress) cat("\n")
	}

	# SAVE CALIBRATION RESULTS FOR EACH SUBSET
	if(estimate_cal_coeffs && unify_view_pairs){
		for(cal_name in c('cal.coeff', 'cal.set.num', 'mean.reconstruct.rmse', 'coefficient.rmse', 'cal.sample.aspects')){
			cal.list[[paste0('sub', unify_sub, '.', cal_name)]] <- cal.list[[cal_name]]
			cal.list[[cal_name]] <- NULL
		}
	}

	if(print.progress){
		
		if(!unify_view_pairs){
			cat('\n')
			if(num.sample.sets > 1){
				cat(paste0("\t\tSelected aspect set (lowest reconstruction error): ", cal.list$cal.set.num, "\n"))
				cat(paste0("\t\t\tAspects in set: ", paste(dimnames(cal_corners_trim_est)[[3]][cal_sample_sets[[cal.list$cal.set.num]]], collapse=", "), "\n"))
			}
			cat(paste0("\t\tMean reconstruction RMS Error: ", round(cal.list$mean.reconstruct.rmse, 4), " px\n"))
			cat(paste0("\t\tDLT Coefficient RMS Error:\n"))
			for(i in 1:length(cal.list$coefficient.rmse)){
				cat(paste0("\t\t\t", img_sub_dir[i], paste(rep(' ', img_sub_dir_salign[i]), collapse=''), 
					" : ", paste0(round(cal.list$coefficient.rmse[i], 4), collapse=', '), " px\n"))
			}
		}else{

			cat('\n')
			if(num.sample.sets > 1){
				cat(paste0("\t\tSelected aspect set (lowest reconstruction error): ", cal.list[[paste0('sub', unify_sub, '.cal.set.num')]], "\n"))
				cat(paste0("\t\t\tAspects in set: ", paste(dimnames(cal_corners_trim_est)[[3]][cal_sample_sets[[cal.list[[paste0('sub', unify_sub, '.cal.set.num')]]]]], collapse=", "), "\n"))
			}
			
			cat(paste0("\t\tMean reconstruction RMS Error: ", round(cal.list[[paste0('sub', unify_sub, '.mean.reconstruct.rmse')]], 4), " px\n"))
			cat(paste0("\t\tDLT Coefficient RMS Error:\n"))
			for(i in 1:length(cal.list[[paste0('sub', unify_sub, '.coefficient.rmse')]])){
				cat(paste0("\t\t\t", img_sub_dir[i], paste(rep(' ', img_sub_dir_salign[i]), collapse=''), 
					" : ", paste0(round(cal.list[[paste0('sub', unify_sub, '.coefficient.rmse')]][i], 4), collapse=', '), " px\n"))
			}
		}
	}
	
	list(
		'cal.list'=cal.list,
		'cal_corners_trim_optim'=cal_corners_trim_optim
	)
}