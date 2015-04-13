dltCalibrateCameras <- function(coor.2d, nx, grid.size, c.run = FALSE, reduce.grid.dim = 3, 
	fit.min.break = 1, nlm.iter.max.init = 100, objective.min.init = 10, 
	nlm.eval.max = 350, nlm.iter.max = 250, nlm.calls.max = 100,
	objective.min = 1, grid.incl.min=2, start.param=NULL,
	print.progress = FALSE){
                    
	#nlm.iter.max.init <- NULL
	#objective.min.init <- 0

	# REMOVE ANY SETS WITH NA
	is_na <- apply(is.na(coor.2d), c(3, 4), 'sum')
	is_na_rowsums <- rowSums(is_na)
	coor.2d <- coor.2d[, , is_na_rowsums == 0, ]	

	# FIND SECOND GRID DIMENSION
	ny <- dim(coor.2d)[1]/nx

	# REDUCE GRID DIMENSIONS
	if(reduce.grid.dim){

		# CHECK THAT reduce.grid.dim IS GREATER THAN TWO
		if(reduce.grid.dim < 3) stop(paste0("reduce.grid.dim (", reduce.grid.dim, ") must be greater than 2."))
		
		# SET REDUCED GRID DIMENSIONS
		rx <- reduce.grid.dim
		ry <- reduce.grid.dim
		
		# SET REDUCED GRID SIZES
		sx <- ((nx-1)*grid.size) / (rx-1)
		sy <- ((ny-1)*grid.size) / (ry-1)

		# EMPTY REDUCED GRID DIMENSION ARRAY
		coor_2d_red <- array(NA, dim=c(rx*ry, 2, dim(coor.2d)[3], dim(coor.2d)[4]))

		if(print.progress) cat('\nReduce grid point density (', dim(coor.2d)[3]*dim(coor.2d)[4], ' total)\n', sep='')

		for(i in 1:dim(coor.2d)[3]){
			for(j in 1:dim(coor.2d)[4]){
				if(print.progress) cat('\t', (i-1)*dim(coor.2d)[4] + j, ') Aspect ', i, ', View ', j, '; ', sep='')
				coor_2d_red[, , i, j] <- resampleGridImagePoints(pts=coor.2d[, , i, j], nx=nx, rx=rx, ry=ry, fit.min.break=fit.min.break, print.progress=print.progress)$pts
			}
		}
	}else{
		
		# MAINTAIN FULL GRID DIMENSIONS
		rx <- nx;ry <- ny;sx <- grid.size;sy <- NULL
		
		# COPY POINTS TO REDUCED 2D COOR ARRAY
		coor_2d_red <- coor.2d
	}	

	# SCALE INITIAL TRANSLATE PARAMETER TO REAL-WORLD UNITS (APPROX THIRD THE MAX DIMENSION OF GRID)
	t_init <- (max(nx, ny)*grid.size)/3
	
	# SET INITIAL TIME POINT
	ptm <- proc.time()

	# RUN NLM FUNCTION TO FIND TRANSFORMATION PARAMETERS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
	if(print.progress) cat('\nFull Transform RMSE Minimization\nNumber of parameters:', (dim(coor.2d)[3]-1)*6, '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\n')

	control <- list(eval.max = nlm.eval.max, iter.max = nlm.iter.max)

	# SET STARTING NUMBER OF GRIDS FOR OPTIMIZATION
	#grid.incl.min <- if(dim(coor.2d)[3] == 2){2}else{3}
	#grid.incl.min <- 2
	
	# SET FIXED START PARAMETERS, FIX FIRST GRID AT ORIGIN
	p_fix <- c()
	
	#trace <- 0
	#if(!is.null(sink.trace)) trace <- 1

	for(num_grid in grid.incl.min:dim(coor.2d)[3]){

		if(print.progress) cat('\nRunning minimization with ', num_grid, ' grids...', sep='')

		# ESTIMATE STARTING PARAMETERS BASED ON 2D VIEW
		estimate_start_params <- dltCCEstimateStartParams(coor.2d, num_grid, nx, ny, grid.size, p_fix)
		start_param_array <- estimate_start_params$start_param_array
		max_disp_rwu <- estimate_start_params$max_disp_rwu

		# GET 2D COORDINATE SUBSET MATRIX FOR TRANSFORMATION OPTIMIZATION
		coor_2d_t <- apply(coor_2d_red[, , 1:num_grid, ], c(2, 4), matrix, byrow=FALSE)

		lower <- rep(c(rep(-pi, 3), rep(-7*max(nx, ny)*grid.size, 3)), num_grid)

		# SET NLM CALL VARIABLES
		nlm_min <- rep(NA, nlm.calls.max)
		nlm_calls <- list()

		# SAVE PROCESSING TIME
		nlm_start <- proc.time()

		for(nlm_n in 1:nlm.calls.max){

			# SET STARTING PARAMETERS
			if(is.null(start.param)){

				# VARY EACH ITERATION
				start <- c(t(start_param_array[nlm_n, ((length(p_fix)/6)+1):(num_grid-1), ]))
				if(length(p_fix)) start <- c(p_fix, start)
			}else{
				start <- start.param[1:((num_grid-1)*6)]
				if(length(p_fix)) start[1:length(p_fix)] <- p_fix
				
				if(((num_grid-1)*6) > length(start.param)){
					start <- c(t(start_param_array[nlm_n, ((length(p_fix)/6)+1):(num_grid-1), ]))
					if(length(p_fix)) start <- c(p_fix, start)
				}

				# VARY LAST SIX PARAMETERS AFTER FIRST ITERATION
				if(nlm_n > 1){
					start[(length(start)-6+1):length(start)] <- sample(seq(0.5, 1.5, length=30), size=6)*start[(length(start)-6+1):length(start)]
				}
			}

			if(print.progress) cat(' ', nlm_n, sep='')

			#if(!is.null(sink.trace)) sink(paste0(sink.trace, "/", "num grids ", num_grid, " nlm iter ", nlm_n, ".txt"))

			start_full <- start
			stopped <- FALSE

			if(!is.null(nlm.iter.max.init)){
				nlm_fit <- tryCatch(
					expr={
						nlminb(start=start, objective=dltTransformationParameterRMSError, 
							control=list(iter.max = nlm.iter.max.init, rel.tol=1e-6),
							lower=lower, upper=-lower, coor.2d=coor_2d_t, nx=rx, ny=ry, 
							sx=sx, sy=sy, p.fixed=rep(0, 6))
					},
					error=function(cond){if(print.progress) cat('F');return(NULL)},
					warning=function(cond) return(NULL)
				)

				if(is.null(nlm_fit)) next

				#cat(" nlm_fit$objective: ", nlm_fit$objective, ", ", nlm_fit$message, ", ", sep="")
				
			}else{
				nlm_fit <- list('objective' = 0, 'message' = '')
			}

			#if(!is.null(sink.trace)) sink()

			if(nlm_fit$objective > objective.min.init && nlm_fit$message == "relative convergence (4)"){
				stopped <- TRUE
			}else{

				start_full <- nlm_fit$par
				nlm_fit <- tryCatch(
					expr={
						nlminb(start=start_full, objective=dltTransformationParameterRMSError, 
							control=control, lower=lower, upper=-lower, coor.2d=coor_2d_t, nx=rx, ny=ry, 
							sx=sx, sy=sy, p.fixed=rep(0, 6))
					},
					error=function(cond){if(print.progress) cat('N');return(NULL)},
					warning=function(cond) return(NULL)
				)
			}

			#
			if(!is.null(nlm_fit)){

				# GET MINIMUM
				if(print.progress){
					if(nlm_fit$message == 'function evaluation limit reached without convergence (9)' || nlm_fit$message == 'iteration limit reached without convergence (10)'){
						cat('L')
					}else if(nlm_fit$message == 'false convergence (8)'){
						cat('F')
					}else if(nlm_fit$message == 'relative convergence (4)'){
						cat('R')
					}else{
						cat('C')
					}
					if(stopped) cat('-S')
					cat('(', round(nlm_fit$objective, 2), ')', sep='')
				}
				nlm_calls[[nlm_n]] <- nlm_fit
				nlm_min[nlm_n] <- nlm_calls[[nlm_n]]$objective

				# IF OPTIMIZATION MINIMUM IS LESS THAN ONE, STOP ITERATING
				if(nlm_calls[[nlm_n]]$convergence == 0 && nlm_calls[[nlm_n]]$objective < objective.min) break
				if(nlm_fit$message == 'iteration limit reached without convergence (10)' && nlm_calls[[nlm_n]]$objective < objective.min) break
				if(nlm_fit$message == 'function evaluation limit reached without convergence (9)' && nlm_calls[[nlm_n]]$objective < objective.min) break
				if(nlm_fit$message == 'false convergence (8)' && nlm_calls[[nlm_n]]$objective < objective.min) break
			}
		}

		# SAVE PROCESSING TIME
		nlm_elapsed <- proc.time() - nlm_start

		# SAVE RUN WITH MINIMUM
		nlm_res_t <- nlm_calls[[which.min(nlm_min)]]

		if(print.progress){
			cat('\n\tNumber of nlminb() calls: ', nlm_n, ' (', nlm.calls.max, ' max)', 
				'\n\tTermination message: ', nlm_res_t$message, 
				'\n\tMinimum: ', nlm_res_t$objective, 
				'\n\tIterations: ', nlm_res_t$iterations, ' (', nlm.iter.max, ' max)', 
				'\n\tFunction evaluations: ', nlm_res_t$evaluations['function'], ' (', nlm.eval.max, ' max)', 
				'\n\tRun-time: ', nlm_elapsed[1], ' sec', 
				'\n\tStarting parameters: ', sep='')
				cat(round(start, 5), sep=', ')
				cat('\n\tFinal estimated parameters: ')
				cat(round(nlm_res_t$par, 5), sep=', ')
				cat('\n\tSum of absolute differences between initial and final parameters: ', sum(abs(start - nlm_res_t$par)))
			cat('\n')
		}

		# SET FIXED PARAMETERS FOR NEXT ITERATION
		p_fix <- nlm_res_t$par
	}

	# SAVE PROCESSING TIME
	run_time <- proc.time() - ptm

	if(print.progress) cat('\nTotal processing time: ', run_time[1], ' sec', sep='')

	# SAVE OPTIMIZED PARAMETERS
	p_init <- matrix(c(rep(0, 6), nlm_res_t$par), nrow=6)

	# GET 3D COORDINATES BASED ON OPTIMIZED PARAMETERS
	coor_3d_coeff <- transformPlanarCalibrationCoordinates(tpar=c(p_init), nx=nx, ny=ny, sx=grid.size)

	# GET 2D INPUT COORDINATES
	coor_2d_coeff <- apply(coor.2d, c(2, 4), matrix, byrow=FALSE)

	# GET DLT CALIBRATION COEFFICIENTS FROM OPTIMIZED 3D COORDINATE SUBSET (ALL GRID POINTS - SO RMSE CAN DIFFER FROM NLM MINIMUM)
	dlt_coefficients_t <- dltCoefficients(coor.3d=coor_3d_coeff, coor.2d=coor_2d_coeff)

	if(c.run){

		# GET 2D COORDINATE SUBSET MATRIX FOR COEFFICIENT OPTIMIZATION
		coor_2d_c <- apply(coor_2d_red, c(2, 4), matrix, byrow=FALSE)

		# SET INITIAL TIME POINT AND NUMBER OF ITERATIONS
		ptm <- proc.time();#c_iter <<- 0

		# RUN NLM FUNCTION TO FIND COEFFICIENTS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
		if(print.progress) cat('\nCoefficient RMSE Minimization\nNumber of parameters:', length(c(dlt_coefficients_t$cal.coeff)), '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\nRunning minimization...\n')
		nlm_res_c <- nlm(dltCoefficientRMSError, p=c(dlt_coefficients_t$cal.coeff), coor.2d=coor_2d_c)

		if(print.progress) cat('\nTermination code:', nlm_res_c$code, '\nIterations:', nlm_res_c$iterations, '\n')

		# SAVE PROCESSING TIME
		run_time_c <- proc.time() - ptm

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=matrix(nlm_res_c$estimate, nrow=11, ncol=dim(coor.2d)[4]), coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_coefficients_c <- dltCoefficients(coor.3d=dlt_reconstruct$coor.3d, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_c$cal.coeff
		coefficient_rmse <- dlt_coefficients_c$rmse
	}else{

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=dlt_coefficients_t$cal.coeff, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_t$cal.coeff
		coefficient_rmse <- dlt_coefficients_t$rmse

		#dlt_coefficients_c <- dltCoefficients2(coor.3d=coor_3d, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))
		#hist(dlt_coefficients_c$rmse)
		
		nlm_res_c <- list('estimate'=NA, 'minimum'=NA)
		run_time_c <- NA
	}

	l <- list(
		cal.coeff=cal_coeff, 
		coor.3d=coor_3d, 
		mean.reconstruct.rmse=mean_reconstruct_rmse, 
		coefficient.rmse=coefficient_rmse,
		t.param.final=p_init,
		t.min=nlm_res_t$objective,
		t.runtime=run_time[1],
		c.param.init=c(dlt_coefficients_t$cal.coeff),
		c.param.final=nlm_res_c$estimate,
		c.min=nlm_res_c$minimum,
		c.iter=nlm_res_c$iterations,
		c.runtime=run_time_c[1]
		)
	class(l) <- 'dltCalibrateCameras'
	l
}

summary.dltCalibrateCameras <- function(object, ...){
	r <- ''

	r <- c(r, '\ndltCalibrateCameras Summary\n')

	r <- c(r, '\tMinimize RMS Error by transformation\n')
	r <- c(r, '\t\tTotal number of parameters estimated: ', length(object$t.param.final)-6, '\n')
	r <- c(r, '\t\tFinal Minimum Mean RMS Error: ', round(object$t.min, 3), '\n')
	r <- c(r, '\t\tTotal Run-time: ', round(object$t.runtime, 2), ' sec\n')

	if(!is.null(object$c.iter)){
		r <- c(r, '\tMinimize RMSE by coefficients\n')
		r <- c(r, '\t\tNumber of parameters: ', length(object$c.param.init), '\n')
		r <- c(r, '\t\tSum of absolute differences between initial and final parameters: ', format(sum(abs(object$c.param.init - object$c.param.final))), '\n')
		r <- c(r, '\t\tTotal function calls: ', object$c.iter, '\n')
		r <- c(r, '\t\tFinal Mean RMS Error: ', format(object$c.min), '\n')
		r <- c(r, '\t\tRun-time: ', round(object$c.runtime, 2), ' sec\n')
	}

	r <- c(r, '\n')

	r <- c(r, '\tMean Reconstruct RMSE: ', round(object$mean.reconstruct.rmse, 3), '\n')

	if(length(object$coefficient.rmse) == 1){
		r <- c(r, '\tMean Calibration Coefficient RMS Error: ', round(object$coefficient.rmse, 3), '\n')
	}else{
		r <- c(r, '\tCalibration Coefficient RMS Error:\n')
		for(i in 1:length(object$coefficient.rmse)){
			r <- c(r, '\t\t', i, ': ', round(object$coefficient.rmse[i], 3), '\n')
		}
	}

	r <- c(r, '\t3D Coordinate Matrix dimensions: ', dim(object$coor.3d)[1], ' x ', dim(object$coor.3d)[2], '\n')

	class(r) <- "summary.dltCalibrateCameras"
	r
}

print.summary.dltCalibrateCameras <- function(x, ...) cat(x, sep='')

dltCCEstimateStartParams <- function(coor.2d, num.grid, nx, ny, grid.size, p.fix, use.param.min=2){

	nrow_start_param <- 576
	
	if(!is.null(p.fix) && length(p.fix) >= use.param.min*6){

		# GET 3D COORDINATES BASED ON OPTIMIZED PARAMETERS
		coor_3d_coeff <- transformPlanarCalibrationCoordinates(tpar=c(matrix(c(rep(0, 6), p.fix), nrow=6)), 
			nx=nx, ny=ny, sx=grid.size)

		# GET 2D INPUT COORDINATES
		coor_2d_coeff <- apply(coor.2d[, , 1:(num.grid-1), ], c(2, 4), matrix, byrow=FALSE)

		# GET DLT CALIBRATION COEFFICIENTS FROM OPTIMIZED 3D COORDINATE SUBSET (ALL GRID POINTS - SO RMSE CAN DIFFER FROM NLM MINIMUM)
		dlt_coefficients_t <- dltCoefficients(coor.3d=coor_3d_coeff, coor.2d=coor_2d_coeff)
		
		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=dlt_coefficients_t$cal.coeff, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		# COPY 3D COORDINATES TO ARRAY
		grids_3d <- array(NA, dim=c(nx*ny, 3, num.grid))
		for(i in 1:dim(grids_3d)[3]) grids_3d[, , i] <- dlt_reconstruct$coor.3d[((i-1)*nx*ny+1):(i*nx*ny), ]
		
		# FIND INVERSE TRANSFORMATIONS TO GET 
		t_param <- inverseGridTransform(grids_3d)

		# STARTING PARAMETER ARRAY
		start_param_array <- array(NA, dim=c(nrow_start_param, num.grid-1, 6))
		for(i in 1:dim(start_param_array)[2]) start_param_array[, i, ] <- matrix(t_param[i+1, ], nrow=nrow_start_param, ncol=6, byrow=TRUE)

		# APPLY RANDOM SCALING
		scaling <- array(sample(seq(0.5, 1.5, length=dim(start_param_array)[1]*dim(start_param_array)[2]*3)), dim=c(dim(start_param_array)[1], dim(start_param_array)[2], 3))
		start_param_array[, , 4:6] <- array(start_param_array[, , 4:6], dim=dim(scaling))*scaling

		# APPLY RANDOM SCALING
		scaling <- array(sample(seq(0, 2, length=dim(start_param_array)[1]*dim(start_param_array)[2]*3)), dim=c(dim(start_param_array)[1], dim(start_param_array)[2], 3))
		start_param_array[, , 1:3] <- array(start_param_array[, , 1:3], dim=dim(scaling))*scaling

		max_disp_rwu <- max(t_param[, 4:6])

	}else{

		coor.2d <- coor.2d[, , 1:num.grid, ]

		# GET APPROXIMATE SCALING FROM PIXELS TO REAL-WORLD UNITS
		px2rwu <- 0
		for(i in 1:dim(coor.2d)[4]){
			for(j in 1:dim(coor.2d)[3]) px2rwu <- max(px2rwu, (grid.size*(nx-1)) / sqrt(sum((coor.2d[1, , j, i] - coor.2d[nx, , j, i])^2)))
		}

		# DISTANCE OF CENTROIDS OF ALL GRID ASPECTS FROM FIRST ASPECT CENTROID
		centroid_dist <- matrix(NA, dim(coor.2d)[3]-1, dim(coor.2d)[4])
		for(i in 1:dim(coor.2d)[4]){

			# CENTROID OF FIRST ASPECT
			centroid_first <- colMeans(coor.2d[, , 1, i])
		
			# CENTROID-CENTROID DISTANCE
			for(j in 2:dim(coor.2d)[3]) centroid_dist[j-1, i] <- sqrt(sum((colMeans(coor.2d[, , j, i]) -  centroid_first)^2))
		}

		# FIND MAXIMUM CENTROID DISPLACEMENT IN REAL-WORLD UNITS
		max_disp_rwu <- max(centroid_dist)*px2rwu

		# FIND MAX SIDE DIMENSIONS IN STANDARD SQUARE LENGTH (RELATIVE MEASURE OF DISTANCE FROM CAMERA)
		square_side_max <- matrix(NA, dim(coor.2d)[3], dim(coor.2d)[4])
		for(i in 1:dim(coor.2d)[4]){
			for(j in 1:dim(coor.2d)[3]){
			
				# SIDE LENGTHS
				side_lengths <- c(
					sqrt(sum((coor.2d[1, , j, i] - coor.2d[nx, , j, i])^2)),
					sqrt(sum((coor.2d[nx*ny-nx+1, , j, i] - coor.2d[nx*ny, , j, i])^2)),
					sqrt(sum((coor.2d[nx, , j, i] - coor.2d[nx*ny, , j, i])^2)),
					sqrt(sum((coor.2d[1, , j, i] - coor.2d[nx*ny-nx+1, , j, i])^2)))
				
				# FIND MAX AND SCALE
				if(which.max(side_lengths) %in% c(1, 2)){
					square_side_max[j, i] <- max(side_lengths)
				}else{
					square_side_max[j, i] <- max(side_lengths)*(nx / ny)
				}
			}
		}

		# FIND DIFFERENCE FROM FIRST ASPECT
		square_side_dist <- square_side_max[2:nrow(square_side_max), ] - matrix(square_side_max[1, ], nrow(square_side_max)-1, ncol(square_side_max), byrow=TRUE)

		# SCALE TO MAX OF CENTROID DISTANCES
		square_side_dist <- abs(square_side_dist) * (max(centroid_dist) / max(abs(square_side_dist)))

		# CONVERT TO REAL-WORLD UNITS AND COMBINE INTO ONE MATRIX
		dist_param <- cbind(centroid_dist * px2rwu, square_side_dist * px2rwu)

		# STARTING PARAMETER ARRAY
		start_param_array <- array(NA, dim=c(nrow_start_param, dim(coor.2d)[3]-1, 6))

		# ADD IN TRANSLATION STARTING PARAMETERS
		for(i in 1:dim(start_param_array)[2]){
			start_param_array[, i, 4:6] <- c(dist_param[i, 1:4], 0, dist_param[i, 4:1], dist_param[i, 3:2], dist_param[i, 1], 0, dist_param[i, 4], dist_param[i, 1:2], 0, dist_param[i, 4])
			start_param_array[, i, 4:6] <- start_param_array[, i, 4:6]*c(-1,1,1,-1)
		}

		# ADD IN ROTATION STARTING PARAMETERS
		start_param_array[, , 1:3] <- c(0.0216, 0.0113, 0.0124, 0.02, 0.0348, 0.0323, 0.0295, 0.02143, 0.034)
		start_param_array[, , 1:3] <- start_param_array[, , 1:3]*c(-1,1,1,-1)

		# APPLY RANDOM SCALING
		scaling <- array(sample(seq(0.7, 1.4, length=dim(start_param_array)[1]*dim(start_param_array)[2]*3)), dim=c(dim(start_param_array)[1], dim(start_param_array)[2], 3))
		start_param_array[, , 4:6] <- array(start_param_array[, , 4:6], dim=dim(scaling))*scaling

		# APPLY RANDOM SCALING
		scaling <- array(sample(seq(0.7, 2, length=dim(start_param_array)[1]*dim(start_param_array)[2]*3)), dim=c(dim(start_param_array)[1], dim(start_param_array)[2], 3))
		start_param_array[, , 1:3] <- array(start_param_array[, , 1:3], dim=dim(scaling))*scaling
	}

	#cat('\n')
	#print(start_param_array[, 1, 1:3])

	list(start_param_array = start_param_array, max_disp_rwu = max_disp_rwu)
}